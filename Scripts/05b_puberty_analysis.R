# PDS-dependent analysis script along with visualization of trajectories
# ERF, 2025.11.28 
#

# R/4.4.2-gfbf-2024a

library(tidyverse)
library(ggeffects)
library(purrr)
library(patchwork)
library(gratia)
library(gamm4)
library(broom.mixed)

est_nomeans = read_csv("estimates_covariates_ageanalyses_NO_MEANS.csv") %>% 
  mutate(n_corr = n_trials * mean_acc) %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(v_0b_lure = v_0b_lure * -1,
         v_2b_lure = v_2b_lure * -1,
         v_0b_nonlure = v_0b_nonlure * -1,
         v_2b_nonlure = v_2b_nonlure * -1) %>% 
  mutate(v_0b = rowMeans(select(., starts_with("v_0b"))),
         v_2b = rowMeans(select(., starts_with("v_2b")))) %>% 
  select(sub_id, TP, a_2b, v_2b, z_2b, mean_rt_2b)

est = read_csv("estimates_covariates_pubanalyses.csv") %>%
  mutate(sex = factor(sex)) %>% 
  left_join(., est_nomeans, by = c("sub_id", "TP"))

males = est %>% filter(sex == "M") %>% 
  mutate(age_z = as.numeric(scale(age)),
         pub_tempo_z = as.numeric(scale(pub_tempo)),
         pub_timing_z = as.numeric(scale(pub_timing)))

females = est %>% filter(sex == "F") %>% 
  mutate(age_z = as.numeric(scale(age)),
         pub_tempo_z = as.numeric(scale(pub_tempo)),
         pub_timing_z = as.numeric(scale(pub_timing)))


outcomes = c("mean_acc_2b", "v_2b", "z_2b",
             "mean_rt_2b", "a_2b", "t_mean")



fit_agepub_gam = function(y_var, df, pub_var,n_k) {
  
  fml_age = as.formula(paste0(y_var," ~ s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_main = as.formula(paste0(y_var,"~", pub_var, " + s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_int = as.formula(paste0(y_var,"~", pub_var,"+ s(age_z, bs = 'cs', k=",n_k,")"," + s(age_z, bs = 'cs', by = ", pub_var, ", k = ", n_k, ")"))

  
  fit_age = gamm4(fml_age, data = df, random = ~(1|sub_id), REML = F)
  fit_pub_main = gamm4(fml_pub_main, data = df, random = ~(1|sub_id), REML = F)
  fit_pub_int = gamm4(fml_pub_int, data = df, random = ~(1|sub_id), REML = F)
  
  
  get_checks = function(fit){
    list(gam_summary = summary(fit$gam),
         lmer_summary = summary(fit$mer),
         k_check = k.check(fit$gam),
         concurvity = concurvity(fit$gam))
  }
  
  get_fit = function(fit) {
    tibble(
      AIC = as.numeric(AIC(fit$mer)),
      n = nobs(fit$mer),
      r_sq = summary(fit$gam)$r.sq
    )
  }
  
  
  fits = list(age = fit_age, pub_main = fit_pub_main, pub_int = fit_pub_int)
  
  
  fit_tbl = bind_rows(get_fit(fit_age) %>% mutate(model = "age"),
                      get_fit(fit_pub_main) %>% mutate(model = paste0("pub_main_", pub_var)),
                      get_fit(fit_pub_int) %>% mutate(model = paste0("pub_int_", pub_var))) %>% 
    mutate(outcome = y_var,
           pub_var = pub_var,
           delta_AIC = AIC - min(AIC)) %>% 
    arrange(delta_AIC)
  
  
  # Model comparisons
  cmp_age_vs_main = anova(fit_age$mer, fit_pub_main$mer)
  cmp_main_vs_int = anova(fit_pub_main$mer, fit_pub_int$mer)
  cmp_age_vs_int = anova(fit_age$mer, fit_pub_int$mer)
  
  
  # Identify the winner
  alpha = 0.05
  p_age_vs_main = cmp_age_vs_main$`Pr(>Chisq)`[2]
  p_age_vs_int  = cmp_age_vs_int$`Pr(>Chisq)`[2]
  p_main_vs_int = cmp_main_vs_int$`Pr(>Chisq)`[2]
  
  sig_age_vs_main = !is.na(p_age_vs_main) && (p_age_vs_main < alpha)
  sig_age_vs_int  = !is.na(p_age_vs_int)  && (p_age_vs_int  < alpha)
  sig_main_vs_int = !is.na(p_main_vs_int) && (p_main_vs_int < alpha)
  
  winner =
    if (!sig_age_vs_main && !sig_age_vs_int) {
      "age"
    } else if (sig_age_vs_main && !sig_age_vs_int) {
      paste0("pub_main_", pub_var)
    } else if (!sig_age_vs_main && sig_age_vs_int) {
      paste0("pub_int_", pub_var)
    } else {
      if (sig_main_vs_int) paste0("pub_int_", pub_var) else paste0("pub_main_", pub_var)
    }
  
  cmp_tbl = bind_rows(
    tidy(cmp_age_vs_main) %>% mutate(comparison = "age_vs_pub_main"),
    tidy(cmp_age_vs_int) %>% mutate(comparison = "age_vs_pub_int"),
    tidy(cmp_main_vs_int) %>% mutate(comparison = "pub_main_vs_pub_int")
  ) %>%
    mutate(
      outcome = y_var,
      pub_var = pub_var,
      winner = winner
    ) 
  
    
  # Return
  list(
    fits = fits,
    checks = lapply(fits, get_checks),
    fit_tbl = fit_tbl,
    comparisons = cmp_tbl,
    cmp_age_vs_main = cmp_age_vs_main,
    cmp_main_vs_int = cmp_main_vs_int,
    cmp_age_vs_int = cmp_age_vs_int)
}




####
###
### Males - Pubertal timing
###
####
results_males_timing = map(
  outcomes,
  ~ fit_agepub_gam(
    y_var = .x,
    df = males,
    pub_var = "pub_timing_z",
    n_k = 7
  )
)
names(results_males_timing) = outcomes

fit_table = map_dfr(results_males_timing, "fit_tbl") %>% 
  select(outcome, pub_var, model, AIC, n, r_sq, delta_AIC)

aic_wide = fit_table %>% 
  select(outcome, pub_var, model, AIC) %>%
  mutate(
    model_type = case_when(
      model == "age" ~ "age",
      grepl("^pub_main_", model) ~ "pub_main",
      grepl("^pub_int_",  model) ~ "pub_int",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_type)) %>%
  group_by(outcome, pub_var, model_type) %>%
  summarise(AIC = first(AIC), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = model_type, values_from = AIC)


cmp_m_timing = map_dfr(results_males_timing, "comparisons")
cmp_m_timing_tests = cmp_m_timing %>%
  filter(!is.na(p.value)) %>% 
  left_join(aic_wide, by = c("outcome", "pub_var")) %>%
  mutate(
    AIC_delta = case_when(
      comparison == "age_vs_pub_main"      ~ age - pub_main,
      comparison == "age_vs_pub_int"       ~ age - pub_int,
      comparison == "pub_main_vs_pub_int"  ~ pub_main - pub_int,
      TRUE ~ NA_real_
    )
  ) %>%
  select(outcome, pub_var, comparison, AIC_delta, term, statistic, df, p.value, winner) 


####
###
### Males - Pubertal tempo
###
####
results_males_tempo = map(
  outcomes,
  ~ fit_agepub_gam(
    y_var = .x,
    df = males,
    pub_var = "pub_tempo_z",
    n_k = 7
  )
)
names(results_males_tempo) = outcomes

fit_table = map_dfr(results_males_tempo, "fit_tbl") %>% 
  select(outcome, pub_var, model, AIC, n, r_sq, delta_AIC)

aic_wide = fit_table %>% 
  select(outcome, pub_var, model, AIC) %>%
  mutate(
    model_type = case_when(
      model == "age" ~ "age",
      grepl("^pub_main_", model) ~ "pub_main",
      grepl("^pub_int_",  model) ~ "pub_int",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_type)) %>%
  group_by(outcome, pub_var, model_type) %>%
  summarise(AIC = first(AIC), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = model_type, values_from = AIC)


cmp_m_tempo = map_dfr(results_males_tempo, "comparisons")
cmp_m_tempo_tests = cmp_m_tempo %>%
  filter(!is.na(p.value)) %>% 
  left_join(aic_wide, by = c("outcome", "pub_var")) %>%
  mutate(
    AIC_delta = case_when(
      comparison == "age_vs_pub_main"      ~ age - pub_main,
      comparison == "age_vs_pub_int"       ~ age - pub_int,
      comparison == "pub_main_vs_pub_int"  ~ pub_main - pub_int,
      TRUE ~ NA_real_
    )
  ) %>%
  select(outcome, pub_var, comparison, AIC_delta, term, statistic, df, p.value, winner) 



####
###
### Females - Pubertal timing
###
####
results_females_timing = map(
  outcomes,
  ~ fit_agepub_gam(
    y_var = .x,
    df = females,
    pub_var = "pub_timing_z",
    n_k = 7
  )
)
names(results_females_timing) = outcomes

fit_table = map_dfr(results_females_timing, "fit_tbl") %>% 
  select(outcome, pub_var, model, AIC, n, r_sq, delta_AIC)

aic_wide = fit_table %>% 
  select(outcome, pub_var, model, AIC) %>%
  mutate(
    model_type = case_when(
      model == "age" ~ "age",
      grepl("^pub_main_", model) ~ "pub_main",
      grepl("^pub_int_",  model) ~ "pub_int",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_type)) %>%
  group_by(outcome, pub_var, model_type) %>%
  summarise(AIC = first(AIC), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = model_type, values_from = AIC)


cmp_f_timing = map_dfr(results_females_timing, "comparisons")
cmp_f_timing_tests = cmp_f_timing %>%
  filter(!is.na(p.value)) %>% 
  left_join(aic_wide, by = c("outcome", "pub_var")) %>%
  mutate(
    AIC_delta = case_when(
      comparison == "age_vs_pub_main"      ~ age - pub_main,
      comparison == "age_vs_pub_int"       ~ age - pub_int,
      comparison == "pub_main_vs_pub_int"  ~ pub_main - pub_int,
      TRUE ~ NA_real_
    )
  ) %>%
  select(outcome, pub_var, comparison, AIC_delta, term, statistic, df, p.value, winner) 


####
###
### Females - Pubertal tempo
###
####
results_females_tempo = map(
  outcomes,
  ~ fit_agepub_gam(
    y_var = .x,
    df = females,
    pub_var = "pub_tempo_z",
    n_k = 7
  )
)
names(results_females_tempo) = outcomes

fit_table = map_dfr(results_females_tempo, "fit_tbl") %>% 
  select(outcome, pub_var, model, AIC, n, r_sq, delta_AIC)

aic_wide = fit_table %>% 
  select(outcome, pub_var, model, AIC) %>%
  mutate(
    model_type = case_when(
      model == "age" ~ "age",
      grepl("^pub_main_", model) ~ "pub_main",
      grepl("^pub_int_",  model) ~ "pub_int",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(model_type)) %>%
  group_by(outcome, pub_var, model_type) %>%
  summarise(AIC = first(AIC), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = model_type, values_from = AIC)


cmp_f_tempo = map_dfr(results_females_tempo, "comparisons")
cmp_f_tempo_tests = cmp_f_tempo %>%
  filter(!is.na(p.value)) %>% 
  left_join(aic_wide, by = c("outcome", "pub_var")) %>%
  mutate(
    AIC_delta = case_when(
      comparison == "age_vs_pub_main"      ~ age - pub_main,
      comparison == "age_vs_pub_int"       ~ age - pub_int,
      comparison == "pub_main_vs_pub_int"  ~ pub_main - pub_int,
      TRUE ~ NA_real_
    )
  ) %>%
  select(outcome, pub_var, comparison, AIC_delta, term, statistic, df, p.value, winner) 



####
###
### Visualizing the winning models
###
###
####

make_pub_groups = function(df, pub_var, seed = 1104) {
  set.seed(seed)
  km = kmeans(df[[pub_var]], centers = 3)
  
  centers = as.numeric(km$centers)
  ord = order(centers)  # low -> high
  
  # map raw cluster ids to ordered labels
  cl = match(km$cluster, ord)
  
  labels = if (pub_var == "pub_timing_z") c("earlier/", "average", "later/") else
    if (pub_var == "pub_tempo_z")  c("slower",  "average", "faster")
  
  df %>%
    dplyr::mutate(
      pub_group = factor(labels[cl], levels = labels)
    )
}


predict_by_group = function(fit, df, y_var, pub_var, n_age = 200, seed = 1104) {
  
  # add groups
  df_g = make_pub_groups(df, pub_var, seed = seed)
  
  # raw age sequence for plotting
  age_seq = seq(min(df_g$age, na.rm = TRUE),
                max(df_g$age, na.rm = TRUE),
                length.out = n_age)
  
  # convert raw age to age_z using the df's mean and sd (need raw for plotting and age_z for predicting)
  age_mu = mean(df_g$age, na.rm = TRUE)
  age_sd = sd(df_g$age, na.rm = TRUE)
  age_z_seq = (age_seq - age_mu) / age_sd
  
  # group-specific pub values using group means
  pub_vals =
    df_g %>%
    group_by(pub_group) %>%
    summarise(pub_value = mean(.data[[pub_var]], na.rm = TRUE), .groups = "drop")
  
  newdat = expand_grid(
    age = age_seq,
    pub_group = pub_vals$pub_group
  ) %>%
    left_join(pub_vals, by = "pub_group") %>%
    mutate(
      age_z = (age - age_mu) / age_sd,
      !!pub_var := pub_value
    ) %>%
    select(age, age_z, all_of(pub_var), pub_group)
  
  pr = predict(fit$gam, newdata = newdat, se.fit = TRUE, type = "response")
  
  newdat %>%
    dplyr::mutate(
      outcome = y_var,
      fit = as.numeric(pr$fit),
      se = as.numeric(pr$se.fit),
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
}


plot_pub_curves = function(pred_df, title = NULL, outcome_labs = NULL) {
  
  y_var = unique(pred_df$outcome)
  
  y_lab =
    if (!is.null(outcome_labs) && y_var %in% names(outcome_labs))
      outcome_labs[[y_var]]
  else y_var
  
  cols = c(
    `earlier/` = "#00A5CF",
    slower  = "#00A5CF",
    average = "#3CA437",
    `later/`   = "#17377A",
    faster  = "#17377A"
  )
  
  ggplot(pred_df, aes(x = age, y = fit, colour = pub_group, fill = pub_group)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
    geom_line(linewidth = 1.3) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Age",
      y = y_lab,
      title = title,
      colour = NULL,
      fill = NULL
    ) +
    theme(
      plot.title = element_text(size = 13),
      axis.title = element_text(size = 13),
      axis.text  = element_text(size = 11)
    )
}


model_title = function(pub_var, comparison) {
  
  pub_label = case_when(
      pub_var == "pub_tempo_z"  ~ "Tempo",
      pub_var == "pub_timing_z" ~ "Timing",
      TRUE ~ pub_var
    )
  age_part = case_when(
      comparison == "age_vs_pub_main"     ~ "+ Age",
      comparison == "pub_main_vs_pub_int" ~ "x Age",
      TRUE ~ comparison
    )
  
  paste(pub_label, age_part)
}

make_sex_figure = function(
    sex_label,
    tests_timing,
    tests_tempo,
    results_timing,
    results_tempo,
    df,
    outcome_labs,
    ncol = 3
) {
  
  decide_tbl =
    bind_rows(tests_timing, tests_tempo) %>%
    distinct(outcome, pub_var, winner) %>%
    filter(grepl("^pub_", winner))   # only puberty winners
  
  # if (nrow(decide_tbl) == 0) {
  #   return(
  #     ggplot() +
  #       theme_void() +
  #       labs(title = paste0(sex_label, ": no pubertal winners to plot"))
  #   )
  # }
  
  plots = pmap(decide_tbl,
      function(outcome, pub_var, winner) {
        
        res =
          if (pub_var == "pub_timing_z") results_timing[[outcome]]
        else results_tempo[[outcome]]
        
        fit_use =
          if (grepl("^pub_int_", winner)) res$fits$pub_int
        else res$fits$pub_main
        
        title =
          case_when(
            pub_var == "pub_tempo_z"  & grepl("^pub_int_", winner)  ~ "Tempo × Age",
            pub_var == "pub_tempo_z"  & grepl("^pub_main_", winner) ~ "Tempo + Age",
            pub_var == "pub_timing_z" & grepl("^pub_int_", winner)  ~ "Timing × Age",
            pub_var == "pub_timing_z" & grepl("^pub_main_", winner) ~ "Timing + Age",
            TRUE ~ paste(pub_var, winner)
          )
        
        pred =
          predict_by_group(
            fit = fit_use,
            df = df,
            y_var = outcome,
            pub_var = pub_var
          )
        
        plot_pub_curves(
          pred_df = pred,
          title = title,
          outcome_labs = outcome_labs
        )
      }
    )
  
  n_panels = length(plots)
  nrow = ceiling(n_panels / ncol)
  
  wrap_plots(plots, ncol = ncol, nrow = nrow) +
    plot_annotation(
      title = sex_label,
      theme = theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 18)
      )
    ) +
    plot_layout(guides = "collect")
}



outcome_labs = c(
  mean_acc_2b = "Accuracy",
  mean_rt_2b = "Response Time",
  v_2b = "Drift Rate",
  a_2b = "Decision \nThreshold",
  t_mean = "Non-Decision \nTime",
  z_2b = "Bias"
)


fig_males =
  make_sex_figure(
    sex_label = "Males",
    tests_timing = cmp_m_timing_tests,
    tests_tempo  = cmp_m_tempo_tests,
    results_timing = results_males_timing,
    results_tempo  = results_males_tempo,
    df = males,
    outcome_labs = outcome_labs
  )

fig_females =
  make_sex_figure(
    sex_label = "Females",
    tests_timing = cmp_f_timing_tests,
    tests_tempo  = cmp_f_tempo_tests,
    results_timing = results_females_timing,
    results_tempo  = results_females_tempo,
    df = females,
    outcome_labs = outcome_labs
  )



ggsave("puberty_ddm_models_males.png", fig_males, width = 8, height = 9)
ggsave("puberty_ddm_models_females.png", fig_females, width = 8, height = 9)


## Get overview over summary outputs

extract_winner_gam_summary = function(gam_obj, outcome, pub_var, model_type) {
  s = summary(gam_obj)
  
  # ---- parametric puberty effect ----
  ptab = as.data.frame(s$p.table)
  
  # find the puberty row 
  pub_row_name = rownames(ptab)[rownames(ptab) == pub_var]
  if (length(pub_row_name) == 0) pub_row_name = NA_character_
  
  main_est = if (!is.na(pub_row_name)) ptab[pub_row_name, "Estimate"] else NA_real_
  main_se  = if (!is.na(pub_row_name)) ptab[pub_row_name, "Std. Error"] else NA_real_
  main_t   = if (!is.na(pub_row_name)) ptab[pub_row_name, "t value"] else NA_real_
  main_p   = if (!is.na(pub_row_name)) ptab[pub_row_name, "Pr(>|t|)"] else NA_real_
  
  # ---- smooth terms (age + maybe interaction) ----
  stab = as.data.frame(s$s.table)
  
  age_term = "s(age_z)"
  age_edf = if (age_term %in% rownames(stab)) stab[age_term, "edf"] else NA_real_
  age_F   = if (age_term %in% rownames(stab)) stab[age_term, "F"] else NA_real_
  age_p   = if (age_term %in% rownames(stab)) stab[age_term, "p-value"] else NA_real_
  
  # interaction smooth name 
  int_term = paste0("s(age_z):", pub_var)
  int_edf = if (int_term %in% rownames(stab)) stab[int_term, "edf"] else NA_real_
  int_F   = if (int_term %in% rownames(stab)) stab[int_term, "F"] else NA_real_
  int_p   = if (int_term %in% rownames(stab)) stab[int_term, "p-value"] else NA_real_
  
  tibble(
    outcome = outcome,
    pub_var = pub_var,
    model_type = model_type,  # "pub_main"/"pub_int"
    main_estimate = main_est,
    main_se = main_se,
    main_t = main_t,
    main_p = main_p,
    age_edf = age_edf,
    age_F = age_F,
    age_p = age_p,
    int_edf = if (model_type == "pub_int") int_edf else NA_real_,
    int_F   = if (model_type == "pub_int") int_F   else NA_real_,
    int_p   = if (model_type == "pub_int") int_p   else NA_real_
  )
}

build_sex_summaries = function(cmp_timing, cmp_tempo,
                               results_timing, results_tempo) {
  
  winners = bind_rows(cmp_timing, cmp_tempo) %>%
    distinct(outcome, pub_var, winner) %>%
    filter(str_detect(winner, "^pub_")) %>%  # drop age winners
    mutate(
      model_type = case_when(
        str_detect(winner, "^pub_main_") ~ "pub_main",
        str_detect(winner, "^pub_int_")  ~ "pub_int",
        TRUE ~ NA_character_
      ),
      puberty_domain = case_when(
        str_detect(pub_var, "timing") ~ "timing",
        str_detect(pub_var, "tempo")  ~ "tempo",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(model_type), !is.na(puberty_domain))
  
  # extract summaries
  map_dfr(seq_len(nrow(winners)), function(i) {
    outcome = winners$outcome[i]
    pub_var = winners$pub_var[i]
    model_type = winners$model_type[i]
    domain = winners$puberty_domain[i]
    
    res_list = if (domain == "timing") results_timing else results_tempo
    
    # grab the GAM object depending on winner type
    gam_obj =
      if (model_type == "pub_main") res_list[[outcome]]$fits$pub_main$gam else
        res_list[[outcome]]$fits$pub_int$gam
    
    extract_winner_gam_summary(gam_obj, outcome, pub_var, model_type)
  })
}

# build the two dataframes
females_summaries =
  build_sex_summaries(
    cmp_timing = cmp_f_timing_tests,
    cmp_tempo  = cmp_f_tempo_tests,
    results_timing = results_females_timing,
    results_tempo  = results_females_tempo
  )


females_summaries = females_summaries %>%
  mutate(
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 18), 
    int_p_fdr = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 18)
        x}},
    main_sig = ifelse(main_p_fdr < 0.05, TRUE, FALSE), 
    int_sig = ifelse(int_p_fdr < 0.05, TRUE, FALSE))


males_summaries =
  build_sex_summaries(
    cmp_timing = cmp_m_timing_tests,
    cmp_tempo  = cmp_m_tempo_tests,
    results_timing = results_males_timing,
    results_tempo  = results_males_tempo
  )

males_summaries = males_summaries %>%
  mutate(
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 18), 
    int_p_fdr = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 18)
        x}},
    main_sig = ifelse(main_p_fdr < 0.05, TRUE, FALSE), 
    int_sig = ifelse(int_p_fdr < 0.05, TRUE, FALSE))



#### Save the data frames

cmp_m_tempo_tests$sex = "M"
cmp_m_timing_tests$sex = "M"
cmp_f_tempo_tests$sex = "F"
cmp_f_timing_tests$sex = "F"
cmp_tests = bind_rows(cmp_m_tempo_tests,
                      cmp_m_timing_tests,
                      cmp_f_tempo_tests,
                      cmp_f_timing_tests)
cmp_tests = cmp_tests %>% select(sex, everything())


write_csv(cmp_tests, "model_comparisons_tests_puberty.csv")

# table
cmp_tests = cmp_tests %>% 
  mutate(winner = case_when(winner == "pub_main_pub_tempo_z" ~ "Tempo Main",
                            winner == "pub_main_pub_timing_z" ~ "Timing Main",
                            winner == "age" ~ "Age",
                            winner == "pub_int_pub_tempo_z" ~ "Tempo Interaction",
                            winner == "pub_int_pub_timing_z" ~ "Timing Interaction"),
         pub_var = case_when(pub_var == "pub_tempo_z" ~ "Tempo",
                             pub_var == "pub_timing_z" ~ "Timing"),
         outcome = case_when(outcome == "mean_acc_2b" ~ "Accuracy",
                             outcome == "v_2b" ~ "Drift rate",
                             outcome == "z_2b" ~ "Bias",
                             outcome == "mean_rt_2b" ~ "RT",
                             outcome == "a_2b" ~ "Threshold",
                             outcome == "t_mean" ~ "NDT"),
         comparison = case_when(comparison == "age_vs_pub_main" ~ "Age vs. Pub. Main",
                                comparison == "age_vs_pub_int" ~ "Age vs. Pub. Int.",
                                comparison == "pub_main_vs_pub_int" ~ "Pub. Main vs. Pub. Int."),
         term = case_when(term == "fit_pub_main$mer" ~ "Pub. Main",
                          term == "fit_pub_int$mer" ~ "Pub. Int."))


fittable = flextable(cmp_tests) %>% 
  colformat_double(j = c("AIC_delta", "statistic"), digits = 2) %>% 
  colformat_double(j = c("p.value"), digits = 3) %>% 
  set_header_labels(
    sex = "Sex",
    outcome = "Outcome",
    pub_var = "Pub var",
    comparison = "Comparison",
    AIC_delta = "AIC delta",
    term = "Term",
    statistic = "Statistic",
    df = "DF",
    `p.value` = "p",
    winner = "Winning model"
  ) 

save_as_html(fittable, path = "puberty_models_modelfits.html")


males_summaries = males_summaries %>%
  mutate(sex = "M") %>%
  select(sex, everything())

females_summaries = females_summaries %>% 
  mutate(sex = "F") %>%
  select(sex, everything())

model_effs_summs = bind_rows(males_summaries, females_summaries)

write_csv(model_effs_summs, "model_effect_summaries_puberty.csv")


# table

testtable = flextable(model_effs_summs) 
testtable = colformat_double(testtable, digits = 4)
save_as_html(testtable, path= "puberty_models_maineffects.html")
