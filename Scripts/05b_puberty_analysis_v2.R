# PDS-dependent analysis script along with visualization of trajectories
# ERF, 2026.08.20
# Adapted to use gamm() from mgcv instead of gamm4()
#
# Also, sensitivity analyses are here, from line 534 onwards
#



# R/4.4.2-gfbf-2024a

library(tidyverse)
library(patchwork)
library(mgcv)
library(flextable)


# Import data 
est_nomeans = read_csv("estimates_covariates_ageanalyses_rev1.csv") %>%
  mutate(n_corr = n_trials * mean_acc) %>%
  mutate(v_0b_lure = v_0b_lure * -1,
         v_2b_lure = v_2b_lure * -1,
         v_0b_nonlure = v_0b_nonlure * -1,
         v_2b_nonlure = v_2b_nonlure * -1) %>%
  mutate(v_0b = rowMeans(select(., starts_with("v_0b"))),
         v_2b = rowMeans(select(., starts_with("v_2b")))) %>% 
  rename(t_mean = "t") %>% 
  select(sub_id, TP, a_2b, v_2b, z_2b, mean_rt_2b,
         mean_acc_2b, t_mean)

est = read_csv("estimates_covariates_pubanalyses_rev1.csv") %>% 
  mutate(sex = factor(sex)) %>% 
  select(sub_id, TP, sex, age, pub_tempo, pub_timing) %>% 
  left_join(., est_nomeans, by = c("sub_id", "TP"))

z = function(x) as.numeric(scale(x))

males = est %>% filter(sex == "M") %>%
  mutate(age_z = z(age), pub_tempo_z = z(pub_tempo), pub_timing_z = z(pub_timing))

females = est %>% filter(sex == "F") %>%
  mutate(age_z = z(age), pub_tempo_z = z(pub_tempo), pub_timing_z = z(pub_timing))

outcomes = c("mean_acc_2b", "v_2b", "z_2b",
             "mean_rt_2b", "a_2b", "t_mean")


## Get model comparison stats
get_lrt_stats = function(cmp) {
  tibble(
    chisq = cmp$`L.Ratio`[2],
    df    = cmp$df[2] - cmp$df[1],
    p     = cmp$`p-value`[2]
  )
}


## Fit the three gamm-models and run model comparison each way 
fit_agepub_gam = function(y_var, df, pub_var, n_k) {
  
  fml_age      = as.formula(paste0(y_var, " ~ s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_main = as.formula(paste0(y_var, " ~ ", pub_var,
                                   " + s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_int  = as.formula(paste0(y_var, " ~ ", pub_var,
                                   " + s(age_z, bs = 'cs', k=", n_k, ")",
                                   " + s(age_z, bs = 'cs', by = ", pub_var,
                                   ", k=", n_k, ")"))
  
  fit_age      = gamm(fml_age,      data = df, random = list(sub_id = ~1), method = "ML")
  fit_pub_main = gamm(fml_pub_main, data = df, random = list(sub_id = ~1), method = "ML")
  fit_pub_int  = gamm(fml_pub_int,  data = df, random = list(sub_id = ~1), method = "ML")
  
  fits = list(age = fit_age, pub_main = fit_pub_main, pub_int = fit_pub_int)
  
  aics = c(age      = as.numeric(AIC(fit_age$lme)),
           pub_main = as.numeric(AIC(fit_pub_main$lme)),
           pub_int  = as.numeric(AIC(fit_pub_int$lme)))
  
  cmp_age_vs_main = anova(fit_age$lme,      fit_pub_main$lme)
  cmp_main_vs_int = anova(fit_pub_main$lme, fit_pub_int$lme)
  cmp_age_vs_int  = anova(fit_age$lme,      fit_pub_int$lme)
  
  lrt_age_vs_main = get_lrt_stats(cmp_age_vs_main)
  lrt_main_vs_int = get_lrt_stats(cmp_main_vs_int)
  lrt_age_vs_int  = get_lrt_stats(cmp_age_vs_int)
  
  alpha = 0.05
  sig_age_vs_main = !is.na(lrt_age_vs_main$p) && lrt_age_vs_main$p < alpha
  sig_main_vs_int = !is.na(lrt_main_vs_int$p) && lrt_main_vs_int$p < alpha
  sig_age_vs_int  = !is.na(lrt_age_vs_int$p)  && lrt_age_vs_int$p  < alpha
  
  winner =
    if (!sig_age_vs_main && !sig_age_vs_int) {
      "age"
    } else if (sig_age_vs_main && !sig_age_vs_int) {
      "pub_main"
    } else if (!sig_age_vs_main && sig_age_vs_int) {
      "pub_int"
    } else {
      if (sig_main_vs_int) "pub_int" else "pub_main"
    }
  
  winner_row = tibble(
    outcome           = y_var,
    pub_var           = pub_var,
    winner            = winner,
    
    AIC_age           = aics[["age"]],
    AIC_pub_main      = aics[["pub_main"]],
    AIC_pub_int       = aics[["pub_int"]],
    AIC_winner        = aics[[winner]],
    dAIC_vs_age       = aics[[winner]] - aics[["age"]],
    
    chisq_age_vs_main = lrt_age_vs_main$chisq, df_age_vs_main = lrt_age_vs_main$df, p_age_vs_main = lrt_age_vs_main$p,
    chisq_main_vs_int = lrt_main_vs_int$chisq, df_main_vs_int = lrt_main_vs_int$df, p_main_vs_int = lrt_main_vs_int$p,
    chisq_age_vs_int  = lrt_age_vs_int$chisq,  df_age_vs_int  = lrt_age_vs_int$df,  p_age_vs_int  = lrt_age_vs_int$p
  )
  
  list(fits = fits, winner = winner, winner_row = winner_row)
}


run_block = function(df, pub_var, sex_label) {
  res = map(outcomes,
            ~ fit_agepub_gam(y_var = .x, df = df, pub_var = pub_var, n_k = 7))
  names(res) = outcomes
  list(
    results = res,
    winners = map_dfr(res, "winner_row") %>% mutate(sex = sex_label, .before = 1)
  )
}

m_timing = run_block(males,   "pub_timing_z", "M")
m_tempo  = run_block(males,   "pub_tempo_z",  "M")
f_timing = run_block(females, "pub_timing_z", "F")
f_tempo  = run_block(females, "pub_tempo_z",  "F")

results_males_timing   = m_timing$results
results_males_tempo    = m_tempo$results
results_females_timing = f_timing$results
results_females_tempo  = f_tempo$results



winners_tbl = bind_rows(
  m_timing$winners,
  m_tempo$winners,
  f_timing$winners,
  f_tempo$winners
) %>%
  mutate(
    winner_lab = case_when(
      winner == "age"                                 ~ "Age",
      winner == "pub_main" & pub_var == "pub_timing_z" ~ "Timing Main",
      winner == "pub_main" & pub_var == "pub_tempo_z"  ~ "Tempo Main",
      winner == "pub_int"  & pub_var == "pub_timing_z" ~ "Timing Interaction",
      winner == "pub_int"  & pub_var == "pub_tempo_z"  ~ "Tempo Interaction"
    ),
    outcome_lab = recode(outcome,
                            mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                            mean_rt_2b  = "RT",       a_2b = "Threshold",  t_mean = "NDT"),
    pub_var_lab = recode(pub_var,
                            pub_timing_z = "Timing", pub_tempo_z = "Tempo")
  ) %>%
  select(sex, outcome = outcome_lab, pub_var = pub_var_lab,
         winner = winner_lab,
         AIC_age, AIC_pub_main, AIC_pub_int, dAIC_vs_age,
         lrt_age_vs_main = chisq_age_vs_main,
         lrt_main_vs_int = chisq_main_vs_int,
         lrt_age_vs_int  = chisq_age_vs_int,
         p_age_vs_main, p_main_vs_int, p_age_vs_int)


# WRITE WINNERS TABLE
write_csv(winners_tbl, "puberty_winning_models_mgcv_rev1.csv")

# prettier table:)
winners_ft = flextable(winners_tbl) %>%
  colformat_double(j = c("AIC_age", "AIC_pub_main", "AIC_pub_int",
                         "dAIC_vs_age", "lrt_age_vs_main", 
                         "lrt_main_vs_int", "lrt_age_vs_int"), digits = 2) %>%
  colformat_double(j = c("p_age_vs_main", "p_main_vs_int", "p_age_vs_int"),
                   digits = 3) %>%
  set_header_labels(
    sex = "Sex", outcome = "Outcome", pub_var = "Pub var",
    winner = "Winning model",
    AIC_age = "AIC age", AIC_pub_main = "AIC main", AIC_pub_int = "AIC int",
    dAIC_vs_age = "ΔAIC vs age",
    lrt_age_vs_main = "lrtAM", lrt_main_vs_int = "lrtMI", lrt_age_vs_int = "lrtAI",
    p_age_vs_main = "pAM",
    p_main_vs_int = "pMI",
    p_age_vs_int  = "pAI"
  )
save_as_html(winners_ft, path = "puberty_winning_models_mgcv_rev1.html")


# GET EFFECTS 
winners_raw = bind_rows(m_timing$winners, m_tempo$winners,
                        f_timing$winners, f_tempo$winners) %>%
  rename(outcome_raw = outcome, pub_var_raw = pub_var) 

extract_effects = function(fit_gam, outcome, pub_var, winner) {
  s    = summary(fit_gam)
  ptab = as.data.frame(s$p.table)
  stab = as.data.frame(s$s.table)
  
  # parametric pub effect
  pull_p = function(col) if (pub_var %in% rownames(ptab)) ptab[pub_var, col] else NA_real_
  
  # age smooth
  age_term = "s(age_z)"
  pull_s = function(tab, term, col)
    if (term %in% rownames(tab)) tab[term, col] else NA_real_
  
  # interaction smooth (only present in pub_int)
  int_term = paste0("s(age_z):", pub_var)
  
  tibble(
    outcome   = outcome,
    pub_var   = pub_var,
    winner    = winner,
    main_est  = pull_p("Estimate"),
    main_se   = pull_p("Std. Error"),
    main_t    = pull_p("t value"),
    main_p    = pull_p("Pr(>|t|)"),
    age_edf   = pull_s(stab, age_term, "edf"),
    age_F     = pull_s(stab, age_term, "F"),
    age_p     = pull_s(stab, age_term, "p-value"),
    int_edf   = if (winner == "pub_int") pull_s(stab, int_term, "edf")     else NA_real_,
    int_F     = if (winner == "pub_int") pull_s(stab, int_term, "F")       else NA_real_,
    int_p     = if (winner == "pub_int") pull_s(stab, int_term, "p-value") else NA_real_
  )
}


# walk through pubertal winners across both sexes, both pub_vars
build_effects_table = function(winners_raw_sex,
                               results_timing, results_tempo) {
  pub_only = winners_raw_sex %>%
    filter(winner %in% c("pub_main", "pub_int"))
  
  pmap_dfr(pub_only %>% select(outcome_raw, pub_var_raw, winner),
           function(outcome_raw, pub_var_raw, winner) {
             res_list = if (pub_var_raw == "pub_timing_z") results_timing else results_tempo
             fit_gam  = res_list[[outcome_raw]]$fits[[winner]]$gam
             extract_effects(fit_gam, outcome_raw, pub_var_raw, winner)
           })
}



effects_males = build_effects_table(
  winners_raw %>% filter(sex == "M"),
  results_males_timing, results_males_tempo) %>% 
  mutate(sex = "M", .before = 1) %>% 
  mutate(covariates = "No") %>% 
  mutate(
    # FDR family = all puberty-effect tests run (not just winners):
    # 6 outcomes x 2 pub_vars x 2 sexes = 24 tests per effect type
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 24),
    int_p_fdr  = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 24)
        x
      }
    },
    # better labels
    outcome_pretty = recode(outcome,
                            mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                            mean_rt_2b = "RT", a_2b = "Threshold", t_mean = "NDT"),
    pub_var_pretty = recode(pub_var,
                            pub_timing_z = "Timing", pub_tempo_z = "Tempo"),
    model_type = if_else(winner == "pub_int", "Interaction", "Main eff.")
  ) %>%
  select(sex,
         covariates,
         outcome    = outcome_pretty,
         pub_var    = pub_var_pretty,
         model_type,
         estimate   = main_est,
         main_se,
         main_t,
         main_p,
         main_p_fdr,
         age_edf,
         int_edf,
         int_F,
         int_p,
         int_p_fdr)

effects_females = build_effects_table(
  winners_raw %>% filter(sex == "F"),
  results_females_timing, results_females_tempo) %>% 
  mutate(sex = "F", .before = 1) %>% 
  mutate(covariates = "No") %>% 
  mutate(
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 24),
    int_p_fdr  = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 24)
        x
      }
    },
    # pretty labels
    outcome_pretty = recode(outcome,
                            mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                            mean_rt_2b = "RT", a_2b = "Threshold", t_mean = "NDT"),
    pub_var_pretty = recode(pub_var,
                            pub_timing_z = "Timing", pub_tempo_z = "Tempo"),
    model_type = if_else(winner == "pub_int", "Interaction", "Main eff.")
  ) %>%
  select(sex,
         covariates,
         outcome    = outcome_pretty,
         pub_var    = pub_var_pretty,
         model_type,
         estimate   = main_est,
         main_se,
         main_t,
         main_p,
         main_p_fdr,
         age_edf,
         int_edf,
         int_F,
         int_p,
         int_p_fdr)



write_csv(effects_males, "puberty_winning_effects_mgcv_males_rev1.csv")
write_csv(effects_females, "puberty_winning_effects_mgcv_females_rev1.csv")


effects_ft_males = flextable(effects_males) %>%
  colformat_double(j = c("estimate", "main_se"), digits = 4) %>%
  colformat_double(j = c("main_t", "age_edf", "int_edf", "int_F"), digits = 2) %>% 
  colformat_double(j = c("main_p", "main_p_fdr", "int_p", "int_p_fdr"),
                   digits = 4) %>%
  set_header_labels(
    sex = "Sex", outcome = "Outcome", pub_var = "Puberty variable",
    model_type = "Model type",
    estimate = "Estimate", main_se = "Main SE", main_t = "Main t",
    main_p = "Main p", main_p_fdr = "Main pFDR",
    age_edf = "Age EDF",
    int_edf = "Int. EDF", int_F = "Int. F",
    int_p = "Int. p", int_p_fdr = "Int. pFDR"
  )

effects_ft_females = flextable(effects_females) %>%
  colformat_double(j = c("estimate", "main_se"), digits = 4) %>%
  colformat_double(j = c("main_t", "age_edf", "int_edf", "int_F"), digits = 2) %>% 
  colformat_double(j = c("main_p", "main_p_fdr", "int_p", "int_p_fdr"),
                   digits = 4) %>%
  set_header_labels(
    sex = "Sex", outcome = "Outcome", pub_var = "Puberty variable",
    model_type = "Model type",
    estimate = "Estimate", main_se = "Main SE", main_t = "Main t",
    main_p = "Main p", main_p_fdr = "Main pFDR",
    age_edf = "Age EDF",
    int_edf = "Int. EDF", int_F = "Int. F",
    int_p = "Int. p", int_p_fdr = "Int. pFDR"
  )


save_as_html(effects_ft_males, path = "puberty_winning_effects_mgcv_males_rev1.html")
save_as_html(effects_ft_females, path = "puberty_winning_effects_mgcv_females_rev1.html")




# raw-name version of effects table 
effects_raw = bind_rows(effects_males, effects_females) %>%
  rename(outcome_raw = outcome, pub_var_raw = pub_var) %>% 
  mutate(
    outcome_raw = recode(outcome_raw,
                         "Accuracy" = "mean_acc_2b",
                         "Response Time" = "mean_rt_2b",
                         "Drift rate" = "v_2b",
                         "Bias" = "z_2b",
                         "Threshold" = "a_2b",
                         "NDT" = "t_mean"),
    pub_var_raw = recode(pub_var_raw,
                         "Timing" = "pub_timing_z",
                         "Tempo" = "pub_tempo_z")
  )


###
### Visualization of pubertal winners 
### 

make_pub_groups = function(df, pub_var, seed = 1104) {
  set.seed(seed)
  km = kmeans(df[[pub_var]], centers = 3)
  ord = order(as.numeric(km$centers))
  cl = match(km$cluster, ord)
  labels = if (pub_var == "pub_timing_z") c("earlier/", "average", "later/") else
    c("slower",   "average", "faster")
  df %>% mutate(pub_group = factor(labels[cl], levels = labels))
}

predict_by_group = function(fit, df, y_var, pub_var, n_age = 200) {
  df_g = make_pub_groups(df, pub_var)
  
  age_seq = seq(min(df_g$age, na.rm = TRUE),
                max(df_g$age, na.rm = TRUE), length.out = n_age)
  age_mu = mean(df_g$age, na.rm = TRUE)
  age_sd = sd(df_g$age,   na.rm = TRUE)
  
  pub_vals = df_g %>%
    group_by(pub_group) %>%
    summarise(pub_value = mean(.data[[pub_var]], na.rm = TRUE), .groups = "drop")
  
  newdat = expand_grid(age = age_seq, pub_group = pub_vals$pub_group) %>%
    left_join(pub_vals, by = "pub_group") %>%
    mutate(age_z = (age - age_mu) / age_sd,
           !!pub_var := pub_value) %>%
    select(age, age_z, all_of(pub_var), pub_group)
  
  pr = predict(fit$gam, newdata = newdat, se.fit = TRUE, type = "response")
  
  newdat %>% mutate(outcome = y_var,
                    fit   = as.numeric(pr$fit),
                    se    = as.numeric(pr$se.fit),
                    lower = fit - 1.96 * se,
                    upper = fit + 1.96 * se)
}


plot_pub_curves = function(pred_df, title = NULL, outcome_labs = NULL,
                           is_sig = TRUE) {
  y_var = unique(pred_df$outcome)
  y_lab = if (!is.null(outcome_labs) && y_var %in% names(outcome_labs))
    outcome_labs[[y_var]] else y_var
  
  cols = c(`earlier/` = "#00A5CF", slower = "#00A5CF",
           average    = "#000000",
           `later/`   = "#DE1A1A", faster = "#DE1A1A")
  
  #line_type = if (is_sig) "solid" else "dashed"
  
  ggplot(pred_df, aes(x = age, y = fit, colour = pub_group, fill = pub_group)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
    # geom_line(linewidth = 1.1, alpha = .8, linetype = line_type) +
    geom_line(linewidth = 1.1, alpha = .8) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    theme_minimal(base_size = 14) +
    labs(x = "Age", y = y_lab, title = title, colour = NULL, fill = NULL) +
    theme(plot.title = element_text(size = 12),
          axis.title = element_text(size = 13),
          axis.text  = element_text(size = 11))
}

# Make figure looping only over pubertal winners
make_sex_figure = function(sex_label, df,
                           results_timing, results_tempo,
                           winners_tbl_sex, effects_tbl_sex,
                           outcome_labs, ncol = 3) {
  
  pub_winners = winners_tbl_sex %>%
    filter(winner %in% c("pub_main", "pub_int")) %>%
    select(outcome_raw, pub_var_raw, winner)
  
  pub_winners = pub_winners %>%
    left_join(
      effects_tbl_sex %>% select(outcome_raw, pub_var_raw,
                                 main_p_fdr, int_p_fdr),
      by = c("outcome_raw", "pub_var_raw")
    ) %>%
    mutate(
      relevant_p_fdr = if_else(winner == "pub_int", int_p_fdr, main_p_fdr),
      is_sig = !is.na(relevant_p_fdr) & relevant_p_fdr < 0.05
    )
  
  plots = pmap(
    pub_winners %>% select(outcome_raw, pub_var_raw, winner, is_sig),
    function(outcome_raw, pub_var_raw, winner, is_sig) {
      res_list = if (pub_var_raw == "pub_timing_z") results_timing else results_tempo
      fit_use  = res_list[[outcome_raw]]$fits[[winner]]
      
      title = paste(
        if (pub_var_raw == "pub_timing_z") "Timing" else "Tempo",
        if (winner == "pub_int") "× Age" else "+ Age"
      )
      
      pred = predict_by_group(fit_use, df, outcome_raw, pub_var_raw)
      plot_pub_curves(pred, title = title, outcome_labs = outcome_labs,
                      is_sig = is_sig)
    })
  
  fig = wrap_plots(plots, ncol = ncol, nrow = ceiling(length(plots) / ncol)) +
    plot_annotation(title = sex_label,
                    theme = theme(legend.position = "bottom",
                                  plot.title = element_text(hjust = 0.5, size = 18))) +
    plot_layout(guides = "collect")
  
  list(fig = fig, plots = plots)
}



outcome_labs = c(
  mean_acc_2b = "Accuracy",
  mean_rt_2b  = "Response Time",
  v_2b        = "Drift Rate",
  a_2b        = "Decision \nThreshold",
  t_mean      = "Non-Decision \nTime",
  z_2b        = "Bias"
)

fig_males = make_sex_figure(
  "Males", males,
  results_males_timing, results_males_tempo,
  winners_raw %>% filter(sex == "M"),
  effects_raw %>% filter(sex == "M"),
  outcome_labs
)

fig_females = make_sex_figure(
  "Females", females,
  results_females_timing, results_females_tempo,
  winners_raw %>% filter(sex == "F"),
  effects_raw %>% filter(sex == "F"),
  outcome_labs
)



ggsave("puberty_ddm_models_males_MGCV_rev1.png",   fig_males,   width = 8, height = 9)
ggsave("puberty_ddm_models_females_MGCV_rev1.png", fig_females, width = 8, height = 9)



###########
############
############# SENSITIVITY ANALYSES  
############
##########



path = "..."

df_income = read_delim(paste0(path, "ab_p_demo.tsv")) %>% #ab_p_demo__income__hhold_001
  rename(house_inc = ab_p_demo__income__hhold_001,
         house_size = ab_p_demo__roster_001,
         sub_id = participant_id) %>% 
  filter(session_id == "ses-00A",
         house_inc != "n/a") %>% 
  mutate(house_inc = case_when(house_inc == "1" ~ round(median(c(0,4999))),         # Less than $5,000
                               house_inc == "2" ~ round(median(c(5000,11999))),     # $5,000 through $11,999
                               house_inc == "3" ~ round(median(c(12000,15999))),    # $12,000 through $15,999
                               house_inc == "4" ~ round(median(c(16000,24999))),    # $16,000 through $24,999
                               house_inc == "5" ~ round(median(c(25000,34999))),    # $25,000 through $34,999
                               house_inc == "6" ~ round(median(c(35000,49999))),    # $35,000 through $49,999
                               house_inc == "7" ~ round(median(c(50000,74999))),    # $50,000 through $74,999
                               house_inc == "8" ~ round(median(c(75000,99999))),    # $75,000 through $99,999
                               house_inc == "9" ~ round(median(c(100000,199999))),  # $100,000 through $199,999 
                               house_inc == "10" ~ round(median(c(200000,300000))), # $200,000 and greater
                               house_inc == "999" | house_inc == "777" ~ NA),       # 999 == don't know / 777 == decline to answer
         house_size = as.numeric(house_size)) %>%  
  filter(house_size < 16) %>%     # n = 7 out 
  select(sub_id, session_id, house_inc, house_size) %>% 
  filter(sub_id %in% est$sub_id) %>% 
  mutate(poverty_line = case_when(house_size == 1 ~ 12060,
                                  house_size == 2 ~ 16240,
                                  house_size == 3 ~ 20420,
                                  house_size == 4 ~ 24600,
                                  house_size == 5 ~ 28780,
                                  house_size == 6 ~ 32960,
                                  house_size == 7 ~ 37140,
                                  house_size == 8 ~ 41320,
                                  house_size > 8  ~ 41320 + (house_size - 8) * 4180),
         ICR = house_inc / poverty_line) %>% 
  select(sub_id, ICR)

est = left_join(est, df_income, by = "sub_id")



###
#### BMI
###


df_ph = read_delim(paste0(path, "ph_y_anthr.tsv")) %>%
  filter(session_id == "ses-00A",
         participant_id %in% est$sub_id) %>%
  select(participant_id, 
         session_id,
         ph_y_anthr__height_mean,
         ph_y_anthr__weight_mean) %>% 
  rename(height = ph_y_anthr__height_mean,
         weight = ph_y_anthr__weight_mean) %>% 
  mutate(weight_kg = as.numeric(weight)*0.45359237,
         height_m = as.numeric(height)*0.0254,
         bmi = weight_kg / (height_m * height_m)) %>% 
  rename(sub_id = "participant_id") %>% 
  select(sub_id, bmi)

est = left_join(est, df_ph, by = "sub_id")


###
#### Race/ethnicity
###

df_ethn = read_delim(paste0(path, "ab_g_stc.tsv")) %>% 
  filter(participant_id %in% est$sub_id) %>% 
  select(sub_id = participant_id,
         ethnicity = ab_g_stc__cohort_ethnrace__leg) %>%
  mutate(ethnicity = factor(ethnicity, 
                            levels = c(2,1,4,3,13),
                            labels = c("White","Hispanic","Asian","Black","Other")))

est = left_join(est, df_ethn)


###
#### Parental education
###

# Highest education across caregivers

df_edu = read_delim(paste0(path, "ab_g_dyn.tsv")) %>% 
  filter(participant_id %in% est$sub_id,
         session_id == "ses-00A") %>% 
  select(sub_id = participant_id,
         education = ab_g_dyn__cohort_edu__cgs) %>% 
  mutate(education = factor(education,
                            levels = c(1, 2, 3, 4, 5),
                            labels = c("Up to high school (no diploma)",
                                       "High school diploma/GED",
                                       "Some college",
                                       "Bachelor's degree",
                                       "Graduate school or professional degree")))

est = left_join(est,df_edu)


####################
#
# Analyses
#
#
######


males = est %>% filter(sex == "M") %>%
  mutate(age_z = z(age), pub_tempo_z = z(pub_tempo), pub_timing_z = z(pub_timing),
         icr_z = z(ICR), bmi_z = z(bmi))

females = est %>% filter(sex == "F") %>%
  mutate(age_z = z(age), pub_tempo_z = z(pub_tempo), pub_timing_z = z(pub_timing),
         icr_z = z(ICR), bmi_z = z(bmi))



fit_agepub_sens_gam = function(y_var, df, pub_var, n_k) {
  
  fml_age      = as.formula(paste0(y_var, " ~ icr_z + bmi_z + ethnicity + education + s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_main = as.formula(paste0(y_var, " ~ ", pub_var,
                                   "+ icr_z + bmi_z + ethnicity + education + s(age_z, bs = 'cs', k=", n_k, ")"))
  fml_pub_int  = as.formula(paste0(y_var, " ~ ", pub_var,
                                   " + icr_z + bmi_z + ethnicity + education + s(age_z, bs = 'cs', k=", n_k, ")",
                                   " + s(age_z, bs = 'cs', by = ", pub_var,
                                   ", k=", n_k, ")"))
  
  
  fit_age      = gamm(fml_age,      data = df, random = list(sub_id = ~1), method = "ML")
  fit_pub_main = gamm(fml_pub_main, data = df, random = list(sub_id = ~1), method = "ML")
  fit_pub_int  = gamm(fml_pub_int,  data = df, random = list(sub_id = ~1), method = "ML")
  
  fits = list(age = fit_age, pub_main = fit_pub_main, pub_int = fit_pub_int)
  
  
  aics = c(age      = as.numeric(AIC(fit_age$lme)),
           pub_main = as.numeric(AIC(fit_pub_main$lme)),
           pub_int  = as.numeric(AIC(fit_pub_int$lme)))
  
  cmp_age_vs_main = anova(fit_age$lme,      fit_pub_main$lme)
  cmp_main_vs_int = anova(fit_pub_main$lme, fit_pub_int$lme)
  cmp_age_vs_int  = anova(fit_age$lme,      fit_pub_int$lme)
  
  lrt_age_vs_main = get_lrt_stats(cmp_age_vs_main)
  lrt_main_vs_int = get_lrt_stats(cmp_main_vs_int)
  lrt_age_vs_int  = get_lrt_stats(cmp_age_vs_int)
  
  alpha = 0.05
  sig_age_vs_main = !is.na(lrt_age_vs_main$p) && lrt_age_vs_main$p < alpha
  sig_main_vs_int = !is.na(lrt_main_vs_int$p) && lrt_main_vs_int$p < alpha
  sig_age_vs_int  = !is.na(lrt_age_vs_int$p)  && lrt_age_vs_int$p  < alpha
  
  winner =
    if (!sig_age_vs_main && !sig_age_vs_int) {
      "age"
    } else if (sig_age_vs_main && !sig_age_vs_int) {
      "pub_main"
    } else if (!sig_age_vs_main && sig_age_vs_int) {
      "pub_int"
    } else {
      if (sig_main_vs_int) "pub_int" else "pub_main"
    }
  
  # One-row summary for this (outcome, pub_var): AIC + LRT stats in one place
  winner_row = tibble(
    outcome           = y_var,
    pub_var           = pub_var,
    winner            = winner,
    
    AIC_age           = aics[["age"]],
    AIC_pub_main      = aics[["pub_main"]],
    AIC_pub_int       = aics[["pub_int"]],
    AIC_winner        = aics[[winner]],
    dAIC_vs_age       = aics[[winner]] - aics[["age"]],
    
    chisq_age_vs_main = lrt_age_vs_main$chisq, df_age_vs_main = lrt_age_vs_main$df, p_age_vs_main = lrt_age_vs_main$p,
    chisq_main_vs_int = lrt_main_vs_int$chisq, df_main_vs_int = lrt_main_vs_int$df, p_main_vs_int = lrt_main_vs_int$p,
    chisq_age_vs_int  = lrt_age_vs_int$chisq,  df_age_vs_int  = lrt_age_vs_int$df,  p_age_vs_int  = lrt_age_vs_int$p
  )
  
  list(fits = fits, winner = winner, winner_row = winner_row)
}


run_sens_block = function(df, pub_var, sex_label) {
  res = map(outcomes,
            ~ fit_agepub_sens_gam(y_var = .x, df = df, pub_var = pub_var, n_k = 7))
  names(res) = outcomes
  list(
    results = res,
    winners = map_dfr(res, "winner_row") %>% mutate(sex = sex_label, .before = 1)
  )
}

m_timing_sensitivity = run_sens_block(males,   "pub_timing_z", "M")
m_tempo_sensitivity  = run_sens_block(males,   "pub_tempo_z",  "M")
f_timing_sensitivity = run_sens_block(females, "pub_timing_z", "F")
f_tempo_sensitivity  = run_sens_block(females, "pub_tempo_z",  "F")


results_males_timing_sensitivity   = m_timing_sensitivity$results
results_males_tempo_sensitivity    = m_tempo_sensitivity$results
results_females_timing_sensitivity = f_timing_sensitivity$results
results_females_tempo_sensitivity  = f_tempo_sensitivity$results




winners_tbl_sens = bind_rows(
  m_timing_sensitivity$winners,
  m_tempo_sensitivity$winners,
  f_timing_sensitivity$winners,
  f_tempo_sensitivity$winners
) %>%
  mutate(
    winner_lab = case_when(
      winner == "age"                                 ~ "Age",
      winner == "pub_main" & pub_var == "pub_timing_z" ~ "Timing Main",
      winner == "pub_main" & pub_var == "pub_tempo_z"  ~ "Tempo Main",
      winner == "pub_int"  & pub_var == "pub_timing_z" ~ "Timing Interaction",
      winner == "pub_int"  & pub_var == "pub_tempo_z"  ~ "Tempo Interaction"
    ),
    outcome_lab = recode(outcome,
                         mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                         mean_rt_2b  = "RT",       a_2b = "Threshold",  t_mean = "NDT"),
    pub_var_lab = recode(pub_var,
                         pub_timing_z = "Timing", pub_tempo_z = "Tempo")
  ) %>%
  select(sex, outcome = outcome_lab, pub_var = pub_var_lab,
         winner = winner_lab,
         AIC_age, AIC_pub_main, AIC_pub_int, dAIC_vs_age,
         lrt_age_vs_main = chisq_age_vs_main,
         lrt_main_vs_int = chisq_main_vs_int,
         lrt_age_vs_int  = chisq_age_vs_int,
         p_age_vs_main, p_main_vs_int, p_age_vs_int)


# WRITE WINNERS TABLE
write_csv(winners_tbl_sens, "puberty_winning_models_mgcv_sensitivity_rev1.csv")

# prettier table:)
winners_ft_sens = flextable(winners_tbl_sens) %>%
  colformat_double(j = c("AIC_age", "AIC_pub_main", "AIC_pub_int",
                         "dAIC_vs_age", "lrt_age_vs_main", 
                         "lrt_main_vs_int", "lrt_age_vs_int"), digits = 2) %>%
  colformat_double(j = c("p_age_vs_main", "p_main_vs_int", "p_age_vs_int"),
                   digits = 3) %>%
  set_header_labels(
    sex = "Sex", outcome = "Outcome", pub_var = "Pub var",
    winner = "Winning model",
    AIC_age = "AIC age", AIC_pub_main = "AIC main", AIC_pub_int = "AIC int",
    dAIC_vs_age = "ΔAIC vs age",
    lrt_age_vs_main = "lrtAM", lrt_main_vs_int = "lrtMI", lrt_age_vs_int = "lrtAI",
    p_age_vs_main = "pAM",
    p_main_vs_int = "pMI",
    p_age_vs_int  = "pAI"
  )
save_as_html(winners_ft_sens, path = "puberty_winning_models_mgcv_sensitivity_rev1.html")




# GET EFFECTS 
# raw-name version of the winners table, used for both effects extraction
# and figure construction
winners_raw_sens = bind_rows(m_timing_sensitivity$winners, m_tempo_sensitivity$winners,
                        f_timing_sensitivity$winners, f_tempo_sensitivity$winners) %>%
  rename(outcome_raw = outcome, pub_var_raw = pub_var) 

extract_effects = function(fit_gam, outcome, pub_var, winner) {
  s    = summary(fit_gam)
  ptab = as.data.frame(s$p.table)
  stab = as.data.frame(s$s.table)
  
  # parametric pub effect
  pull_p = function(col) if (pub_var %in% rownames(ptab)) ptab[pub_var, col] else NA_real_
  
  # age smooth
  age_term = "s(age_z)"
  pull_s = function(tab, term, col)
    if (term %in% rownames(tab)) tab[term, col] else NA_real_
  
  # interaction smooth (only in pub_int)
  int_term = paste0("s(age_z):", pub_var)
  
  tibble(
    outcome   = outcome,
    pub_var   = pub_var,
    winner    = winner,
    main_est  = pull_p("Estimate"),
    main_se   = pull_p("Std. Error"),
    main_t    = pull_p("t value"),
    main_p    = pull_p("Pr(>|t|)"),
    age_edf   = pull_s(stab, age_term, "edf"),
    age_F     = pull_s(stab, age_term, "F"),
    age_p     = pull_s(stab, age_term, "p-value"),
    int_edf   = if (winner == "pub_int") pull_s(stab, int_term, "edf")     else NA_real_,
    int_F     = if (winner == "pub_int") pull_s(stab, int_term, "F")       else NA_real_,
    int_p     = if (winner == "pub_int") pull_s(stab, int_term, "p-value") else NA_real_
  )
}


build_effects_table = function(winners_raw_sex,
                               results_timing, results_tempo) {
  pub_only = winners_raw_sex %>%
    filter(winner %in% c("pub_main", "pub_int"))
  
  pmap_dfr(pub_only %>% select(outcome_raw, pub_var_raw, winner),
           function(outcome_raw, pub_var_raw, winner) {
             res_list = if (pub_var_raw == "pub_timing_z") results_timing else results_tempo
             fit_gam  = res_list[[outcome_raw]]$fits[[winner]]$gam
             extract_effects(fit_gam, outcome_raw, pub_var_raw, winner)
           })
}



effects_males_sens = build_effects_table(
  winners_raw_sens %>% filter(sex == "M"),
  results_males_timing_sensitivity, results_males_tempo_sensitivity) %>% 
  mutate(sex = "M", .before = 1) %>% 
  mutate(covariates = "Yes") %>% 
  mutate(
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 24),
    int_p_fdr  = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 24)
        x
      }
    },
    outcome_pretty = recode(outcome,
                            mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                            mean_rt_2b = "RT", a_2b = "Threshold", t_mean = "NDT"),
    pub_var_pretty = recode(pub_var,
                            pub_timing_z = "Timing", pub_tempo_z = "Tempo"),
    model_type = if_else(winner == "pub_int", "Interaction", "Main eff.")
  ) %>%
  select(sex,
         covariates,
         outcome    = outcome_pretty,
         pub_var    = pub_var_pretty,
         model_type,
         estimate   = main_est,
         main_se,
         main_t,
         main_p,
         main_p_fdr,
         age_edf,
         int_edf,
         int_F,
         int_p,
         int_p_fdr)

effects_females_sens = build_effects_table(
  winners_raw_sens %>% filter(sex == "F"),
  results_females_timing_sensitivity, results_females_tempo_sensitivity) %>% 
  mutate(sex = "F", .before = 1) %>% 
  mutate(covariates = "Yes") %>% 
  mutate(
    main_p_fdr = p.adjust(main_p, method = "fdr", n = 24),
    int_p_fdr  = {
      x = int_p
      if (all(is.na(x))) x else {
        x[!is.na(x)] = p.adjust(x[!is.na(x)], method = "fdr", n = 24)
        x
      }
    },
    outcome_pretty = recode(outcome,
                            mean_acc_2b = "Accuracy", v_2b = "Drift rate", z_2b = "Bias",
                            mean_rt_2b = "RT", a_2b = "Threshold", t_mean = "NDT"),
    pub_var_pretty = recode(pub_var,
                            pub_timing_z = "Timing", pub_tempo_z = "Tempo"),
    model_type = if_else(winner == "pub_int", "Interaction", "Main eff.")
  ) %>%
  select(sex,
         covariates,
         outcome    = outcome_pretty,
         pub_var    = pub_var_pretty,
         model_type,
         estimate   = main_est,
         main_se,
         main_t,
         main_p,
         main_p_fdr,
         age_edf,
         int_edf,
         int_F,
         int_p,
         int_p_fdr)



write_csv(effects_males_sens, "puberty_winning_effects_mgcv_males_sens_rev1.csv")
write_csv(effects_females_sens, "puberty_winning_effects_mgcv_females_sens_rev1.csv")


   # 
  # # 
 # # #
# # # # Make table 
 # # #
  # # 
   # 


effects_m_all = bind_rows(effects_males,
                          effects_males_sens)

effects_f_all = bind_rows(effects_females,
                          effects_females_sens)

effects_ft_males_sens = flextable(effects_m_all) %>%
  colformat_double(j = c("estimate", "main_se"), digits = 3) %>%
  colformat_double(j = c("main_t", "age_edf", "int_edf", "int_F"), digits = 2) %>% 
  colformat_double(j = c("main_p", "main_p_fdr", "int_p", "int_p_fdr"),
                   digits = 3) %>%
  set_header_labels(
    sex = "Sex", covariates = "Covariates",outcome = "Outcome", pub_var = "Puberty variable",
    model_type = "Model type",
    estimate = "Estimate", main_se = "Main SE", main_t = "Main t",
    main_p = "Main p", main_p_fdr = "Main pFDR",
    age_edf = "Age EDF",
    int_edf = "Int. EDF", int_F = "Int. F",
    int_p = "Int. p", int_p_fdr = "Int. pFDR"
  )

effects_ft_females_sens = flextable(effects_f_all) %>%
  colformat_double(j = c("estimate", "main_se"), digits = 3) %>%
  colformat_double(j = c("main_t", "age_edf", "int_edf", "int_F"), digits = 2) %>% 
  colformat_double(j = c("main_p", "main_p_fdr", "int_p", "int_p_fdr"),
                   digits = 3) %>%
  set_header_labels(
    sex = "Sex", covariates = "Covariates",outcome = "Outcome", pub_var = "Puberty variable",
    model_type = "Model type",
    estimate = "Estimate", main_se = "Main SE", main_t = "Main t",
    main_p = "Main p", main_p_fdr = "Main pFDR",
    age_edf = "Age EDF",
    int_edf = "Int. EDF", int_F = "Int. F",
    int_p = "Int. p", int_p_fdr = "Int. pFDR"
  )


save_as_html(effects_ft_males_sens, path = "puberty_winning_effects_mgcv_males_sens_rev1.html")
save_as_html(effects_ft_females_sens, path = "puberty_winning_effects_mgcv_females_sens_rev1.html")

# 
# # 
# # #
#   #  # # # # Visualize 
# # #
# # 
# 

predict_by_group_sens = function(fit, df, y_var, pub_var, n_age = 200) {
  df_g = make_pub_groups(df, pub_var)
  
  age_seq = seq(min(df_g$age, na.rm = TRUE),
                max(df_g$age, na.rm = TRUE), length.out = n_age)
  age_mu = mean(df_g$age, na.rm = TRUE)
  age_sd = sd(df_g$age,   na.rm = TRUE)
  
  pub_vals = df_g %>%
    group_by(pub_group) %>%
    summarise(pub_value = mean(.data[[pub_var]], na.rm = TRUE), .groups = "drop")
  
  # Account for the sensitivity analyses
  icr_mean = mean(df$icr_z, na.rm = T)
  bmi_mean = mean(df$bmi_z, na.rm = T)
  ethnicity_mode = df %>%
    count(ethnicity, sort = TRUE) %>%
    slice(1) %>%
    pull(ethnicity)
  pared_mode = df_g %>%
    count(education, sort = TRUE) %>%
    slice(1) %>%
    pull(education)
  
  newdat = expand_grid(age = age_seq, pub_group = pub_vals$pub_group) %>%
    left_join(pub_vals, by = "pub_group") %>%
    mutate(age_z = (age - age_mu) / age_sd,
           !!pub_var := pub_value,
           icr_z = icr_mean,
           bmi_z = bmi_mean,
           ethnicity = ethnicity_mode,
           education = pared_mode) %>%
    select(age, age_z, all_of(pub_var), pub_group,
           icr_z, bmi_z, ethnicity, education)
  
  pr = predict(fit$gam, newdata = newdat, se.fit = TRUE, type = "response")
  
  newdat %>% mutate(outcome = y_var,
                    fit   = as.numeric(pr$fit),
                    se    = as.numeric(pr$se.fit),
                    lower = fit - 1.96 * se,
                    upper = fit + 1.96 * se)
}

make_sex_figure_sens = function(sex_label, df,
                           results_timing, results_tempo,
                           winners_tbl_sex, effects_tbl_sex,
                           outcome_labs, ncol = 3) {
  
  pub_winners = winners_tbl_sex %>%
    filter(winner %in% c("pub_main", "pub_int")) %>%
    select(outcome_raw, pub_var_raw, winner)
  
  pub_winners = pub_winners %>%
    left_join(
      effects_tbl_sex %>% select(outcome_raw, pub_var_raw,
                                 main_p_fdr, int_p_fdr),
      by = c("outcome_raw", "pub_var_raw")
    ) %>%
    mutate(
      relevant_p_fdr = if_else(winner == "pub_int", int_p_fdr, main_p_fdr),
      is_sig = !is.na(relevant_p_fdr) & relevant_p_fdr < 0.05
    )
  
  plots = pmap(
    pub_winners %>% select(outcome_raw, pub_var_raw, winner, is_sig),
    function(outcome_raw, pub_var_raw, winner, is_sig) {
      res_list = if (pub_var_raw == "pub_timing_z") results_timing else results_tempo
      fit_use  = res_list[[outcome_raw]]$fits[[winner]]
      
      title = paste(
        if (pub_var_raw == "pub_timing_z") "Timing" else "Tempo",
        if (winner == "pub_int") "× Age" else "+ Age"
      )
      
      pred = predict_by_group_sens(fit_use, df, outcome_raw, pub_var_raw)
      plot_pub_curves(pred, title = title, outcome_labs = outcome_labs,
                      is_sig = is_sig)
    })
  
  fig = wrap_plots(plots, ncol = ncol, nrow = ceiling(length(plots) / ncol)) +
    plot_annotation(title = sex_label,
                    theme = theme(legend.position = "bottom",
                                  plot.title = element_text(hjust = 0.5, size = 18))) +
    plot_layout(guides = "collect")
  
  list(fig = fig, plots = plots)
}

effects_raw_sens = bind_rows(effects_males_sens, effects_females_sens) %>%
  rename(outcome_raw = outcome, pub_var_raw = pub_var) %>% 
  mutate(
    outcome_raw = recode(outcome_raw,
                         "Accuracy" = "mean_acc_2b",
                         "Response Time" = "mean_rt_2b",
                         "Drift rate" = "v_2b",
                         "Bias" = "z_2b",
                         "Threshold" = "a_2b",
                         "NDT" = "t_mean"),
    pub_var_raw = recode(pub_var_raw,
                         "Timing" = "pub_timing_z",
                         "Tempo" = "pub_tempo_z")
  )


fig_males_sens = make_sex_figure_sens(
  "Males", males,
  results_males_timing_sensitivity, 
  results_males_tempo_sensitivity,
  winners_raw_sens %>% filter(sex == "M"),
  effects_raw_sens %>% filter(sex == "M"),
  outcome_labs
)

fig_females_sens = make_sex_figure_sens(
  "Females", females,
  results_females_timing_sensitivity, 
  results_females_tempo_sensitivity,
  winners_raw_sens %>% filter(sex == "F"),
  effects_raw_sens %>% filter(sex == "F"),
  outcome_labs
)


plots_all = c(fig_females$plots, list(plot_spacer()), fig_females_sens$plots)
plots_all = map(plots_all, ~ .x + 
                  theme_minimal(base_size = 11) +
                  theme(legend.position = "none") +
                  xlab("Age (years)"))

fig_females_combined = wrap_plots(plots_all, ncol = 3) +
  plot_annotation(title = "Females",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

plots_all_m = c(fig_males$plots, fig_males_sens$plots)
plots_all_m = map(plots_all_m, ~ .x + 
                    theme_minimal(base_size = 11) +
                    theme(legend.position = "none") +
                    xlab("Age (years)"))

fig_males_combined = wrap_plots(plots_all_m, ncol = 3) +
  plot_annotation(title = "Males",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))


ggsave("fig_puberty_trajectories_F_with_sensitivity_rev1.png",fig_females_combined, 
       width = 8, height = 9, units = "in")
ggsave("fig_puberty_trajectories_M_with_sensitivity_rev1.png",fig_males_combined, 
       width = 8, height = 9, units = "in")
