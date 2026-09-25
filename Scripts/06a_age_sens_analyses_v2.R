# Sensitivity analyses for the age-related analyses (not puberty-related)
# 
# Two analyses being done:
# ---> 1) GAMM trajectories differentiated by sex
# ---> 2) GAMM trajectories for the 0-back parameters 
# Both are shown in supplementary materials. 


# R/4.4.2-gfbf-2024a



library(tidyverse)
library(patchwork)
library(gratia)
library(mgcv)
library(scales)


est = read_csv("estimates_covariates_ageanalyses_rev1.csv") %>%
  mutate(n_corr = n_trials * mean_acc,
         sex = factor(sex),
         v_0b_lure    = v_0b_lure    * -1,
         v_2b_lure    = v_2b_lure    * -1,
         v_0b_nonlure = v_0b_nonlure * -1,
         v_2b_nonlure = v_2b_nonlure * -1) %>%
  mutate(v_0b = rowMeans(select(., starts_with("v_0b"))),
         v_2b = rowMeans(select(., starts_with("v_2b"))))

outcomes = c("mean_acc_2b", "v_2b", "z_2b",
             "mean_rt_2b", "a_2b", "t")

age_long = est %>%
  select(sub_id, age, all_of(outcomes)) %>%
  pivot_longer(cols = all_of(outcomes),
               names_to  = "outcome",
               values_to = "value") %>%
  mutate(outcome = factor(outcome, levels = outcomes))




fit_age_gam = function(y_var, df, n_k = 7) {
  fml = as.formula(paste0(y_var, " ~ sex + s(age, by = sex, k=", n_k, ")"))
  fit = gamm(fml, data = df, random = list(sub_id = ~1), method = "REML")
  
  age_seq = seq(min(df$age, na.rm = TRUE), max(df$age, na.rm = TRUE), length.out = 200)
  newdata = expand_grid(age = age_seq, sex = levels(df$sex)) %>%
    mutate(sex = factor(sex, levels = levels(df$sex)))
  
  pr = predict(fit$gam, newdata = newdata, se.fit = TRUE, type = "response")
  
  preds = tibble(
    age     = newdata$age,
    sex     = newdata$sex,
    outcome = y_var,
    fit     = as.numeric(pr$fit),
    se      = as.numeric(pr$se.fit),
    lower   = fit - 1.96 * se,
    upper   = fit + 1.96 * se
  )
  
  list(fit = fit, preds = preds)
}



get_age_deriv = function(fit, df, n = 200,
                         interval = "simultaneous", level = 0.95) {
  sexes = levels(df$sex)
  
  sm = smooths(fit$gam)
  age_smooths = sm[grepl("age", sm)]
  
  map_dfr(sexes, function(this_sex) {
    age_grid = tibble(
      age = seq(min(df$age, na.rm = TRUE), max(df$age, na.rm = TRUE), length.out = n),
      sex = factor(this_sex, levels = sexes)
    )
    
    this_smooth = age_smooths[grepl(this_sex, age_smooths)][1]
    
    deriv = derivatives(fit$gam,
                        select   = this_smooth,
                        data     = age_grid,
                        order    = 1,
                        interval = interval,
                        level    = level) %>%
      as_tibble()
    
    deriv_col = names(deriv)[grepl("deriv", names(deriv))][1]
    
    deriv %>%
      rename(deriv = !!deriv_col) %>%
      mutate(sex = this_sex)
  }) %>%
    mutate(
      sig = (.lower_ci > 0) | (.upper_ci < 0),
      direction = case_when(
        sig & deriv > 0 ~ "increasing",
        sig & deriv < 0 ~ "decreasing",
        TRUE             ~ "ns"
      )
    )
}




results = map(outcomes, ~ fit_age_gam(.x, df = est, n_k = 7))
names(results) = outcomes

pred_all = bind_rows(map(results, "preds")) %>%
  mutate(outcome = factor(outcome, levels = outcomes))

deriv_all = map2_dfr(results, outcomes, \(res, outcome) {
  get_age_deriv(res$fit, est, n = 200) %>%
    mutate(outcome = outcome)
}) %>%
  mutate(sig_deriv = if_else(sig, deriv, 0),
         outcome    = factor(outcome, levels = outcomes))




outcome_cols = c(
  mean_acc_2b = "#DE1A1A",
  mean_rt_2b  = "#943CB4",
  t           = "#f0027f",
  v_2b        = "#386cb0",
  a_2b        = "#66a61e",
  z_2b        = "#FFBF00"
)

outcome_labs = c(
  mean_acc_2b = "Accuracy",
  mean_rt_2b  = "Response Time",
  t           = "Non-Decision \nTime",
  v_2b        = "Drift Rate",
  a_2b        = "Decision \nThreshold",
  z_2b        = "Bias"
)

# ---- Fig: trajectories with per-sex derivative strip -----------------------

xlim_use = range(pred_all$age, na.rm = TRUE)

plot_traj_plus_continuous_strip = function(pred_df, deriv_df, raw_df,
                                           outcome_name,
                                           xlim = NULL,
                                           strip_height = 0.15,
                                           strip_lims = c(-1, 1)) {
  x_scale = scale_x_continuous(limits = xlim, expand = c(0, 0))
  this_col = unname(outcome_cols[[outcome_name]])
  this_lab = unname(outcome_labs[[outcome_name]])
  
  p_top =
    ggplot() +
    geom_line(data = raw_df,
              aes(x = age, y = value, group = sub_id),
              colour = this_col, linewidth = 0.5, alpha = 0.03,
              show.legend = FALSE) +
    geom_ribbon(data = pred_df,
                aes(x = age, ymin = lower, ymax = upper, group = sex),
                fill = "grey70", alpha = 0.6, colour = NA,
                inherit.aes = FALSE) +
    geom_line(data = pred_df,
              aes(x = age, y = fit, linetype = sex),
              colour = "black", linewidth = 1.3, inherit.aes = FALSE) +
    x_scale +
    labs(x = NULL, y = NULL, title = this_lab, linetype = "Sex") +
    theme_minimal(base_size = 14) +
    theme(plot.title  = element_text(size = 12, hjust = 0.5),
          axis.text.x = element_blank(),
          legend.position = "right",
          plot.margin = margin(6, 12, 0, 12))
  
  p_strip =
    ggplot(deriv_df, aes(x = age, y = sex, fill = sig_deriv)) +
    geom_raster(interpolate = FALSE) +
    x_scale +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradient2(
      low = "#08519c", mid = "white", high = "#a50f15",
      midpoint = 0, limits = strip_lims,
      oob = scales::squish,
      breaks = scales::pretty_breaks(n = 5),
      guide = "none"
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(axis.line.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_text(size = 8),
          plot.margin  = margin(0, 12, 6, 12))
  
  p_top / p_strip + plot_layout(heights = c(1, strip_height))
}

plots = map(outcomes, \(o) {
  pred_one  = pred_all  %>% filter(outcome == o)
  deriv_one = deriv_all %>% filter(outcome == o)
  raw_one   = age_long  %>% filter(outcome == o)
  
  local_mx = max(abs(deriv_one$sig_deriv), na.rm = TRUE)
  local_lims = c(-local_mx, local_mx)
  
  plot_traj_plus_continuous_strip(
    pred_df = pred_one, deriv_df = deriv_one, raw_df = raw_one,
    outcome_name = o, xlim = xlim_use,
    strip_height = 0.15, strip_lims = local_lims
  )
})

fig3 = wrap_plots(plots, ncol = 3, guides = "collect")


ggsave("age_trajectories_with_deriv_by_sex_rev1.png", fig3,
       height = 8, width = 7, units = "in", dpi = 500)





#####################
#
# Supplementary table

library(flextable)


outcome_labs_short = c(
  mean_acc_2b = "Accuracy",
  mean_rt_2b  = "RT",
  t           = "NDT",
  v_2b        = "Drift rate",
  a_2b        = "Threshold",
  z_2b        = "Bias"
)

extract_results_age_table = function(results, smooth_var = "age",
                                     p_adjust_method = "BH") {
  raw = results %>% map_dfr(function(res) {
    sm = summary(res$fit$gam)
    outcome = unique(res$preds$outcome)[1]
    
    as.data.frame(sm$s.table) %>%
      rownames_to_column("term") %>%
      filter(str_detect(term, paste0("s\\(", smooth_var, "\\)"))) %>%
      mutate(sex = str_extract(term, "(?<=sex)[A-Za-z0-9]+"),
             outcome = outcome) %>%
      transmute(outcome, sex,
                edf    = round(edf, 2),
                F_stat = round(F, 2),
                p      = `p-value`)
  })
  
  raw %>%
    mutate(p_fdr = p.adjust(p, method = p_adjust_method),
           p_fdr_disp = ifelse(p_fdr < 0.001,
                               "<0.001",
                               format(round(p_fdr, 3), nsmall = 3)),
           outcome = factor(outcome, levels = outcomes),
           sex     = factor(sex, levels = levels(est$sex))) %>%
    arrange(outcome, sex) %>%
    transmute(Outcome  = unname(outcome_labs_short[as.character(outcome)]),
              Sex      = as.character(sex),
              `Age EDF` = edf,
              F        = F_stat,
              `p (FDR)` = p_fdr_disp)
}

results_table = extract_results_age_table(results)

write_csv(results_table, "gamm_age_by_sex_resultsTable_rev1.csv")

ft = flextable(results_table) %>%
  merge_v(j = "Outcome") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  valign(j = "Outcome", valign = "center") %>%
  border_inner(border = officer::fp_border(width = 0.75)) %>%
  border_outer(border = officer::fp_border(width = 1.25)) %>%
  fix_border_issues() %>%
  autofit()

save_as_html(ft, path = "gamm_age_by_sex_resultsTable_rev1.html")



###############################################################################
#
#
# Do the original 2-back code, but with just 0-back parameters 
#
#


rm(list = ls())

est = read_csv("estimates_covariates_ageanalyses_rev1.csv") %>%
  mutate(n_corr = n_trials * mean_acc,
         sex = factor(sex),
         v_0b_lure    = v_0b_lure    * -1,
         v_2b_lure    = v_2b_lure    * -1,
         v_0b_nonlure = v_0b_nonlure * -1,
         v_2b_nonlure = v_2b_nonlure * -1) %>%
  mutate(v_0b = rowMeans(select(., starts_with("v_0b"))),
         v_2b = rowMeans(select(., starts_with("v_2b"))))

outcomes = c("mean_acc_0b", "v_0b", "z_0b",
             "mean_rt_0b", "a_0b", "t")

age_long = est %>%
  select(sub_id, age, all_of(outcomes)) %>%
  pivot_longer(cols = all_of(outcomes),
               names_to  = "outcome",
               values_to = "value") %>%
  mutate(outcome = factor(outcome, levels = outcomes))


fit_age_gam = function(y_var, df, n_k = 7) {
  fml = as.formula(paste0(y_var, " ~ s(age, k=", n_k, ")"))
  
  fit = gamm(fml, data = df, random = list(sub_id = ~1), method = "REML")
  
  age_seq = seq(min(df$age, na.rm = TRUE),
                max(df$age, na.rm = TRUE), length.out = 200)
  pr = predict(fit$gam, newdata = data.frame(age = age_seq),
               se.fit = TRUE, type = "response")
  
  preds = tibble(
    age     = age_seq,
    outcome = y_var,
    fit     = as.numeric(pr$fit),
    se      = as.numeric(pr$se.fit),
    lower   = fit - 1.96 * se,
    upper   = fit + 1.96 * se
  )
  
  list(fit = fit, preds = preds)
}


get_age_deriv = function(fit, df, n = 200,
                         interval = "simultaneous", level = 0.95) {
  age_grid = tibble(age = seq(min(df$age, na.rm = TRUE),
                              max(df$age, na.rm = TRUE),
                              length.out = n))
  
  sm = smooths(fit$gam)
  age_smooth = sm[grepl("age", sm)][1]
  
  deriv = derivatives(fit$gam,
                      select   = age_smooth,
                      data     = age_grid,
                      order    = 1,
                      interval = interval,
                      level    = level) %>%
    as_tibble()
  
  deriv_col = names(deriv)[grepl("deriv", names(deriv))][1]
  
  deriv %>%
    rename(deriv = !!deriv_col) %>%
    mutate(
      sig = (.lower_ci > 0) | (.upper_ci < 0),
      direction = case_when(
        sig & deriv > 0 ~ "increasing",
        sig & deriv < 0 ~ "decreasing",
        TRUE            ~ "ns"
      )
    )
}


results = map(outcomes, ~ fit_age_gam(.x, df = est, n_k = 7))
names(results) = outcomes

pred_all = bind_rows(map(results, "preds")) %>%
  mutate(outcome = factor(outcome, levels = outcomes))

deriv_all = map2_dfr(results, outcomes, \(res, outcome) {
  get_age_deriv(res$fit, df = est, n = 200) %>%
    mutate(outcome = outcome)
}) %>%
  mutate(sig_deriv = if_else(sig, deriv, 0),
         outcome   = factor(outcome, levels = outcomes))


outcome_cols = c(
  mean_acc_0b = "#DE1A1A",
  mean_rt_0b  = "#943CB4",
  t           = "#f0027f",
  v_0b        = "#386cb0",
  a_0b        = "#66a61e",
  z_0b        = "#FFBF00"
)

outcome_labs = c(
  mean_acc_0b = "Accuracy",
  mean_rt_0b  = "Response Time",
  t           = "Non-Decision \nTime",
  v_0b        = "Drift Rate",
  a_0b        = "Decision \nThreshold",
  z_0b        = "Bias"
)


obs_scale = age_long %>%
  group_by(outcome) %>%
  summarise(sd = sd(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(sd = ifelse(sd == 0 | is.na(sd), NA_real_, sd))

baseline_age = min(pred_all$age, na.rm = TRUE)
baseline_pred = pred_all %>%
  filter(age == baseline_age) %>%
  select(outcome, fit0 = fit)

pred_bc_z = pred_all %>%
  left_join(baseline_pred, by = "outcome") %>%
  left_join(obs_scale,     by = "outcome") %>%
  mutate(
    fit_z   = (fit   - fit0) / sd,
    lower_z = (lower - fit0) / sd,
    upper_z = (upper - fit0) / sd
  )

scaled_lines = ggplot(pred_bc_z, aes(x = age, y = fit_z, colour = outcome)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lower_z, ymax = upper_z, fill = outcome),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = outcome_cols, labels = outcome_labs) +
  scale_fill_manual  (values = outcome_cols, labels = outcome_labs) +
  labs(x = "Age (years)", y = "Change from baseline \n(z-score)", colour = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  guides(fill = "none")


# Visualize trajectories with derivative strips/rate of change

xlim_use = range(pred_all$age, na.rm = TRUE)

plot_traj_plus_continuous_strip = function(pred_df, deriv_df, raw_df,
                                           outcome_name,
                                           xlim = NULL,
                                           strip_height = 0.22,
                                           show_legend = FALSE,
                                           strip_lims = c(-1, 1)) {
  x_scale = scale_x_continuous(limits = xlim, expand = c(0, 0))
  this_col = unname(outcome_cols[[outcome_name]])
  this_lab = unname(outcome_labs[[outcome_name]])
  
  p_top =
    ggplot() +
    geom_line(data = raw_df,
              aes(x = age, y = value, group = sub_id),
              colour = this_col, linewidth = 0.5, alpha = 0.03,
              show.legend = FALSE) +
    geom_ribbon(data = pred_df,
                aes(x = age, ymin = lower, ymax = upper),
                fill = "grey70", alpha = 0.9, colour = NA,
                inherit.aes = FALSE) +
    geom_line(data = pred_df,
              aes(x = age, y = fit),
              colour = "black", linewidth = 1.3, inherit.aes = FALSE) +
    x_scale +
    labs(x = NULL, y = NULL, title = this_lab) +
    theme_minimal(base_size = 14) +
    theme(plot.title  = element_text(size = 12, hjust = 0.5),
          axis.text.x = element_blank())
  
  p_strip =
    ggplot(deriv_df, aes(x = age, y = 1, fill = sig_deriv)) +
    geom_raster(interpolate = TRUE) +
    x_scale +
    scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    scale_fill_gradient2(
      low  = "#08519c", mid = "white", high = "#a50f15",
      midpoint = 0, limits = strip_lims,
      oob = scales::squish,
      breaks = scales::pretty_breaks(n = 5)
    ) +
    labs(x = NULL, y = NULL, fill = "Growth rate\n(first derivative)") +
    theme_minimal(base_size = 14) +
    theme(axis.line.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          plot.margin  = margin(0, 12, 6, 12),
          legend.position = if (show_legend) "bottom" else "none",
          legend.title = element_text(size = 10),
          legend.text  = element_text(size = 9)) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 title.position = "left",
                                 barwidth  = unit(5,    "cm"),
                                 barheight = unit(0.35, "cm")))
  
  p_top / p_strip + plot_layout(heights = c(1, strip_height))
}

plots = map(outcomes, \(o) {
  pred_one  = pred_all  %>% filter(outcome == o)
  deriv_one = deriv_all %>% filter(outcome == o)
  raw_one   = age_long  %>% filter(outcome == o)
  
  local_mx   = max(abs(deriv_one$sig_deriv), na.rm = TRUE)
  local_lims = c(-local_mx, local_mx)
  
  plot_traj_plus_continuous_strip(
    pred_df = pred_one, deriv_df = deriv_one, raw_df = raw_one,
    outcome_name = o, xlim = xlim_use,
    strip_height = 0.05, show_legend = FALSE,
    strip_lims = local_lims
  )
})

fig3 = wrap_plots(plots, ncol = 3) +
  theme(plot.margin = margin(6, 12, 6, 12))  +
  plot_annotation(title = "0-Back",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))



ggsave("age_trajectories_with_deriv_0-BACK_rev1.png", fig3,
       height = 7, width = 7, units = "in", dpi = 500)
ggsave("age_scaled_trajectories__0-BACK_rev1.png", scaled_lines,
       height = 6, width = 5, units = "in", dpi = 500)
