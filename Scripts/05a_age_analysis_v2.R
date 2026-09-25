# Age analysis script along with visualization of trajectories
# ERF, 2026.08.20
# Adapted to use gamm() from mgcv instead of gamm4()
#
# Pipeline:
#  ----> Fit s(age) GAMM for each outcome with random intercept for sub_id (REML).
#  ----> Predict population-level age trajectories from fit$gam.
#  ----> Compute first derivatives of the age smooth with simultaneous CIs.
#  ----> Plots: (A) z-scored baseline-corrected trajectories on a common scale,
#               (B) trajectories with derivative strips underneath.
#  ----> Supplementary table: EDF/F/p_FDR per outcome.

# R/4.4.2-gfbf-2024a

library(tidyverse)
library(patchwork)
library(gratia)
library(mgcv)
library(scales)
library(flextable)

# import 
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


# fitting and prediction function
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


# derivatives function
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


# Run gamm model on each outcome 
results = map(outcomes, ~ fit_age_gam(.x, df = est, n_k = 7))
names(results) = outcomes

pred_all = bind_rows(map(results, "preds")) %>%
  mutate(outcome = factor(outcome, levels = outcomes))

# get the derivatives for visualization of significant changes 
deriv_all = map2_dfr(results, outcomes, \(res, outcome) {
  get_age_deriv(res$fit, df = est, n = 200) %>%
    mutate(outcome = outcome)
}) %>%
  mutate(sig_deriv = if_else(sig, deriv, 0),
         outcome   = factor(outcome, levels = outcomes))


#
## Visualize
#

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

# Figure 2A
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


# Figure 2B
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
  theme(plot.margin = margin(6, 12, 6, 12))


# Save figures
ggsave("age_trajectories_with_deriv_unscaled_rev1.png", fig3,
       height = 7, width = 7, units = "in", dpi = 500)
ggsave("age_scaled_trajectories_rev1.png", scaled_lines,
       height = 6, width = 5, units = "in", dpi = 500)



# Table for supplementary 
extract_results_age_table = function(results, smooth_var = "age",
                                     p_adjust_method = "BH") {
  results %>% map_df(function(res) {
    sm = summary(res$fit$gam)
    outcome = unique(res$preds$outcome)[1]
    
    as.data.frame(sm$s.table) %>%
      rownames_to_column("term") %>%
      filter(str_detect(term, paste0("s\\(", smooth_var, "\\)"))) %>%
      slice(1) %>%
      transmute(outcome = outcome,
                edf_age = round(edf, 2),
                F_age   = round(F, 2),
                p_age   = `p-value`)
  }) %>%
    mutate(
      p_age_fdr = p.adjust(p_age, method = p_adjust_method),
      p_age_fdr_disp = ifelse(p_age_fdr < 0.001,
                              "<0.001",
                              format(round(p_age_fdr, 3), nsmall = 3))
    ) %>%
    select(outcome, edf_age, F_age, p_age_fdr_disp)
}

results_table = extract_results_age_table(results)

write_csv(results_table, "gamm_age_resultsTable_rev1.csv")
save_as_html(flextable(results_table), path = "gamm_age_resultsTable_rev1.html")
