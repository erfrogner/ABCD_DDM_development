# Assess and visualize parameter recovery.  
# ERF 

library(tidyverse)
library(patchwork)


setwd(".../param_rec")
files = dir(recursive = T, pattern = "stats_group_*")


par_rec = tibble()

for (i in seq_along(files)) {
  
  temp = read_csv(files[i])
  
  group_num = str_extract(files[i], "(?<=stats_group_)\\d+(?=\\.csv)")
  
  
  
  temp = temp %>% mutate(group = group_num)
  
  par_rec = bind_rows(par_rec, temp)
  
}

par_rec = par_rec %>% rename(param = 1)


real_ids = read_csv("sub_files_overview_drop_6_1.csv") %>% select(sub_id, subj_idx)

params_sub = par_rec %>%
  filter(str_detect(param, "subj")) %>%
  select(param, mean, r_hat, group) %>%
  mutate(subj_idx = as.numeric(sub(".*\\.", "", param)),
         TP = case_when(subj_idx < 190000 ~ 0,
                        subj_idx >= 220000 & subj_idx < 230000 ~ 2,
                        subj_idx >= 440000 & subj_idx < 450000 ~ 4,
                        subj_idx >= 660000 & subj_idx < 670000 ~ 6),
         param_name = param %>%
           sub("\\.[0-9]+$", "", .) %>%
           sub("subj", "", .),
         param_group = case_when(
           str_starts(param_name, "v") ~ "v",
           str_starts(param_name, "a") ~ "a",
           str_starts(param_name, "z") ~ "z",
           str_starts(param_name, "t") ~ "t",
           TRUE ~ NA_character_
         ),
         BlockType = case_when(
           str_detect(param_name, "0-Back") ~ "0b",
           str_detect(param_name, "2-Back") ~ "2b",
           TRUE ~ NA_character_
         ),
         TargetType = case_when(
           str_detect(param_name, "lure") ~ "lure",
           str_detect(param_name, "nonlure") ~ "nonlure",
           str_detect(param_name, "target") ~ "target",
           TRUE ~ NA_character_
         )
  ) %>%
  rename(value = "mean") %>%
  left_join(., real_ids, by = "subj_idx")


param_rec = params_sub %>% 
  select(subj_idx, param_name, value, r_hat) %>% 
  rename(rec_value = "value",
         rec_rhat = "r_hat")

param = read_csv("params_sub_long_drop_6_1.csv") %>% 
  filter(subj_idx %in% params_sub$subj_idx) %>% 
  select(subj_idx, param_name, value) %>% 
  rename(obs_value = "value") %>% 
  left_join(., param_rec, by = c("subj_idx", "param_name")) %>% 
  drop_na(rec_value)

# Set names
param = param %>%
  mutate(
    param_name = case_when(
      param_name == "t_" ~ "Non-Decision Time",
      param_name == "z_(0-Back)" ~ "Bias (0-back)",
      param_name == "z_(2-Back)" ~ "Bias (2-back)",
      param_name == "a_(0-Back)" ~ "Threshold (0-back)",
      param_name == "a_(2-Back)" ~ "Threshold (2-back)",
      param_name == "v_(0-Back.lure)" ~ "Drift (Lure 0-back)",
      param_name == "v_(0-Back.nonlure)" ~ "Drift (Nonlure 0-back)",
      param_name == "v_(0-Back.target)" ~ "Drift (Target 0-back)",
      param_name == "v_(2-Back.lure)" ~ "Drift (Lure 2-back)",
      param_name == "v_(2-Back.nonlure)" ~ "Drift (Nonlure 2-back)",
      param_name == "v_(2-Back.target)" ~ "Drift (Target 2-back)",
      TRUE ~ param_name
    )
  )

# Calculate spearmans rho
rho_df = param %>%
  group_by(param_name) %>%
  summarise(
    rho = cor(obs_value, rec_value, method = "spearman"),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("Spearman \u03C1 = %.2f", rho))

lims_df = param %>%
  group_by(param_name) %>%
  summarise(
    lo = min(c(obs_value, rec_value), na.rm = TRUE),
    hi = max(c(obs_value, rec_value), na.rm = TRUE),
    .groups = "drop"
  )

par_rec_fig = ggplot(param, aes(x = obs_value, y = rec_value)) +
  geom_blank(data = lims_df, aes(x = lo, y = lo), inherit.aes = FALSE) +
  geom_blank(data = lims_df, aes(x = hi, y = hi), inherit.aes = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.3) +
  geom_text(
    data = rho_df,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1, vjust = 2.2,
    size = 3.6
  ) +
  facet_wrap(~ param_name, scales = "free") +
  labs(x = "Estimated", y = "Recovered") + 
  theme_bw(base_size = 12) + 
  theme(strip.text = element_text(size = 10, face = "bold"))  
  
ggsave("parameter_recovery.png", par_rec_fig, 
       width = 9.2, height = 8.5, units = "in")

