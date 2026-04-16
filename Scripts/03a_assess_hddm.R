# Check HDDM traces and convergence
#
# ERF 

library(tidyverse)
library(RColorBrewer)


###
  # Convergence
###


stats_path = ".../stats"
stats_files = dir(stats_path, recursive = T, pattern = "stats_drop_6_1_group_*")

stats = tibble()

for (i in seq_along(stats_files)) {

  temp = read_csv(paste0(stats_path, "/", stats_files[i]))

  group_num = str_extract(stats_files[i], "(?<=stats_drop_6_1_group_)\\d+(?=\\.csv)")

  temp = temp %>% mutate(group = group_num)

  stats = bind_rows(stats, temp)

}

stats = stats %>% rename(param = 1)

t= stats %>%
  filter(r_hat >= 1.1) 


# ###
#   # Traces
# ###


real_ids = read_csv("sub_files_overview_drop_6_1.csv") %>%
  select(sub_id, subj_idx)

params_group = stats %>%
  filter(!str_detect(param, "subj"))
# 
# params_sub = stats %>%
#   filter(str_detect(param, "subj")) %>%
#   select(param, mean, group) %>%
#   mutate(subj_idx = as.numeric(sub(".*\\.", "", param)),
#          TP = case_when(subj_idx < 190000 ~ 0,
#                         subj_idx >= 220000 & subj_idx < 230000 ~ 2,
#                         subj_idx >= 440000 & subj_idx < 450000 ~ 4,
#                         subj_idx >= 660000 & subj_idx < 670000 ~ 6),
#          param_name = param %>%
#            sub("\\.[0-9]+$", "", .) %>%
#            sub("subj", "", .),
#          param_group = case_when(
#            str_starts(param_name, "v") ~ "v",
#            str_starts(param_name, "a") ~ "a",
#            str_starts(param_name, "z") ~ "z",
#            str_starts(param_name, "t") ~ "t",
#            TRUE ~ NA_character_
#          ),
#          BlockType = case_when(
#            str_detect(param_name, "0-Back") ~ "0b",
#            str_detect(param_name, "2-Back") ~ "2b",
#            TRUE ~ NA_character_
#          ),
#          TargetType = case_when(
#            str_detect(param_name, "lure") ~ "lure",
#            str_detect(param_name, "nonlure") ~ "nonlure",
#            str_detect(param_name, "target") ~ "target",
#            TRUE ~ NA_character_
#          )
#   ) %>%
#   rename(value = "mean") %>%
#   left_join(., real_ids, by = "subj_idx")
# 
# write_csv(params_sub, "params_sub_long_drop_6_1.csv")

params_sub = read_csv("params_sub_long_drop_6_1.csv") %>% 
  mutate(TP = case_when(TP == 0 ~ "ses-00A",
                        TP == 2 ~ "ses-02A",
                        TP == 4 ~ "ses-04A",
                        TP == 6 ~ "ses-06A"))



estimates_wide = params_sub %>% 
  select(sub_id, TP, param_name, value) %>% 
  pivot_wider(names_from = param_name, 
              values_from = value) %>% 
  rename(a_0b = "a_(0-Back)",
         a_2b = "a_(2-Back)",
         v_0b_lure = "v_(0-Back.lure)",
         v_0b_nonlure = "v_(0-Back.nonlure)",
         v_0b_target = "v_(0-Back.target)",
         v_2b_lure = "v_(2-Back.lure)",
         v_2b_nonlure = "v_(2-Back.nonlure)",
         v_2b_target = "v_(2-Back.target)",
         t = "t_",
         z_0b = "z_(0-Back)",
         z_2b = "z_(2-Back)") 


### Clean the estimate data
#   Remove participants that had parameter estimates resulting in NAs
#   Make estimates >< 4 SDs from the group mean into NA 
estimates_wide_clean = estimates_wide %>% 
  drop_na(c(a_0b, a_2b, v_0b_lure, v_0b_nonlure,
            v_0b_target, v_2b_lure, v_2b_nonlure,
            v_2b_target, t, z_0b, z_2b)) %>% 
  group_by(TP) %>% 
  mutate(
    across(
      c(a_0b, a_2b, v_0b_lure, v_0b_nonlure,
        v_0b_target, v_2b_lure, v_2b_nonlure,
        v_2b_target, t, z_0b, z_2b),
      ~ ifelse(
        abs(.x - mean(.x, na.rm = TRUE)) > 4 * sd(.x, na.rm = TRUE),
        NA,
        .x
      )
    )
  ) %>% 
  ungroup()


# How many outliers were identified and set to NA
length(which(is.na(estimates_wide_clean)))                    
only_na = estimates_wide_clean[!complete.cases(estimates_wide_clean),]
nrow(only_na)                                                 
length(unique(only_na$sub_id))                                


estimates_means = estimates_wide_clean %>%  
  mutate(v_0b_lure = v_0b_lure * -1,
         v_2b_lure = v_2b_lure * -1,
         v_0b_nonlure = v_0b_nonlure * -1,
         v_2b_nonlure = v_2b_nonlure * -1) %>% 
  mutate(
    v_mean = rowMeans(select(., starts_with("v_"))), 
    a_mean = rowMeans(select(., starts_with("a_"))),
    t_mean = t,
    z_mean = rowMeans(select(., starts_with("z_")))
  ) %>% 
  select(sub_id, TP, v_mean, a_mean, t_mean, z_mean)


df_all = estimates_means


estimates_long_clean = estimates_wide_clean %>% 
  pivot_longer(
    cols = c(a_0b, a_2b, v_0b_lure, 
             v_0b_nonlure, v_0b_target, 
             v_2b_lure, v_2b_nonlure, 
             v_2b_target, t, 
             z_0b, z_2b)
  )

means = estimates_long_clean %>% 
  group_by(name) %>% 
  summarise(mean_value = mean(value, na.rm = T))

estimates_histograms = estimates_long_clean %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color ="black") +
  geom_vline(data = means, aes(xintercept = mean_value),
             linetype = "dashed", color = "red", linewidth = 0.75) +
  facet_wrap(~ name, scales = "free", ncol = 3) +
  theme_minimal(base_size = 14)


#### 
    # Saving stuff 
####

write_csv(df_all, "param_means_clean_drop_6_1.csv")
ggsave("params_histograms_drop_6_1.png", estimates_histograms,
       width = 9, height = 9, units = "in")

