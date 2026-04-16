# Get other measures and combine with parameter estimates per sub

library(tidyverse)

set.seed(1104)


##
  ## Parameter estimates
##

# estimates = read_csv("params_sub_long.csv") %>%  # n = 4844
#   mutate(TP = case_when(TP == 0 ~ "ses-00A",
#                         TP == 2 ~ "ses-02A",
#                         TP == 4 ~ "ses-04A",
#                         TP == 6 ~ "ses-06A"))

estimates = read_csv("param_means_clean_drop_6_1.csv") 
estimates = read_csv("param_wide_clean_drop_6_1.csv")  # not means

##
  ## Demographic variables
##


path = "..."
tsv_files = data.frame(dir(path, pattern = "*.tsv", recursive = T)) %>% 
  rename(filename = 1) %>% 
  filter(!str_detect(filename, "^mr"))


df_age = read_delim(paste0(path, "ab_g_dyn.tsv")) %>%
  filter(session_id == "ses-00A" | session_id == "ses-02A" | session_id == "ses-04A" | session_id == "ses-06A") %>% 
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age,
         ) %>%
  rename(age = ab_g_dyn__visit_age,
         TP = session_id)  

df_covs = read_delim(paste0(path, "ab_g_stc.tsv")) %>% 
  select(participant_id, 
         ab_g_stc__cohort_sex,                # Sex
         ) %>% 
  rename(sex = "ab_g_stc__cohort_sex") %>% 
  mutate(sex = factor(ifelse(sex == 1, "M", "F"))) %>% 
  left_join(df_age, ., by = "participant_id") %>% 
  rename(sub_id = "participant_id")


################################################################################
#
#
  # Get nback data
#   
#

  

tps = c("bl", "2y", "4y", "6y")
nback = tibble()

for (i in 1:length(tps)){ 
  
  temp_nback = read_csv(paste0("nback_obs_data_", tps[i], "_n5307.csv")) %>% 
    group_by(subject) %>% 
    summarise(
      eventname = first(eventname),
      n_trials = n(),
      mean_acc_2b      = if_else(any(BlockType == "2-Back"),
                                 mean(accuracy[BlockType == "2-Back"], na.rm = TRUE),
                                 NA_real_),
      mean_acc_0b      = if_else(any(BlockType == "0-Back"),
                                 mean(accuracy[BlockType == "0-Back"], na.rm = TRUE),
                                 NA_real_),
      mean_acc  = mean(accuracy, na.rm = TRUE),
      mean_rt_0b      = if_else(any(BlockType == "0-Back"),
                                 mean(rt[BlockType == "0-Back"], na.rm = TRUE),
                                 NA_real_),
      mean_rt_2b      = if_else(any(BlockType == "2-Back"),
                                mean(rt[BlockType == "2-Back"], na.rm = TRUE),
                                NA_real_),
      mean_rt = mean(rt))
  
  nback = bind_rows(nback, temp_nback)
  
}

nback = nback %>% 
  rename(
    TP = eventname, 
    sub_id = subject) %>% 
  filter(sub_id %in% estimates$sub_id)



#
  # Join all dataframes
#

df_all = left_join(estimates, nback, by = c("sub_id", "TP"))
df_all = left_join(df_all, df_covs, by = c("sub_id", "TP"))


# 
  # Double check cleanliness 
#

# Any with less than 50 trials?
length(which(df_all$n_trials < 50)) 

# Any with only 1 measurement?
df_all %>% count(sub_id, name = "n_tp") %>% filter(n_tp == 1) 
df_all %>% count(sub_id, name = "n_tp") %>% filter(n_tp == 2) 



#
  # Turn long and write csv
#

df_all_long = df_all %>% 
  pivot_longer(cols = c("v_mean", "a_mean", 
                        "t_mean", "z_mean",
                        "v_load", "a_load",
                        "z_load",
                        "mean_acc", "rt",
                        "mean_acc_2b", "mean_acc_0b"),
               names_to = "param",
               values_to = "value")

write_csv(df_all, "estimates_covariates_ageanalyses.csv")
write_csv(df_all_long, "estimates_covariates_long.csv")

