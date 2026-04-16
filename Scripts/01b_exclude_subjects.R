# Remove family relatedness
set.seed(1104)

library(tidyverse)



# Get id's
nback_bl = read_csv("nback_obs_data_bl_n5307.csv")  
nback_2y = read_csv("nback_obs_data_2y_n5307.csv") 
nback_4y = read_csv("nback_obs_data_4y_n5307.csv")
nback_6y = read_csv("nback_obs_data_6y_n5307.csv")

all_nback = bind_rows(nback_bl, nback_2y, nback_4y, nback_6y) 

ids = unique(all_nback$subject) 

##
### Family relatedness
##

path = "..."
tsv_files = data.frame(dir(path, pattern = "*.tsv", recursive = T)) %>% 
  rename(filename = 1) %>% 
  filter(!str_detect(filename, "^mr"))

#
## Get ages 
df_age = read_delim(paste0(path, "ab_g_dyn.tsv")) %>%
  filter(session_id == "ses-00A" | session_id == "ses-02A" | session_id == "ses-04A" | session_id == "ses-06A") %>% 
  select(participant_id, 
         session_id, 
         ab_g_dyn__visit_age
  ) %>%
  rename(age = ab_g_dyn__visit_age,
         TP = session_id)
#
## Get sex and family id
df_covs = read_delim(paste0(path, "ab_g_stc.tsv")) %>% 
  select(participant_id, 
         ab_g_stc__cohort_sex,                # Sex
         ab_g_stc__design_id__fam,            # Family ID
  ) %>% 
  rename(sex = "ab_g_stc__cohort_sex",       
         fam_id = "ab_g_stc__design_id__fam") %>% 
  mutate(sex = factor(ifelse(sex == 1, "M", "F"))) %>% 
  left_join(df_age, ., by = "participant_id") %>% 
  rename(sub_id = "participant_id") %>% 
  filter(sub_id %in% ids)



###############################
#
# Randomly exclude subjects from same family, preferentially keeping subs with 4 TPs 

keepers = df_covs %>%
  group_by(sub_id) %>%
  summarise(n_timepoints = n(), .groups = "drop") %>% 
  left_join(df_covs %>% distinct(sub_id, fam_id), by = "sub_id") %>% 
  group_by(fam_id) %>% 
  filter(n_timepoints == max(n_timepoints, na.rm = T)) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  pull(sub_id) 

df_covs = df_covs %>% 
  filter(sub_id %in% keepers)

write_csv(df_covs, "ids_covariates_uniquefamids_drop_6_1_2026_02_01.csv")

