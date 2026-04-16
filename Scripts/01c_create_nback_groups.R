# Make groups for HDDM modeling
# ERF 2025-04-15
# Edited for 4TPs with groups consisting of same subjects across TPs
# ERF 2025-11-13

library(tidyverse)


set.seed(1104)


G = 90 # number of groups

################################################################################
#
# Import clean nback data from the four timepoints


nback_bl = read_csv("nback_obs_data_bl_n5307.csv") 
nback_2y = read_csv("nback_obs_data_2y_n5307.csv") 
nback_4y = read_csv("nback_obs_data_4y_n5307.csv")
nback_6y = read_csv("nback_obs_data_6y_n5307.csv")

all_nback = bind_rows(nback_bl, nback_2y, nback_4y, nback_6y) 

keepers = read_csv("ids_covariates_uniquefamids_drop_6_1_2026_02_01.csv")

ids_all_data = read_csv("sub_files_overview_n5307.csv") %>% 
  filter(subj_idx %in% all_nback$subj_idx) %>%              # only clean datasets 
  filter(sub_id %in% keepers$sub_id)                        # no siblings
  
ids = ids_all_data %>% 
  group_by(sub_id) %>% 
  summarise(n_tps = n()) %>% 
  ungroup()



################################################################################
#

# Split 4s and 3s; shuffle within each

four_tp = ids %>% 
  filter(n_tps == 4) %>% 
  slice_sample(prop = 1)

three_tp = ids %>% 
  filter(n_tps == 3) %>% 
  slice_sample(prop = 1)

n4 = nrow(four_tp)
n3 = nrow(three_tp)


# Initialize containers

groups = vector("list", G)

group_sums = rep(0L, G) # sum of n_tps per group
group_n4 = rep(0L, G)   # how many 4tp subs in each group


#
    # Spread 4 TP subs as evenly as possible
#

base4 = n4 %/% G
extra4 = n4 %% G
target4 = rep(base4, G)

if (extra4 > 0) target4[seq_len(extra4)] = target4[seq_len(extra4)] + 1


# Assign the 4TP subs

i = 1

for (row_idx in seq_len(n4)) {
  # find groups that "need" a 4-tp sub
  needers = which(group_n4 < target4)
  
  # among them, pick the one with the smallest current sum of datasets
  g = needers[which.min(group_sums[needers])]
  
  # place subject
  if (is.null(groups[[g]])) groups[[g]] = four_tp[row_idx, ,drop = F]
  else groups[[g]] = bind_rows(groups[[g]], four_tp[row_idx, ,drop = F])
  group_sums[g] = group_sums[g] + 4L
  group_n4[g] = group_n4[g] + 1L
  i = i + 1
  
}


# 
    # Greedily place 3-tp subs to balance total datasets
#

for (row_idx in seq_len(n3)) {
  # choose the group with the smallest current sum of datasets
  g = which.min(group_sums)
  if (is.null(groups[[g]])) groups[[g]] = three_tp[row_idx, , drop = FALSE]
  else groups[[g]] = bind_rows(groups[[g]], three_tp[row_idx, , drop = FALSE])
  group_sums[g] = group_sums[g] + 3L
}

# Bind back and add the label
df_grouped = bind_rows(
  map2(groups, seq_len(G), ~ mutate(.x, sample_group = .y))
)


summary_by_group = df_grouped %>%
  group_by(sample_group) %>%
  summarise(
    n_ids   = n(),
    n_3tp   = sum(n_tps == 3),
    n_4tp   = sum(n_tps == 4),
    sum_tps = sum(n_tps),
    .groups = "drop"
  )


id_group_overview = df_grouped %>% select(sub_id, sample_group)
# 
ids = left_join(ids, id_group_overview, by = "sub_id")
ids_all_data = left_join(ids_all_data, ids, by = "sub_id") %>% 
  drop_na(sample_group)
write_csv(ids_all_data, "sub_files_overview_drop_6_1.csv")

################################################################################
#
    #
#
#### Split the all_nback-dataframe into groups and write csv

all_nback = all_nback %>% 
  filter(subject %in% id_group_overview$sub_id)

length(unique(all_nback$subject)) #n = 4201
id_group_overview = id_group_overview %>% 
  rename(subject = "sub_id")

all_nback = left_join(all_nback, id_group_overview, by = "subject")


for (i in 1:G) {
  cat(paste0('Group: ', i, ' of ', G))
  cat('\n')
  
  cat(paste0('Number of unique participants in group: ', length(unique(all_nback$subject[all_nback$sample_group == i]))))
  cat(paste0('\nNumber of datasets in group: ', length(unique(all_nback$subj_idx[all_nback$sample_group == i]))))
  
  temp_data = all_nback[all_nback$sample_group == i,]
  
  cat(paste0('Writing csv for group: ', i))
  cat('\n')
  
  write_csv(temp_data, paste0('.../abcd_nback_grouped_data/nback_xtps_group_drop_6_1_', i, '.csv'))
}
