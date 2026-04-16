library(tidyverse)

## paths
dir_bl  = ".../abcd_nback_bl_data/uncleaned"
dir_2y  = ".../abcd_nback_2y_data/uncleaned"
dir_4y  = ".../abcd_nback_4y_data/uncleaned"
dir_6y  = ".../abcd_nback_6y_data/uncleaned"

# make_df = function(dir_path) {
#   files = list.files(dir_path, pattern = "\\.csv$", full.names = FALSE)
#
#   if (length(files) == 0) {
#     return(data.frame(file_name = character(),
#                       sub_id    = character(),
#                       stringsAsFactors = FALSE))
#   }
#
#   
#   sub_id = sub("(_ses-.*)$", "", files)  # everything before _ses-
#
#   data.frame(
#     file_name = files,
#     sub_id    = sub_id,
#     stringsAsFactors = FALSE
#   )
# }
#
# df_bl = make_df(dir_bl) %>% mutate(tp_year = 0)
# df_2y = make_df(dir_2y) %>% mutate(tp_year = 2)
# df_4y = make_df(dir_4y) %>% mutate(tp_year = 4)
# df_6y = make_df(dir_6y) %>% mutate(tp_year = 6)
#
# ## combine all in one df
# df_all = bind_rows(df_bl, df_2y, df_4y, df_6y)
#
# ## find subjects with nback data from at least 3 time points
# subs_3plus = df_all %>%
#   group_by(sub_id) %>%
#   summarise(n_tp_year = n_distinct(tp_year)) %>%
#   filter(n_tp_year >= 3) %>%
#   pull(sub_id)
#
# ## keep only those subjects
# df_3plus = df_all %>%
#   filter(sub_id %in% subs_3plus) %>%
#   select(file_name, sub_id, tp_year)

#write_csv(df_3plus, "sub_files_overview_n5307.csv")

df_3plus = read_csv("sub_files_overview_n5307.csv")



################################################################################
#
    # Make new subject ID's that are compatible with dockerHDDM
#

#new_ids = sample(1000:9999, length(subs_3plus), replace = F)

#id_key = data.frame(subs_3plus) %>%
#  rename(sub_id = "subs_3plus") 
#id_key$new_ids = new_ids
# 
# df_3plus = df_3plus %>% 
#   left_join(., id_key, by = "sub_id") %>%
#   mutate(tp_num = case_when(tp_year == 0 ~ 0,
#                             tp_year == 2 ~ 220000,
#                             tp_year == 4 ~ 440000,
#                             tp_year == 6 ~ 660000),
#          subj_idx = new_ids + tp_num) 


################################################################################
#
    # Clean N-back files: baseline
#

files_bl = df_3plus %>% 
  filter(tp_year == 0) %>% 
  select(file_name, sub_id, subj_idx)

dest_bl = ".../abcd_nback_bl_data/"

acc = data.frame()
data = tibble()
flagged_resp = ""
flagged_acc = ""

for (i in seq(1, nrow(files_bl))) {
  cat(i)
  cat('\n')
  subj_files = files_bl$file_name[i]
  
  temp = read_delim(paste0(dir_bl, "/", subj_files))
  
  # Rename columns and fix the response-variable
  temp = temp %>%
    mutate(trial = ifelse(!is.na(TargetType),
                          cumsum(!is.na(TargetType)),
                          NA)) %>% 
    drop_na(TargetType) %>% 
    mutate(rt = Stim.RT/1000,
           subject = files_bl$sub_id[i],
           subj_idx = files_bl$subj_idx[i],
           eventname = "ses-00A",
           accuracy = Stim.ACC) %>%
    mutate(response = case_when(
      TargetType == 'target' & accuracy == 1 ~ 1,
      TargetType == 'target' & accuracy == 0 ~ 0,
      TargetType != 'target' & accuracy == 1 ~ 0,
      TargetType != 'target' & accuracy == 0 ~ 1
    )) %>% 
    select(subject, subj_idx, eventname, Stim.RESP, trial,
           response, accuracy, rt, BlockType, TargetType, StimType) 
  
  # Check if there are at least 50 recorded responses
  if (sum(is.na(temp$Stim.RESP)) > 50) {
    cat(paste0("FLAG: ", unique(temp$subject), " over 50 missing responses!"))
    flagged_resp = rbind(flagged_resp, unique(temp$subject)) #n = 
    next
  }
  
  temp = temp %>% 
  drop_na(Stim.RESP) %>%            # Throw out trials with no response. 
  select(-c(Stim.RESP)) %>%         # These trials are coded as "incorrect" responses - misleading.
  filter(rt > 0.2)
  
  # Check that task accuracy is >= 60%
  temp_acc = temp %>% 
    summarise(
      acc_mean = mean(accuracy, na.rm = T)
    )
  if (temp_acc$acc_mean < 0.6) {
    cat(paste0("FLAG: ", unique(temp$subj_idx), " accuracy < 60%!"))
    flagged_acc = rbind(flagged_acc, unique(temp$subject)) 
    next
  }

  
  write_csv(temp, paste0(dest_bl,
                         unique(temp$subject),
                         "_ses-00A_nback_OK.csv"))
  
  data = bind_rows(data, temp)
}



write_csv(data, "nback_obs_data_bl_n5307.csv")
flagged_acc_df = data.frame(flagged_acc)
write_csv(flagged_acc_df, "nback_flagged_accuracy_bl.csv")
flagged_resp_df = data.frame(flagged_resp)
write_csv(flagged_resp_df, "nback_flagged_responses_bl.csv")

################################################################################
#
    # 2-year follow-up
#

files_2y = df_3plus %>% 
  filter(tp_year == 2) %>% 
  select(file_name, sub_id, subj_idx)

dest_2y = ".../abcd_nback_2y_data/"

acc = data.frame()
data = tibble()
flagged_resp = ""
flagged_acc = ""

for (i in seq(1, nrow(files_2y))) {
  cat(i)
  cat('\n')
  subj_files = files_2y$file_name[i]
  
  temp = read_delim(paste0(dir_2y, "/", subj_files))
  
  # Rename columns and fix the response-variable
  temp = temp %>%
    mutate(trial = ifelse(!is.na(TargetType),
                          cumsum(!is.na(TargetType)),
                          NA)) %>% 
    drop_na(TargetType) %>% 
    mutate(rt = Stim.RT/1000,
           subject = files_2y$sub_id[i],
           subj_idx = files_2y$subj_idx[i],
           eventname = "ses-02A",
           accuracy = Stim.ACC) %>%
    mutate(response = case_when(
      TargetType == 'target' & accuracy == 1 ~ 1,
      TargetType == 'target' & accuracy == 0 ~ 0,
      TargetType != 'target' & accuracy == 1 ~ 0,
      TargetType != 'target' & accuracy == 0 ~ 1
    )) %>% 
    select(subject, subj_idx, eventname, Stim.RESP, trial,
           response, accuracy, rt, BlockType, TargetType, StimType) 
  
  # Check if there are at least 50 recorded responses
  if (sum(is.na(temp$Stim.RESP)) > 50) {
    cat(paste0("FLAG: ", unique(temp$subject), " over 50 missing responses!"))
    flagged_resp = rbind(flagged_resp, unique(temp$subject)) 
    next
  }
  
  temp = temp %>% 
    drop_na(Stim.RESP) %>%          
    select(-c(Stim.RESP)) %>%       
    filter(rt > 0.2)
  
  # Check that task accuracy is >= 60%
  temp_acc = temp %>% 
    summarise(
      acc_mean = mean(accuracy, na.rm = T)
    )
  if (temp_acc$acc_mean < 0.6) {
    cat(paste0("FLAG: ", unique(temp$subj_idx), " accuracy < 60%!"))
    flagged_acc = rbind(flagged_acc, unique(temp$subject)) 
    next
  }
  
  
  write_csv(temp, paste0(dest_2y,
                         unique(temp$subject),
                         "_ses-02A_nback_OK.csv"))
  
  data = bind_rows(data, temp)
}



write_csv(data, "nback_obs_data_2y_n5307.csv")
flagged_acc_df = data.frame(flagged_acc)
write_csv(flagged_acc_df, "nback_flagged_accuracy_2y.csv")
flagged_resp_df = data.frame(flagged_resp)
write_csv(flagged_resp_df, "nback_flagged_responses_2y.csv")

################################################################################
#
  # 4-year follow-up
#

files_4y = df_3plus %>% 
  filter(tp_year == 4) %>% 
  select(file_name, sub_id, subj_idx)

dest_4y = ".../abcd_nback_4y_data/"

acc = data.frame()
data = tibble()
flagged_resp = ""
flagged_acc = ""

for (i in seq(1, nrow(files_4y))) {
  cat(i)
  cat('\n')
  subj_files = files_4y$file_name[i]
  
  temp = read_delim(paste0(dir_4y, "/", subj_files))
  
  # Rename columns and fix the response-variable
  temp = temp %>%
    mutate(trial = ifelse(!is.na(TargetType),
                          cumsum(!is.na(TargetType)),
                          NA)) %>% 
    drop_na(TargetType) %>% 
    mutate(rt = Stim.RT/1000,
           subject = files_4y$sub_id[i],
           subj_idx = files_4y$subj_idx[i],
           eventname = "ses-04A",
           accuracy = Stim.ACC) %>%
    mutate(response = case_when(
      TargetType == 'target' & accuracy == 1 ~ 1,
      TargetType == 'target' & accuracy == 0 ~ 0,
      TargetType != 'target' & accuracy == 1 ~ 0,
      TargetType != 'target' & accuracy == 0 ~ 1
    )) %>% 
    select(subject, subj_idx, eventname, Stim.RESP, trial,
           response, accuracy, rt, BlockType, TargetType, StimType) 
  
  # Check if there are at least 50 recorded responses
  if (sum(is.na(temp$Stim.RESP)) > 50) {
    cat(paste0("FLAG: ", unique(temp$subject), " over 50 missing responses!"))
    flagged_resp = rbind(flagged_resp, unique(temp$subject)) 
    next
  }
  
  temp = temp %>% 
    drop_na(Stim.RESP) %>%            
    select(-c(Stim.RESP)) %>%        
    filter(rt > 0.2)
  
  # Check that task accuracy is >= 60%
  temp_acc = temp %>% 
    summarise(
      acc_mean = mean(accuracy, na.rm = T)
    )
  if (temp_acc$acc_mean < 0.6) {
    cat(paste0("FLAG: ", unique(temp$subj_idx), " accuracy < 60%!"))
    flagged_acc = rbind(flagged_acc, unique(temp$subject)) 
    next
  }
  
  
  write_csv(temp, paste0(dest_4y,
                         unique(temp$subject),
                         "_ses-04A_nback_OK.csv"))
  
  data = bind_rows(data, temp)
}



write_csv(data, "nback_obs_data_4y_n5307.csv")
flagged_acc_df = data.frame(flagged_acc)
write_csv(flagged_acc_df, "nback_flagged_accuracy_4y.csv")
flagged_resp_df = data.frame(flagged_resp)
write_csv(flagged_resp_df, "nback_flagged_responses_4y.csv")

################################################################################
#
  # 6-year follow-up
#

files_6y = df_3plus %>% 
  filter(tp_year == 6) %>% 
  select(file_name, sub_id, subj_idx)

dest_6y = ".../abcd_nback_6y_data/"

acc = data.frame()
data = tibble()
flagged_resp = ""
flagged_acc = ""

for (i in seq(1, nrow(files_6y))) {
  cat(i)
  cat('\n')
  subj_files = files_6y$file_name[i]
  
  temp = read_delim(paste0(dir_6y, "/", subj_files))
  
  # Rename columns and fix the response-variable
  temp = temp %>%
    mutate(trial = ifelse(!is.na(TargetType),
                          cumsum(!is.na(TargetType)),
                          NA)) %>% 
    drop_na(TargetType) %>% 
    mutate(rt = Stim.RT/1000,
           subject = files_6y$sub_id[i],
           subj_idx = files_6y$subj_idx[i],
           eventname = "ses-06A",
           accuracy = Stim.ACC) %>%
    mutate(response = case_when(
      TargetType == 'target' & accuracy == 1 ~ 1,
      TargetType == 'target' & accuracy == 0 ~ 0,
      TargetType != 'target' & accuracy == 1 ~ 0,
      TargetType != 'target' & accuracy == 0 ~ 1
    )) %>% 
    select(subject, subj_idx, eventname, Stim.RESP, trial,
           response, accuracy, rt, BlockType, TargetType, StimType) 
  
  # Check if there are at least 50 recorded responses
  if (sum(is.na(temp$Stim.RESP)) > 50) {
    cat(paste0("FLAG: ", unique(temp$subject), " over 50 missing responses!"))
    flagged_resp = rbind(flagged_resp, unique(temp$subject)) 
    next
  }
  
  temp = temp %>% 
    drop_na(Stim.RESP) %>%            
    select(-c(Stim.RESP)) %>%        
    filter(rt > 0.2)
  
  # Check that task accuracy is >= 60%
  temp_acc = temp %>% 
    summarise(
      acc_mean = mean(accuracy, na.rm = T)
    )
  if (temp_acc$acc_mean < 0.6) {
    cat(paste0("FLAG: ", unique(temp$subj_idx), " accuracy < 60%!"))
    flagged_acc = rbind(flagged_acc, unique(temp$subject))  
    next
  }
  
  
  write_csv(temp, paste0(dest_6y,
                         unique(temp$subject),
                         "_ses-06A_nback_OK.csv"))
  
  data = bind_rows(data, temp)
}



write_csv(data, "nback_obs_data_6y_n5307.csv")
flagged_acc_df = data.frame(flagged_acc)
write_csv(flagged_acc_df, "nback_flagged_accuracy_6y.csv")
flagged_resp_df = data.frame(flagged_resp)
write_csv(flagged_resp_df, "nback_flagged_responses_6y.csv")