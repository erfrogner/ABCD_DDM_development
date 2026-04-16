# Import PPC from dockerHDDM script and visualize results. 
# ERF 

library(tidyverse)
library(RColorBrewer)
library(patchwork)


# files = dir(recursive = T, pattern = "ppc_xtps_drop_6_1_group*")
# 
# ppc_all = tibble()
# 
# for (i in 1:length(files)) {
# 
#   temp = read_csv(files[i])
# 
#   temp = temp %>%
#     mutate(acc_sim = ifelse(TargetType =='target', response_sim, 1-response_sim),
#            acc_obs = ifelse(TargetType == 'target', response_obs, 1-response_obs),
#            rt_obs = abs(rt_obs)) %>%
#     select(-1)
# 
#   ppc_all = bind_rows(ppc_all, temp)
# }
# 
# ppc_all_tp = ppc_all %>%
#   mutate(TP = case_when(subj_idx < 190000 ~ 0,
#                         subj_idx >= 220000 & subj_idx < 230000 ~ 2,
#                         subj_idx >= 440000 & subj_idx < 450000 ~ 4,
#                         subj_idx >= 660000 & subj_idx < 670000 ~ 6))
# 
# 
# #write_csv(ppc_all_tp, "ppc_xtps_ALL_drop_6_1.csv")

ppc_all = read_csv(".../ppc/ppc_xtps_ALL_drop_6_1.csv")

ppc_all = ppc_all %>% 
  mutate(BlockType = ifelse(BlockType == "2-Back", "2-back", "0-back"),
         TargetType = case_when(TargetType == "lure" ~ "Lure",
                                TargetType == "nonlure" ~ "Nonlure",
                                TargetType == "target" ~ "Target"))

cols_both = c(Observed = "#8c510a", Predicted = "#2A297A")


acc_ppc = ppc_all %>% 
  group_by(BlockType, TargetType, subj_idx) %>% 
  summarise(
    Observed = mean(acc_obs), 
    Predicted = mean(acc_sim)) %>% 
  pivot_longer(c(Observed, Predicted), names_to = 'type', values_to = 'accuracy_subj') %>% 
  group_by(TargetType, BlockType, type) %>% 
  summarise(accuracy = mean(accuracy_subj), sd = sd(accuracy_subj)) %>% 
  ggplot(aes(TargetType, accuracy, color = type, shape = type)) +
  geom_pointrange(aes(ymin = accuracy -sd, ymax = accuracy + sd), 
                  position = position_dodge(width = 0.55),
                  size = 0.8) +
  facet_grid( ~ BlockType) +
  ylab("Accuracy") +
  xlab(" ") + 
  labs(color = NULL, shape = NULL) + 
  scale_color_manual(values = cols_both) +
  scale_shape_manual(values = c(Observed = 16, Predicted = 18)) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(size=12)) +
  theme(legend.position = "none")

rt_ppc = ppc_all %>% 
  group_by(TargetType,BlockType,subj_idx) %>%
  summarise(Observed = mean(rt_obs),Predicted = mean(rt_sim)) %>%
  pivot_longer(c(Observed,Predicted),names_to='type',values_to='rt_subj') %>%
  group_by(TargetType,BlockType,type) %>%
  summarise(rt_mean = mean(rt_subj),sd = sd(rt_subj)) %>%
  ggplot(aes(TargetType,rt_mean,color=type, shape = type)) + 
  geom_pointrange(aes(ymin= rt_mean-sd,ymax = rt_mean+sd),
                  position=position_dodge(width=0.55),
                  size = 0.8) + 
  facet_grid(~ BlockType) + 
  ylab("RT") +
  xlab(" ") + 
  labs(color = NULL, shape = NULL) + 
  scale_color_manual(values = cols_both) +
  scale_shape_manual(values = c(Observed = 16, Predicted = 18)) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(size=12))


ppc_both = (acc_ppc | rt_ppc) +
  plot_layout(guides = "collect", widths = c(1,1))


###
####
##### Part two: group parameters 
####
###

pg = read_csv("group_params_figure_1.csv")

pd = position_dodge(width = 0.55)

pg$facet = gsub("\u2013|\u2014|\u2212", "-", pg$facet) # avoid the font issue with "-"
pg$x_lab = gsub("\u2013|\u2014|\u2212", "-", pg$x_lab)

pg = pg %>% 
  mutate(condition = case_when(condition == "0-Back" ~ "0-back",
                               condition == "2-Back" ~ "2-back",
                               condition == "All" ~ "Combined"),
         across(c(mean, lower, upper),
                ~ ifelse(facet == "Drift Rate" & x_lab %in% c("Lure", "Nonlure"), #Flip lure and nonlure drifts to positive
                         abs(.x), .x)))

group_params = ggplot(pg, aes(x = x_lab, y = mean, color = condition)) +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.55),
    size = 0.8
  ) +
  facet_wrap(~ facet, scales = "free", nrow = 1) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  scale_color_manual(
    name = "Condition",
    values = c(
      "0-back" = "#00A5CF",
      "2-back" = "#DE1A1A",
      "Combined"    = "#000000")) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "plain")
  ) 

figure = ppc_both / group_params


ggsave("ppc_xtps.png", ppc_both, height = 6, width = 13, units = "in")

ggsave("ppc_group_params_xtps.png", figure, width = 11, height = 8, units = "in")
  

