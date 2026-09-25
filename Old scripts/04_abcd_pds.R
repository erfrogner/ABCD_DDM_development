library(tidyverse)
library(ggplot2)
library(nlme)
library(broom)
library(patchwork)
library(ggside)

# Import
est = read_csv("estimates_covariates_ageanalyses.csv") %>% 
  mutate(sex = factor(sex))
age = est %>% 
  select(TP, sub_id, age)
sex = est %>% 
  select(sub_id, sex) %>% 
  group_by(sub_id) %>% 
  summarise(sex = first(na.omit(sex)), .groups = "drop")


path = "..."

## Parent report

p_pds = read_delim(paste0(path, "ph_p_pds.tsv")) %>% 
  rename(sub_id = "participant_id",
         TP = "session_id",
         age_pds = "ph_p_pds_age") %>%
  filter(sub_id %in% est$sub_id) %>% 
  select(sub_id, TP, age_pds,
         ph_p_pds_001, ph_p_pds_002,
         ph_p_pds_003, ph_p_pds__f_001,
         ph_p_pds__f_002, ph_p_pds__m_001,
         ph_p_pds__m_002) %>%
  left_join(., age, by = c("sub_id", "TP")) %>% 
  left_join(., sex, by = "sub_id") %>%  
  mutate(ph_p_pds__f_001 = as.numeric(ph_p_pds__f_001),
         ph_p_pds__f_002 = as.numeric(ph_p_pds__f_002),
         ph_p_pds__m_001 = as.numeric(ph_p_pds__m_001),
         ph_p_pds__m_002 = as.numeric(ph_p_pds__m_002),
         across(
           starts_with("ph_p_pds"),
           ~ ifelse(.x %in% c(777, 999), NA, .x)),              # Set 777/999 responses to NA
         ph_p_pds__f_002 = case_when(ph_p_pds__f_002 == 1 ~ 4,
                                     ph_p_pds__f_002 == 0 ~ 1,
                                     TRUE ~ ph_p_pds__f_002))


# Flag responses that should be included (i.e. at least 4 item responses)
p_pds = p_pds %>%
  rowwise() %>%
  mutate(
    include = case_when(
      sex == "F" ~ as.integer(sum(!is.na(c_across(c(
        ph_p_pds_001, ph_p_pds_002, ph_p_pds_003, ph_p_pds__f_001, ph_p_pds__f_002
      )))) >= 4),
      sex == "M" ~ as.integer(sum(!is.na(c_across(c(
        ph_p_pds_001, ph_p_pds_002, ph_p_pds_003, ph_p_pds__m_001, ph_p_pds__m_002
      )))) >= 4),
      TRUE ~ 0L
    )
  ) %>%
  ungroup()  

p_pds = p_pds %>%
  rowwise() %>%
  mutate(
    pds_mean = case_when(
      include == 1 & sex == "F" ~
        mean(c_across(c(
          ph_p_pds_001,
          ph_p_pds_002,
          ph_p_pds_003,
          ph_p_pds__f_001,
          ph_p_pds__f_002
        )), na.rm = TRUE),
      
      include == 1 & sex == "M" ~
        mean(c_across(c(
          ph_p_pds_001,
          ph_p_pds_002,
          ph_p_pds_003,
          ph_p_pds__m_001,
          ph_p_pds__m_002
        )), na.rm = TRUE),
      
      TRUE ~ NA_real_
    )) %>%
  ungroup()

 

pds = p_pds %>% 
  select(sub_id, TP, age_pds, pds_mean, include, sex) 

#
  # Find regressing cases
#

regressing_ids = pds %>% 
  arrange(sub_id, TP) %>% 
  group_by(sub_id) %>% 
  mutate(pds_diff = pds_mean - lag(pds_mean)) %>% 
  summarise(
    n_declines = sum(pds_diff < 0, na.rm = T),
    .groups = "drop"
  ) %>% 
  left_join(., sex, by = "sub_id")

regressing_ids_sex = regressing_ids %>% 
  filter(n_declines ==3)

pds = pds %>% 
  filter(!sub_id %in% regressing_ids_sex$sub_id)

#write_csv(pds, "abcd_pds_scores.csv")

pds = read_csv("abcd_pds_scores.csv") %>%
  mutate(sex = factor(sex),
         TP_y = factor(case_when(TP == "ses-00A" ~ "BL",
                                 TP == "ses-01A" ~ "1-year",
                                 TP == "ses-02A" ~ "2-year",
                                 TP == "ses-03A" ~ "3-year",
                                 TP == "ses-04A" ~ "4-year",
                                 TP == "ses-05A" ~ "5-year",
                                 TP == "ses-06A" ~ "6-year"),
                       levels = c(
                         "BL",
                         "1-year",
                         "2-year",
                         "3-year",
                         "4-year",
                         "5-year",
                         "6-year"
                       )))


##
  ## Visualize distributions of PDS mean at each timepoint
##

sex_cols <- c(M = "#005B94", F = "#BBB53E")

dx <- 0.15

raw_scores = ggplot(pds, aes(x = TP_y, y = pds_mean, fill = sex, color = sex)) +
  
  geom_boxplot(
    data = subset(pds, sex == "F"),
    width = 0.18,
    outlier.shape = NA,
    alpha = 0.2,
    linewidth = 1,
    position = position_nudge(x = -dx)
  ) +
  geom_boxplot(
    data = subset(pds, sex == "M"),
    width = 0.18,
    outlier.shape = NA,
    alpha = 0.2,
    linewidth = 1,
    position = position_nudge(x =  dx)
  ) +
  
  scale_fill_manual(values = sex_cols, name = "Sex") +
  scale_color_manual(values = sex_cols, name = "Sex") +
  guides(
    color = "none",
    fill = guide_legend(
      override.aes = list(
        colour = NA,
        linewidth = 0,
        linetype = 0,
        shape = 22,
        alpha = 1))) +
  labs(x = "Time point", y = "Mean PDS") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.92, 0.16),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = NA
    )
  )


###
  #### 
###
######
#
####
######
##
###
  ### Logistic Growth Curve Modeling
##
####
#######
#####
###
##
#
###
#
#


logistic_mod = function(age, alpha, lambda){
  1+3 / (1 + exp(-alpha * (age - lambda))) # PDS = ((B0 + (B1 - B0 ))) / (1 + exp(-alpha*(age-lambda)))
                                           # B0 = 1 (lower bound for PDS); B1 = 4 (upper bound for PDS, set to 3 because B1 - B0)    
                                           # alpha = the slope of the function at midpubertal age
                                           # lambda = the age at PDS 2.5
}


# Males
males = pds %>% 
  filter(sex == "M") #%>% 

fit_males = nlme(
  pds_mean ~ logistic_mod(age_pds, alpha, lambda), 
  data = males,
  fixed = alpha + lambda ~ 1,
  random = alpha + lambda ~ 1|sub_id,
  start = c(alpha = 0.9, lambda = 13.5),  
  na.action = na.exclude, 
  control = nlmeControl(
    pnlsTol = 1e-6,         # Default = 1e-3
    maxIter = 200,          # Default = 50
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)


summary(fit_males)
VarCorr(fit_males)

# Females
females = pds %>% 
  filter(sex == "F")

fit_females = nlme(
  pds_mean ~ logistic_mod(age_pds, alpha, lambda), 
  data = females,
  fixed = alpha + lambda ~ 1,
  random = alpha + lambda ~ 1|sub_id,
  start = c(alpha = 0.9, lambda = 13.5),  
  na.action = na.exclude, 
  control = nlmeControl(
    pnlsTol = 1e-6,         
    maxIter = 200,          
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)

summary(fit_females)
VarCorr(fit_females)


##
  ## Random effects and plotting predicted lines
##

# Make predicted trajectories
make_pred_grid = function(df, fit, id_col = "sub_id", age_col = "age_pds",
                          n_age = 200) {
  
  age_range = range(df[[age_col]], na.rm = TRUE)
  age_grid = seq(age_range[1], age_range[2], length.out = n_age)
  
  ids = unique(df[[id_col]])
  
  newdata = expand.grid(
    sub_id = ids,
    age_pds = age_grid
  )
  
  # Individual-level predictions (fixed + random effects)
  newdata$y_hat_ind = as.numeric(predict(fit, newdata = newdata, level = 1))
  
  # Population predictions (fixed effects only)
  newdata$y_hat_pop = as.numeric(predict(fit, newdata = newdata, level = 0))
  
  newdata
}


# Get random effects
re_males = ranef(fit_males)
re_females = ranef(fit_females)

ind_scores_m = tibble(
  sub_id = rownames(re_males),
  pub_tempo = fixef(fit_males)["alpha"] + re_males$alpha,
  pub_timing = fixef(fit_males)["lambda"] + re_males$lambda)
hist(ind_scores_m$pub_tempo)
hist(ind_scores_m$pub_timing)

ind_scores_f = tibble(
  sub_id = rownames(re_females),
  pub_tempo = fixef(fit_females)["alpha"] + re_females$alpha,
  pub_timing = fixef(fit_females)["lambda"] + re_females$lambda 
)
hist(ind_scores_f$pub_tempo)
hist(ind_scores_f$pub_timing)

pred_males = make_pred_grid(males, fit_males)
pred_females = make_pred_grid(females, fit_females)


# Plot individual and population trajectories

lambda_pop_m = as.numeric(fixef(fit_males)["lambda"])

trajectories_pds_m = ggplot(pred_males, aes(x = (age_pds))) +
  geom_line(aes(y = y_hat_ind, group = sub_id),
            alpha = 0.08, linewidth = 0.4) +
  geom_line(aes(y = y_hat_pop),
            linewidth = 1.5, color = "#005B94") +
  geom_hline(yintercept = 2.5, linetype = 2, linewidth = 1) +
  geom_vline(xintercept = lambda_pop_m, linetype = 2, linewidth = 1, color = "#005B94") +  
  coord_cartesian(ylim = c(1, 4)) +
  labs(
    x = "Age",
    y = "Predicted PDS",
    title = "Males"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18))



lambda_pop_f = as.numeric(fixef(fit_females)["lambda"])

trajectories_pds_f = ggplot(pred_females, aes(x = (age_pds))) +
  geom_line(aes(y = y_hat_ind, group = sub_id),
            alpha = 0.08, linewidth = 0.4) +
  geom_line(aes(y = y_hat_pop),
            linewidth = 1.5, color = "#BBB53E") +
  geom_hline(yintercept = 2.5, linetype = 2, linewidth = 1) +
  geom_vline(xintercept = lambda_pop_f, linetype = 2, linewidth = 1, color = "#BBB53E") + 
  coord_cartesian(ylim = c(1, 4)) +
  labs(
    x = "Age",
    y = "Predicted PDS",
    title = "Females"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1,  size = 18))


ggsave("female_pds_trajectories.png", trajectories_pds_f,
       height = 5, width = 6, unit = "in")

ggsave("male_pds_trajectories.png", trajectories_pds_m,
       height = 5, width = 6, unit = "in")

trajectories_pds = trajectories_pds_f | trajectories_pds_m

ggsave("pds_trajectories.png", trajectories_pds,
       height = 5, width = 12, unit = "in")


## Combine the tempo and timing dataframes 

pub_maturation = bind_rows(ind_scores_m, ind_scores_f)

est_pub = est %>% 
  left_join(., pub_maturation, by = "sub_id") %>% 
  drop_na(pub_tempo) 

write_csv(est_pub,"estimates_covariates_pubanalyses.csv") 


##
  ## Lastly, visualize the relationship between pubertal timing and tempo
##

plot_data = est_pub %>%
  distinct(sub_id, pub_tempo, pub_timing, sex)

# Fit models by sex
models = plot_data %>%
  group_by(sex) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(pub_tempo ~ pub_timing, data = .x))
  )

# Model summaries
coef_table = models %>%
  mutate(tidy = map(model, ~ tidy(.x))) %>%
  select(sex, tidy) %>%
  unnest(tidy) 

fit_table = models %>%
  mutate(glance = map(model, ~ glance(.x))) %>%
  select(sex, glance) %>%
  unnest(glance) %>%
  select(sex, r.squared, adj.r.squared, statistic, p.value, df, df.residual)

# Print tables
coef_table
fit_table


# Predicted relationships
pred_dat = models %>%
  mutate(
    pred = map(data, ~{
      grid <- tibble(pub_timing = seq(min(.x$pub_timing),
                                      max(.x$pub_timing),
                                      length.out = 100))
      grid$pub_tempo = predict(lm(pub_tempo ~ pub_timing, data = .x), newdata = grid)
      grid
    })
  ) %>%
  select(sex, pred) %>%
  unnest(pred)

# Visualize <3 
tempo_timing = ggplot(plot_data, aes(pub_timing, pub_tempo, color = sex)) +
  geom_point(alpha = 0.1) +
  scale_color_manual(values = c(M = "#005B94", F = "#BBB53E")) +
  theme_minimal(base_size = 14) +
  labs(x = "Pubertal timing", y = "Pubertal tempo") +
  theme(legend.position = "none")



####
tempo_timing =
  ggplot(plot_data, aes(pub_timing, pub_tempo, color = sex)) +
  geom_point(alpha = 0.1) +
  
  geom_xsidedensity(
    aes(x = pub_timing, y = after_stat(density), fill = sex),
    alpha = 0.35,
    linewidth = 0.6,
    position = "identity"
  ) +
  
   
  geom_ysidedensity(
    aes(y = pub_tempo, x = after_stat(density), fill = sex),
    alpha = 0.35,
    linewidth = 0.6,
    position = "identity"
  ) +
  
  scale_color_manual(values = c(M = "#005B94", F = "#BBB53E")) +
  scale_fill_manual(values = c(M = "#005B94", F = "#BBB53E")) +
  
  theme_minimal(base_size = 14) +
  labs(x = "Pubertal timing", y = "Pubertal tempo") +
  
  theme(
    axis.title.x = element_text(hjust = 0.5 * (1 - 0.25)),
    axis.title.y = element_text(hjust = 0.5 * (1 - 0.25)),
    
    legend.position = "none",
    ggside.panel.scale = 0.25,
    
    # remove side axis text/ticks
    ggside.axis.text = element_blank(),
    ggside.axis.ticks = element_blank(),
    
    # make side panels clean
    ggside.panel.background = element_blank(),
    ggside.panel.grid = element_blank()
  )
####



ggsave("pub_tempo_timing.png", tempo_timing,
       height = 5, width = 6, unit = "in")


## All plots together
pub_plots = (raw_scores | tempo_timing) / trajectories_pds
ggsave("puberty_figures_v2.png", pub_plots,
       height = 10, width = 10, unit = "in")

