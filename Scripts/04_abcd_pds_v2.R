library(tidyverse)
library(ggplot2)
library(nlme)
library(broom)
library(patchwork)
library(ggside)
library(stats)

# Import
est = read_csv("estimates_covariates_ageanalyses_rev1.csv") %>% 
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
           ~ ifelse(.x %in% c(777, 999), NA, .x)),                              # Set 777/999 responses to NA
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

# Sex distribution of subjects with >3 regressing cases
regressing_ids_sex = regressing_ids %>% 
  filter(n_declines ==3) 

pds = pds %>% 
  filter(!sub_id %in% regressing_ids_sex$sub_id)

length(unique(pds$sub_id)) 


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


pds = pds %>%         
  drop_na(pds_mean) %>% 
  group_by(sub_id) %>% 
  mutate(n_sessions = n()) %>% 
  ungroup() %>% 
  filter(n_sessions > 2) %>%
  mutate(pds_prop = (pds_mean - 1) / 3)                                         # Scale to 0 - 1 for model convergence
  
males = pds %>% 
  filter(sex == "M") 

females = pds %>% 
  filter(sex == "F")

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


###
#   ASYM MODEL
#   Logistic model with formula from SSlogis function
#   This model estimates ceiling as well through Asym parameter
#


###
## Males 
#


#   More sensitive to starting values to run, run single-level fits to identify
#   good starting values. 

init_fits = nlsList(pds_prop ~ SSlogis(age_pds, Asym, xmid, scal) | sub_id, data = males)
summary(init_fits)
coef_init = coef(init_fits)
apply(coef_init, 2, median, na.rm = TRUE)



logistic_asym_m = nlme(
  pds_prop ~ Asym / (1 + exp((xmid - age_pds) / exp(logscal))),                 
  data = males,
  fixed = Asym + xmid + logscal ~ 1,
  random = xmid + logscal ~ 1 | sub_id,
  start = c(Asym = 1, xmid = 12.78, logscal = log(1.04)),
  method = 'ML',
  control = nlmeControl(pnlsTol = 1e-6, maxIter = 200, pnlsMaxIter = 50, 
                        msMaxIter = 300, msTol = 1e-8, returnObject = F))


summary(logistic_asym_m)

coef_asym_m = coef(logistic_asym_m)
coef_asym_m$scal = exp(coef_asym_m$logscal)

logistic_asym_m_scores = data.frame(
  sub_id = rownames(coef_asym_m),
  timing = coef_asym_m[,"xmid"],
  pub_scal   = coef_asym_m[,"scal"]) %>% 
  mutate(scal_flipped = 1/pub_scal,                                             # rate constant - how fast curve rises, regardless of height
         tempo_scaled = coef_asym_m[,"Asym"] / (4 * pub_scal)) %>%              # slope of curve at midpoint 
  mutate(tempo = tempo_scaled * 3) %>%                                          # bring back to 1 - 4 scale in outcome-units per year
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_asym_m_scores$timing, 
                logistic_asym_m_scores$tempo, 
                method = "pearson") 
spearman_rho = cor(logistic_asym_m_scores$timing, 
                   logistic_asym_m_scores$tempo, 
                   method = "spearman") 


fig_log_asym_m = logistic_asym_m_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.5, color = "#005B94") +
  ggtitle("Logistic model (SSlogis)") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)




###
## Females
#

#
# Find starting values

init_fits = nlsList(pds_prop ~ SSlogis(age_pds, Asym, xmid, scal) | sub_id, data = females)
summary(init_fits)
coef_init = coef(init_fits)
apply(coef_init, 2, median, na.rm = TRUE) 



logistic_asym_f = nlme(
  pds_prop ~ Asym / (1 + exp((xmid - age_pds) / exp(logscal))), 
  data = females,
  fixed = Asym + xmid + logscal ~ 1,
  random = xmid + logscal ~ 1 | sub_id,
  start = c(Asym = 1, xmid = 11.66, logscal = log(1.12)),
  method = 'ML',
  control = nlmeControl(pnlsTol = 1e-6, maxIter = 200, pnlsMaxIter = 50, 
                        msMaxIter = 300, msTol = 1e-8, returnObject = F)) 


summary(logistic_asym_f)

coef_asym_f = coef(logistic_asym_f)
coef_asym_f$scal = exp(coef_asym_f$logscal)

logistic_asym_f_scores = data.frame(
  sub_id = rownames(coef_asym_f),
  timing = coef_asym_f[,"xmid"],
  pub_scal   = coef_asym_f[,"scal"]) %>% 
  mutate(scal_flipped = 1/pub_scal,                                                
         tempo_scaled = coef_asym_f[,"Asym"] / (4 * pub_scal)) %>%                  
  mutate(tempo = tempo_scaled * 3) %>%                                             
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_asym_f_scores$timing, 
                logistic_asym_f_scores$tempo, 
                method = "pearson") 
spearman_rho = cor(logistic_asym_f_scores$timing, 
                   logistic_asym_f_scores$tempo, 
                   method = "spearman") 

fig_log_asym_f = logistic_asym_f_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.7, color = "#BBB53E") +
  ggtitle("Logistic model (SSlogis)") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)




##
## Visualize predicted trajectories 
##

prop_to_pds = function(p) 1 + 3 * p

# Make predicted trajectories 
make_pred_grid = function(df, fit, id_col = "sub_id", 
                          age_col = "age_pds", n_age = 200) {
  
  age_range = range(df[[age_col]], na.rm = T)
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
  
  # Rescale to PDS scale
  newdata$pds_hat_ind = prop_to_pds(newdata$y_hat_ind)
  newdata$pds_hat_pop = prop_to_pds(newdata$y_hat_pop)
  
  newdata
  
}


pred_males   = make_pred_grid(males, logistic_asym_m)
pred_females = make_pred_grid(females, logistic_asym_f)


# Population timing
lambda_pop_m = as.numeric(fixef(logistic_asym_m)["xmid"])
lambda_pop_f = as.numeric(fixef(logistic_asym_f)["xmid"])



# Plot
trajectories_pds_m = ggplot(pred_males, aes(x = (age_pds))) +
  geom_line(aes(y = pds_hat_ind, group = sub_id),
            alpha = 0.08, linewidth = 0.4) +
  geom_line(aes(y = pds_hat_pop),
            linewidth = 1.5, color = "#005B94") +
  geom_hline(yintercept = 2.5, linetype = 2, linewidth = 1) +
  geom_vline(xintercept = lambda_pop_m, linetype = 2, linewidth = 1, color = "#005B94") +  
  coord_cartesian(ylim = c(1, 4)) +
  labs(
    x = "Age (years)",
    y = "Predicted PDS",
    title = "Males"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18))



trajectories_pds_f = ggplot(pred_females, aes(x = (age_pds))) +
  geom_line(aes(y = pds_hat_ind, group = sub_id),
            alpha = 0.08, linewidth = 0.4) +
  geom_line(aes(y = pds_hat_pop),
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






ggsave("female_pds_trajectories_rev1.png", trajectories_pds_f,
       height = 5, width = 6, unit = "in", dpi = 500)

ggsave("male_pds_trajectories_rev1.png", trajectories_pds_m,
       height = 5, width = 6, unit = "in", dpi = 500)

trajectories_pds = trajectories_pds_f | trajectories_pds_m

ggsave("pds_trajectories_rev1.png", trajectories_pds,
       height = 5, width = 12, unit = "in", dpi = 500)



##
## Combine the tempo and timing dataframes 
##

scores_f = logistic_asym_f_scores %>% 
  select(sub_id, timing, tempo) %>% 
  rename(pub_tempo = tempo,
         pub_timing = timing)

scores_m = logistic_asym_m_scores %>% 
  select(sub_id, timing, tempo) %>% 
  rename(pub_tempo = tempo,
         pub_timing = timing)

pub_maturation = bind_rows(scores_f, scores_m)

est_pub = est %>% 
  left_join(., pub_maturation, by = "sub_id") %>% 
  drop_na(pub_tempo) 

write_csv(est_pub,"estimates_covariates_pubanalyses_rev1.csv") 



##
  ## Lastly, test and visualize the relationship between pubertal timing and tempo
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


# Visualize
tempo_timing = ggplot(plot_data, aes(pub_timing, pub_tempo, color = sex)) +
  
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
  
  scale_color_manual(values = c(M = "#005B94", F = "#BBB53E"), name = "Sex") +
  scale_fill_manual(values = c(M = "#005B94", F = "#BBB53E"), name = "Sex") +
  
  theme_minimal(base_size = 14) +
  labs(x = "Pubertal timing", y = "Pubertal tempo") +
  
  theme(
    axis.title.x = element_text(hjust = 0.5 * (1 - 0.25)),
    axis.title.y = element_text(hjust = 0.5 * (1 - 0.25)),
    ggside.panel.scale = 0.25,
    
    ggside.axis.text = element_blank(),
    ggside.axis.ticks = element_blank(),
    
    ggside.panel.background = element_blank(),
    ggside.panel.grid = element_blank()
  )



ggsave("pub_tempo_timing_rev1.png", tempo_timing,
       height = 5, width = 6, unit = "in", dpi = 500)


## All plots together
pub_plots = (raw_scores | tempo_timing) / trajectories_pds
ggsave("puberty_figures_rev1.png", pub_plots,
       height = 10, width = 10, unit = "in", dpi = 600)

