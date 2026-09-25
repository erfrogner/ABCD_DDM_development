# Compare four different modeling approaches to estimating pubertal timing
# and pubertal tempo. Reads the csv-file from 04_abcd_pds.R. 



library(tidyverse)
library(nlme)
library(broom)
library(patchwork)
library(ggtext)
library(ggcorrplot)

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



females = pds %>% 
  filter(sex == "F",
         !is.na(pds_mean)) %>%
  group_by(sub_id) %>% 
  mutate(n_sessions = n(),
         pds_prop = (pds_mean - 1)/3) %>% 
  ungroup() %>% 
  filter(n_sessions > 2)

males = pds %>% 
  filter(sex == "M",
         !is.na(pds_mean)) %>% 
  group_by(sub_id) %>% 
  mutate(n_sessions = n(),
         pds_prop = (pds_mean - 1)/3) %>% 
  ungroup() %>% 
  filter(n_sessions > 2)




#####
#
# Linear model
#

# Females 

linear_f = lme(pds_prop ~ age_pds, 
               random = ~ age_pds | sub_id,
               data = females,
               method = "ML")

linear_f_summ = summary(linear_f)
linear_re = ranef(linear_f) 
linear_fe = fixef(linear_f)


linear_f_scores = data.frame(
  sub_id = rownames(linear_re),
  intercept = linear_fe["(Intercept)"] + linear_re[,"(Intercept)"],
  tempo_unscaled     = linear_fe["age_pds"] + linear_re[,"age_pds"]
) %>% 
  mutate(tempo = tempo_unscaled * 3,
         timing = (0.5 - intercept) / tempo_unscaled)

# Remove timing estimates that are > 3SD and tempo < .1 (Beltz et al. 2014)

linear_f_scores = linear_f_scores %>%
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(linear_f_scores$timing, 
                linear_f_scores$tempo, 
                method = "pearson")
spearman_rho = cor(linear_f_scores$timing, 
                   linear_f_scores$tempo, 
                   method = "spearman")

fig_linear_f = linear_f_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.7, color = "#BBB53E") +
  ggtitle("Linear model") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)





## Males 

linear_m = lme(pds_prop ~ age_pds, 
               random = ~ age_pds | sub_id,
               data = males,
               method = "ML")

linear_m_summ = summary(linear_m)
linear_re = ranef(linear_m) 
linear_fe = fixef(linear_m)

linear_m_scores = data.frame(
  sub_id = rownames(linear_re),
  intercept = linear_fe["(Intercept)"] + linear_re[,"(Intercept)"],
  tempo_unscaled     = linear_fe["age_pds"] + linear_re[,"age_pds"]
) %>% 
  mutate(tempo = tempo_unscaled * 3,
         timing = (0.5 - intercept) / tempo_unscaled)

# Remove timing estimates that are > 3SD and tempo < .1 (Beltz et al. 2014)

linear_m_scores = linear_m_scores %>%
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)



pearson_r = cor(linear_m_scores$timing, 
                linear_m_scores$tempo, 
                method = "pearson")
spearman_rho = cor(linear_m_scores$timing, 
                   linear_m_scores$tempo, 
                   method = "spearman")

fig_linear_m = linear_m_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.5, color = "#005B94") +
  ggtitle("Linear model") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12) 


####
#
# Logistic models
#

##
## simpler logistic mod without estimation of correlated random effects
##

# Females

logistic_noncorr_f = nlme(
  pds_prop ~ 1 / (1 + exp(-alpha * (age_pds - lambda))),
  data = females,
  fixed = alpha + lambda ~ 1,
  random = pdDiag(alpha + lambda ~ 1),
  groups = ~ sub_id,
  start = c(alpha = 0.5, lambda = 13),
  method = "ML",
  control = nlmeControl(
    pnlsTol = 1e-6,         
    maxIter = 200,          
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)

logistic_noncorr_f_scores = coef(logistic_noncorr_f) %>% 
  mutate(sub_id = rownames(.)) %>% 
  rename(tempo = alpha,
         timing = lambda) %>% 
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_noncorr_f_scores$timing, 
                logistic_noncorr_f_scores$tempo, 
                method = "pearson") # r = 0.26
spearman_rho = cor(logistic_noncorr_f_scores$timing, 
                   logistic_noncorr_f_scores$tempo, 
                   method = "spearman") # rho = 0.26

fig_log_noncorr_f = logistic_noncorr_f_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.7, color = "#BBB53E") +
  ggtitle("Logistic model without estimated correlation") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)


##
# Males

logistic_noncorr_m = nlme(
  pds_prop ~ 1 / (1 + exp(-alpha * (age_pds - lambda))),
  data = males,
  fixed = alpha + lambda ~ 1,
  random = pdDiag(alpha + lambda ~ 1),
  groups = ~ sub_id,
  start = c(alpha = 0.5, lambda = 13),
  method = "ML",
  control = nlmeControl(
    pnlsTol = 1e-6,         
    maxIter = 200,          
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)

logistic_noncorr_m_scores = coef(logistic_noncorr_m) %>% 
  mutate(sub_id = rownames(.)) %>% 
  rename(tempo = alpha,
         timing = lambda) %>% 
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_noncorr_m_scores$timing, 
                logistic_noncorr_m_scores$tempo, 
                method = "pearson") # r = 0.33
spearman_rho = cor(logistic_noncorr_m_scores$timing, 
                   logistic_noncorr_m_scores$tempo, 
                   method = "spearman") # rho = 0.32

fig_log_noncorr_m = logistic_noncorr_m_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.5, color = "#005B94") +
  ggtitle("Logistic model without estimated correlation") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)


##
## logistic model that also estimates correlations between tempo and timing (random effects)
##

# Females 

logistic_f = nlme(
  pds_prop ~ 1 / (1 + exp(-alpha * (age_pds - lambda))),
  data = females,
  fixed = alpha + lambda ~ 1,
  random = alpha + lambda ~ 1 | sub_id,
  start = c(alpha = 0.5, lambda = 13),
  method = "ML",
  control = nlmeControl(
    pnlsTol = 1e-6,         
    maxIter = 200,          
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)


logistic_f_scores = coef(logistic_f) %>% 
  mutate(sub_id = rownames(.)) %>% 
  rename(tempo = alpha,
         timing = lambda) %>% 
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_f_scores$timing, 
                logistic_f_scores$tempo, 
                method = "pearson") # r = 0.49
spearman_rho = cor(logistic_f_scores$timing, 
                   logistic_f_scores$tempo, 
                   method = "spearman") # rho = 0.47

fig_log_f = logistic_f_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.7, color = "#BBB53E") +
  ggtitle("Logistic model **with** estimated correlation") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_markdown())


# Males

logistic_m = nlme(
  pds_prop ~ 1  / (1 + exp(-alpha * (age_pds - lambda))),
  data = males,
  fixed = alpha + lambda ~ 1,
  random = alpha + lambda ~ 1 | sub_id,
  start = c(alpha = 0.5, lambda = 13),
  method = "ML",
  control = nlmeControl(
    pnlsTol = 1e-6,         
    maxIter = 200,          
    pnlsMaxIter = 50, 
    msMaxIter = 200, 
    returnObject = F
  )
)

logistic_m_scores = coef(logistic_m) %>% 
  mutate(sub_id = rownames(.)) %>% 
  rename(tempo = alpha,
         timing = lambda) %>% 
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_m_scores$timing, 
                logistic_m_scores$tempo, 
                method = "pearson") # r = 0.63
spearman_rho = cor(logistic_m_scores$timing, 
                   logistic_m_scores$tempo, 
                   method = "spearman") # rho = 0.6

fig_log_m = logistic_m_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.5, color = "#005B94") +
  ggtitle("Logistic model **with** estimated correlation") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)+
  theme(plot.title = element_markdown())


###
#
#   Logistic model with formula from SSlogis function, estimates ceiling (Asym) as well
#

## 1 + 3 / (1 + exp(-alpha * (age_pds - lambda)))
## with SSlogis, the expression is Asym / (1+exp((xmid-input)/scal))


#
# This model is more sensitive to starting values to run, 
# so do single subject level fits first to find starting values 

init_fits = nlsList(pds_prop ~ SSlogis(age_pds, Asym, xmid, scal) | sub_id, data = males)
summary(init_fits)
coef_init = coef(init_fits)
apply(coef_init, 2, median, na.rm = TRUE) 



logistic_asym_m = nlme(
  pds_prop ~ Asym / (1 + exp((xmid - age_pds) / exp(logscal))), 
  data = males,
  fixed = Asym + xmid + logscal ~ 1,
  random = xmid + logscal ~ 1 | sub_id,
  start = c(Asym = 1, xmid = 12.78, logscal = log(1.04)),                       # log transformed, has convergence issues otherwise
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
  mutate(tempo = tempo_scaled * 3) %>%                                          # bring back to 1 - 4 scale in PDS units per year
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_asym_m_scores$timing, 
                logistic_asym_m_scores$tempo, 
                method = "pearson") # r = 0.23
spearman_rho = cor(logistic_asym_m_scores$timing, 
                   logistic_asym_m_scores$tempo, 
                   method = "spearman") # rho = 0.22


fig_log_asym_m = logistic_asym_m_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.5, color = "#005B94") +
  ggtitle("Logistic model (Asymptote)") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)




####
##
# Females

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
  mutate(scal_flipped = 1/pub_scal,                                             # rate constant - how fast curve rises, regardless of height
         tempo_scaled = coef_asym_f[,"Asym"] / (4 * pub_scal)) %>%              # slope of curve at midpoint 
  mutate(tempo = tempo_scaled * 3) %>%                                          # bring back to 1 - 4 scale in outcome-units per year
  filter(abs(timing - mean(timing)) <= 3 * sd(timing),
         tempo >= 0.1)


pearson_r = cor(logistic_asym_f_scores$timing, 
                logistic_asym_f_scores$tempo, 
                method = "pearson") # r = 0.31
spearman_rho = cor(logistic_asym_f_scores$timing, 
                   logistic_asym_f_scores$tempo, 
                   method = "spearman") # rho = 0.31

fig_log_asym_f = logistic_asym_f_scores %>%
  ggplot(aes(x = timing, y = tempo)) +
  geom_point(alpha = 0.7, color = "#BBB53E") +
  ggtitle("Logistic model (Asymptote)") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
           label = sprintf("r = %.2f\nrho = %.2f", pearson_r, spearman_rho)) +
  theme_minimal(base_size = 12)


### Gather all the visualizations to give an overview over how tempo and timing
##  relate in the different models

fig_all_models = (fig_linear_f | fig_linear_m) / 
  (fig_log_noncorr_f | fig_log_noncorr_m) / 
  (fig_log_f | fig_log_m) /
  (fig_log_asym_f | fig_log_asym_m) 



################################################################################
##
#### Correlations across models
##
##

make_corr = function(dfs, var, model_names) {
  merged = Reduce(function(x, y) merge(x, y, by = "sub_id"),
                  lapply(seq_along(dfs), function(i) {
                    d = dfs[[i]][, c("sub_id", var)]
                    names(d)[2] = model_names[i]
                    d
                  }))
  cor(merged[, model_names], use = "complete.obs")
}

model_names = c("Linear", "Logistic: no covariance", "Logistic: covariance", "Logistic: asymptote")

# Females
f_dfs = list(linear_f_scores, logistic_noncorr_f_scores, logistic_f_scores, logistic_asym_f_scores)
timing_f_corr = make_corr(f_dfs, "timing", model_names)
tempo_f_corr  = make_corr(f_dfs, "tempo",  model_names)

# Males
m_dfs = list(linear_m_scores, logistic_noncorr_m_scores, logistic_m_scores, logistic_asym_m_scores)
timing_m_corr = make_corr(m_dfs, "timing", model_names)
tempo_m_corr  = make_corr(m_dfs, "tempo",  model_names)

# Plots
timing_corr_f = ggcorrplot(timing_f_corr, lab = TRUE, title = "Timing (females)", tl.cex = 11, lab_size = 2.8)
tempo_corr_f = ggcorrplot(tempo_f_corr,  lab = TRUE, title = "Tempo (females)", tl.cex = 11, lab_size = 2.8)
timing_corr_m = ggcorrplot(timing_m_corr, lab = TRUE, title = "Timing (males)", tl.cex = 11, lab_size = 2.8)
tempo_corr_m = ggcorrplot(tempo_m_corr,  lab = TRUE, title = "Tempo (males)", tl.cex = 11, lab_size = 2.8)


correlations_f = timing_corr_f | tempo_corr_f
correlations_m = timing_corr_m | tempo_corr_m

corrplots = correlations_f / correlations_m



###############################################################################
#
#
### Model comparisons (AIC/BIC)
#
#

# linear model
aic_linear_f = AIC(linear_f)
bic_linear_f = BIC(linear_f)

aic_linear_m = AIC(linear_m)
bic_linear_m = BIC(linear_m)

# logistic w/o correlated re's
aic_log_noncorr_f = AIC(logistic_noncorr_f)
bic_log_noncorr_f = BIC(logistic_noncorr_f)

aic_log_noncorr_m = AIC(logistic_noncorr_m)
bic_log_noncorr_m = BIC(logistic_noncorr_m)

# logistic with correlated re's
aic_log_f = AIC(logistic_f)
bic_log_f = BIC(logistic_f)

aic_log_m = AIC(logistic_m)
bic_log_m = BIC(logistic_m)

# logistic with sslogis approach (asym)
aic_log_asym_f = AIC(logistic_asym_f)
bic_log_asym_f = BIC(logistic_asym_f)

aic_log_asym_m = AIC(logistic_asym_m)
bic_log_asym_m = BIC(logistic_asym_m)



fit_stats = data.frame(
  model = rep(c("Linear", "Logistic: no covariance", "Logistic: covariance", "Logistic: asymptote"), each = 2, times = 2),
  sex = rep(c("Female", "Male"), each = 8),
  metric = rep(c("AIC", "BIC"), times = 8),
  value = c(aic_linear_f, bic_linear_f, aic_log_noncorr_f, bic_log_noncorr_f, aic_log_f, bic_log_f, aic_log_asym_f, bic_log_asym_f,
            aic_linear_m, bic_linear_m, aic_log_noncorr_m, bic_log_noncorr_m, aic_log_m, bic_log_m, aic_log_asym_m, bic_log_asym_m)) %>% 
  group_by(sex, metric) %>% 
  mutate(delta = value - min(value)) %>% 
  ungroup() %>% 
  mutate(model = factor(model, levels = c("Linear", "Logistic: no covariance", "Logistic: covariance", "Logistic: asymptote")))


model_comparison = ggplot(fit_stats, aes(x = model, y = delta, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(delta, 1)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
  facet_wrap(~sex, ncol = 1) +
  scale_fill_manual(values = c("AIC" = "black", "BIC" = "#00A5CF")) +
  labs(x = NULL, y = "\Delta (Information Criterion - best model)", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "top",
        panel.grid.major.x = element_blank())



ggsave("pub_models_correlations_rev1.png",corrplots, width = 8.5, height = 7, units = "in")
ggsave("pub_models_comparison_rev1.png", model_comparison, width = 6, height = 7, units = "in")
