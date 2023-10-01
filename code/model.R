library(tidyverse)
library(brms)
library(tidybayes)
library(modelsummary)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(dplyr)
library(ggeffects)
library(interactions)
library(ggthemes)
library(lavaan)


#(1) Make Data

dat_c = palermo_project %>% 
  mutate(weigh_c= scale(log_weight, center = T, scale = F),
         O2_c = scale(log_O2, center = T, scale = F),
         temp_c = scale(temperature, center = T, scale = F))

#(2) Fit Models
model_brm = brm(O2_c ~ weigh_c*temp_c*pred_regime,
                data = dat_c,
                family = gaussian(),
                prior = c(prior(normal(0, 1), class = "Intercept"),
                          prior(normal(0.75, 0.2), coef = "weigh_c"),
                          prior(normal(0, 1), class = "b")))


pred_regime = unique(dat_c$pred_regime)
test = plot(conditional_effects(model_brm, effects = "weigh_c:temp_c", conditions = pred_regime), points = T)

test$`weigh_c:temp_c`$data %>% View

summary(model_brm)

#(3) Check Models
pp_check(model_brm)

#(4) Extract posteriors
#Temperature and Predation brm

model_posts = as_draws_df(model_brm) 

bodysize_sims = tibble(weigh_c = seq(min(model_brm$data$weigh_c), max(model_brm$data$weigh_c), length.out = 20))

temp_sims = unique(model_brm$data$temp_c)
pred_sims = unique(model_brm$data$pred_regime)

size_pred_temp_sims = bodysize_sims %>% 
  expand_grid(temp_c = temp_sims) %>% 
  expand_grid(pred_regime = pred_sims)


model_posts %>% 
  expand_grid(log_body_size = c(-1, 0, 1)) %>% 
  mutate(epred = b_Intercept + b_weigh_c*log_body_size) %>% 
  group_by(log_body_size) %>% 
  median_qi(epred) %>% 
  ggplot(aes(x = log_body_size, y = epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5)
