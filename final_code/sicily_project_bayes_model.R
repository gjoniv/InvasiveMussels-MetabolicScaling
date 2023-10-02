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

dat_c = data %>% 
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


#Sample posteriors and conditional posteriors

pred_temp_posts = as_draws_df(model_brm)

pred_temp_conditional_posts = size_pred_temp_sims %>% 
  add_epred_draws(model_brm)

# predation and temperature interaction
pred_temp_slopes = tibble(weigh_c = c(0, 0.0001)) %>% 
  expand_grid(model_brm$data %>% distinct(temp_c, pred_regime)) %>% 
  add_epred_draws(model_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = weigh_c, values_from = .epred) %>% 
  mutate(slope = 1e4*(`1e-04` - `0`)) %>% 
  group_by(temp_c, pred_regime) %>% 
  mutate(model = "log_a ~ log_m*temp_c*pred_regime")

#tables slope
all_slopes = bind_rows(pred_temp_slopes) %>% 
  mutate(id = paste0(temp_c, pred_regime),
         id_no = as.factor(as.integer(as.factor(id))),
         parameter = "slope")

slope_table = pred_temp_slopes %>% 
  group_by(temp_c, pred_regime) %>% 
  median_qi(slope) %>% 
  select(-.width, -.point, -.interval) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(slope = paste0(slope, " (", .lower, " to ", .upper, ")")) %>% 
  select(-.lower, -.upper)

#table intercept
all_intercepts = bind_rows(pred_temp_intercept) %>% 
  mutate(id = paste0(temp_c, pred_regime, model),
         id_no = as.factor(as.integer(as.factor(id))),
         parameter = "intercept") 

intercept_table = all_intercepts %>% 
  group_by(model, temp_c,pred_regime) %>% 
  median_qi(.epred) %>% 
  select(-.width, -.point, -.interval) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(intercept = paste0(.epred, " (", .lower, " to ", .upper, ")")) %>% 
  select(-.lower, -.upper, -.epred)


#figures

pred_temp_medians = pred_temp_conditional_posts %>% 
  group_by(weigh_c, temp_c, pred_regime) %>% 
  median_qi(.epred)

pred_temp_lines = dat_c %>% distinct(temp_c, pred_regime) %>% 
  mutate(weigh_c = 0) %>% 
  add_epred_draws(model_brm) %>% 
  group_by(temp_c, pred_regime) %>% 
  summarize(intercept = median(.epred)) %>% 
  mutate(slope = -0.75)

pred_temp_intercept = tibble(weigh_c = c(0)) %>% 
  expand_grid(model_brm$data %>% distinct(temp_c, pred_regime)) %>% 
  add_epred_draws(model_brm) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  mutate(model = "log_a ~ log_m*temp_c*pred_regime")


###Figure 1###
pred_temp_medians %>% 
  ggplot(aes(x = weigh_c, y = .epred, color= temp_c, fill = temp_c)) + 
  geom_smooth(data = dat_c, aes(y = O2_c), method = "lm", 
              se = FALSE) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) +
  geom_point(data = dat_c, aes(y = O2_c)) +
  labs(y = "metabolic rate",
       x = "body size") + geom_abline(intercept = 0, slope = 0.75, size=1, linetype = "dashed", color = "black") +
  facet_grid(temp_c~pred_regime) + theme_few() +
  scale_fill_gradient(low = "blue", high = "red") + 
  scale_color_gradient(low = "blue", high = "red")

###Figure 2###
pred_temp_medians %>% 
  ggplot(aes(x = weigh_c, y = .epred, group=temp_c, fill= temp_c, color = temp_c)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) +
  geom_point(data = dat_c, aes(y = O2_c)) +
  labs(y = "metabolic rate",
       x = "body size") + geom_abline(intercept = 0, slope = 0.75, size=1, linetype = "dashed", color = "black") +
  facet_grid(. ~ pred_regime) + theme_few() + scale_fill_gradient(low = "blue", high = "red") + 
  scale_color_gradient(low = "blue", high = "red")

pred_temp_medians %>% 
  ggplot(aes(x = weigh_c, y = .epred, group=pred_regime, fill= pred_regime, color = pred_regime)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) +
  geom_point(data = dat_c, aes(y = O2_c)) +
  labs(y = "metabolic rate",
       x = "body size") + geom_abline(intercept = 0, slope = 0.75, size=1, linetype = "dashed", color = "black") +
  facet_grid(. ~ temp_c) + theme_few() 

all_intercepts = bind_rows(pred_temp_intercept) %>% 
  mutate(id = paste0(temp_c, pred_regime, model),
         id_no = as.factor(as.integer(as.factor(id))),
         parameter = "intercept") 

intercept_table = all_intercepts %>% 
  group_by(model, temp_c,pred_regime) %>% 
  median_qi(.epred) %>% 
  select(-.width, -.point, -.interval) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(intercept = paste0(.epred, " (", .lower, " to ", .upper, ")")) %>% 
  select(-.lower, -.upper, -.epred)


####### probability tests ######

# probability of differences in intercepts

diff_temp = model_brm$data %>% 
  distinct(pred_regime, temp_c) %>% 
  mutate(weigh_c = 0) %>% 
  add_epred_draws(model_brm) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = temp_c, values_from = .epred) %>% 
  mutate(diff_pred = 13 - 27,
         prop_pred = diff_pred/13)


diff = model_brm$data %>% 
  distinct(pred_regime, temp_c) %>% 
  mutate(weigh_c = 0) %>% 
  add_epred_draws(model_brm) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = pred_regime, values_from = .epred) %>% 
  mutate(diff_pred = no_predation - predation,
         prop_pred = diff_pred/no_predation)

# mean and CrI differences

diff %>% 
  group_by(temp_c, weigh_c) %>% 
  median_qi(diff_pred,
            prop_pred)

diff_temp %>% 
  group_by(pred_regime, weigh_c) %>% 
  median_qi(diff_pred,
            prop_pred)


diff %>% 
  group_by(temp_c, weigh_c) %>% 
  reframe(sum_diff = sum(diff_pred>0),
          prob_diff = sum_diff/4000)


# probability of differences in slopes


diff_slopes = model_brm$data %>% 
  distinct(pred_regime, temp_c) %>% 
  expand_grid(weigh_c = c(0, 1)) %>% 
  add_epred_draws(model_brm) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = weigh_c, values_from = .epred) %>% 
  mutate(slope_diff = `1`-`0`) %>% 
  select(-`0`, -`1`) %>% 
  pivot_wider(names_from = pred_regime, values_from = slope_diff) %>% 
  mutate(diff_slope = predation - no_predation)

# mean and CrI differences

diff_slopes %>% 
  group_by(temp_c) %>% 
  median_qi(diff_slope)


# probability of differences

diff_slopes %>% 
  group_by(temp_c) %>% 
  reframe(sum_diff = sum(diff_slope>0),
          prob_diff = sum_diff/4000)
