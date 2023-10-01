####### propability tests ######

# propability of differences in intercepts

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
  group_by(pred_regime, weigh_c) %>% 
  median_qi(diff_pred,
            prop_pred)

diff_temp %>% 
  group_by(pred_regime, weigh_c) %>% 
  median_qi(diff_pred,
            prop_pred)


diff %>% 
  group_by(predation_regime, weigh_c) %>% 
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


plot(conditional_effects(model_brm), points = T)

