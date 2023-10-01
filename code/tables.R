
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

#tables
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


pred_temp_medians %>% 
  ggplot(aes(x = weigh_c, y = .epred, fill = temp_c, color = pred_regime)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = dat_c, aes(y = O2_c)) +
  labs(y = "log10 Metabolic Rate",
       x = "log10 Body Size") +
  facet_grid(pred_regime~temp_c)


pred_temp_medians %>% 
  ggplot(aes(x = weigh_c, y = .epred, fill = temp_c, color = pred_regime)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  geom_point(data = dat_c, aes(y = O2_c)) +
  labs(y = "log10 Metabolic Rate",
       x = "log10 Body Size") +
  facet_grid(pred_regime~temp_c)
