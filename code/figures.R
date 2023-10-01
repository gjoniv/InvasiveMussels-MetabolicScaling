#figure

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
