dat_pyrate_species %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(diversity)) %>% 
  filter(between(start_age, 61.2, 74.2)) %>% 
  pivot_wider(names_from = start_age, 
              values_from = mean_div) %>% 
  mutate(KPg = ((`63.8` - `74.122`)/`74.122`)*100)


# loess model
dat_pred <- dat_pyrate_species %>%
  spline(x = .$start_age,
         y = .$diversity, 
         ties = min, 
         xout = c(72.1, 61.6),
         method = "natural") %>% 
  pluck("y")
  
((dat_pred[2] - dat_pred[1])/dat_pred[1])*100
  
61.6` - `72.1