library(here)
library(tidyverse)



# read data ---------------------------------------------------------------

dat_pyrate_species <- read_delim(here("data", 
                                      "all_species_input_PyRate_mcmcdiv.log")) %>% 
  select(it, contains("t_")) %>% 
  pivot_longer(-it, 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = str_remove_all(start_age, "t_"), 
         start_age = as.double(start_age)) %>% 
  rename(run = it) %>% 
  filter(run != 0) %>% 
  mutate(run = run/500) %>% 
  add_column(metric = "PyRate")

dat_sqs <- read_rds(here("data",
                         "diversity_continuous_sqs.rds")) %>% 
  select(start_age, diversity = speciesRT) 


# set up function ---------------------------------------------------------

get_perc_change <- function(data_set, start_age_vec) {
  
  data_set %>% 
    filter(start_age %in% start_age_vec) %>%
    arrange(start_age) %>%
    group_by(start_age) %>%
    summarise(diversity = mean(diversity)) %>%
    pivot_wider(names_from = start_age,
                values_from = diversity) %>%
    mutate(perc_change = ((pick(1) - pick(2))/pick(2))*100)
  
}


# apply -------------------------------------------------------------------

# campanian-selandian pyrate
get_perc_change(dat_pyrate_species, c(79.872, 60.058))

# maastrichtian-danian sqs
get_perc_change(dat_sqs, c(72.1, 66))

# danian-selandian sqs
get_perc_change(dat_sqs, c(66, 61.6))

# campanian-maastrichtian
get_perc_change(dat_sqs, c(83.6, 72.1))
get_perc_change(dat_pyrate_species, c(83.679, 74.122))

# campanian-maastrichtian deepdive
get_perc_change(dat_pyrate_species, c(83.679, 61.258))

# fit a spline model ------------------------------------------------------

# and predict 
dat_pred <- dat_pyrate_species %>%
  spline(x = .$start_age,
         y = .$diversity, 
         ties = min, 
         xout = c(83.6, 72.1),
         method = "natural") %>% 
  pluck("y")
  
((dat_pred[2] - dat_pred[1])/dat_pred[1])*100
  
