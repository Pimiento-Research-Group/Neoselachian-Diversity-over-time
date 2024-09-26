library(here)
library(tidyverse)
library(writexl)


# read data ---------------------------------------------------------------

# pyrate species
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

# pyrate genus
dat_pyrate_genus <- read_delim(here("data", 
                                      "all_genus_PyRate_mcmcdiv.log")) %>% 
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

# sqs species
dat_sqs <- read_rds(here("data",
                         "diversity_continuous_sqs.rds")) %>% 
  select(start_age, diversity = speciesRT) 

# sqs genus
dat_sqs_genus <- read_rds(here("data",
                         "diversity_continuous_sqs.rds")) %>% 
  select(start_age, diversity = speciesRT) 

# raw species
dat_raw <- read_rds(here("data",
                         "diversity_continuous_raw.rds")) %>% 
  select(start_age, diversity = speciesRT) 

# sqs genus
dat_raw_genus <- read_rds(here("data",
                               "diversity_continuous_raw.rds")) %>% 
  select(start_age, diversity = speciesRT) 

# set up spline function --------------------------------------------------

div_per_stage <- function(data_set) {
  
  # and predict 
  dat_mean <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "mean",
           xout = unique(dat_sqs$start_age),
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "mean_div") %>% 
    add_column(start_age = unique(dat_sqs$start_age),
               .before = 1)
  
  # and predict 
  dat_min <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "min",
           xout = unique(dat_sqs$start_age),
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "min_div") %>% 
    add_column(start_age = unique(dat_sqs$start_age),
               .before = 1)
  
  # and predict 
  dat_max <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "max",
           xout = unique(dat_sqs$start_age),
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "max_div") %>% 
    add_column(start_age = unique(dat_sqs$start_age),
               .before = 1)
  
  # combine
  dat_full <- dat_mean %>% 
    full_join(dat_min) %>% 
    full_join(dat_max) %>%
    mutate(min_div = if_else(min_div < 0, 0, min_div)) 
  
  return(dat_full)
  
}


# fit a spline model ------------------------------------------------------

# species
div_per_stage(dat_pyrate_species) %>% 
  # save 
  write_xlsx(here("data", "taxa_per_stage_species_pyrate.xlsx"))


# genus
div_per_stage(dat_pyrate_genus) %>% 
  # save 
  write_xlsx(here("data", "taxa_per_stage_genus_pyrate.xlsx"))


# set up summary function -------------------------------------------------

sum_per_stage <- function(data_set) {
  data_set %>% 
  group_by(start_age) %>% 
  summarise(mean_dif = mean(diversity), 
            min_dif = min(diversity), 
            max_dif = max(diversity))
  }

# raw species
sum_per_stage(dat_raw) %>% 
  write_xlsx(here("data", "taxa_per_stage_species_raw.xlsx"))

# raw genus
sum_per_stage(dat_raw_genus)%>% 
  write_xlsx(here("data", "taxa_per_stage_genus_raw.xlsx"))

# sqs species
sum_per_stage(dat_sqs)%>% 
  write_xlsx(here("data", "taxa_per_stage_raw_sqs.xlsx"))

# sqs genus
sum_per_stage(dat_sqs_genus)%>% 
  write_xlsx(here("data", "taxa_per_stage_genus_sqs.xlsx"))



# set up perc change function ---------------------------------------------------------

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


  
