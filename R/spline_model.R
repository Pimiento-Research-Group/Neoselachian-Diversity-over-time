library(here)
library(tidyverse)
library(writexl)


# read data ---------------------------------------------------------------

# pyrate species
dat_pyrate_species <- read_delim(here("data", 
                                      "species_PyRate_mcmcdiv.log")) %>% 
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
                                      "genus_PyRate_mcmcdiv.log")) %>% 
  select(it, contains("t_")) %>% 
  mutate(across(-it, as.double)) %>% 
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
  select(start_age, diversity = genusRT) 

# raw species
dat_raw <- read_rds(here("data",
                         "diversity_continuous_raw.rds")) %>% 
  select(start_age, diversity = speciesRT) 

# raw genus
dat_raw_genus <- read_rds(here("data",
                               "diversity_continuous_raw.rds")) %>% 
  select(start_age, diversity = genusRT) 

# divvy species
dat_divvy_species <- read_rds(here("data",
                                   "diversity_continuous_divvy_species.rds")) %>% 
  select(-stg)

# divvy genus
dat_divvy_genus <- read_rds(here("data",
                                   "diversity_continuous_divvy_genus.rds")) %>% 
  select(-stg)

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds")) %>% 
  select(stage = name, start_age = max_age)

# define the bins
bins <- sort(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
               89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
               47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
               13.820, 11.630, 7.246, 5.333, 2.580, 0))

# set up spline function --------------------------------------------------

div_per_stage <- function(data_set) {
  
  # and predict 
  dat_mean <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "mean",
           xout = bins,
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "mean_div") %>% 
    add_column(start_age = bins,
               .before = 1)
  
  # and predict 
  dat_min <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "min",
           xout = bins,
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "min_div") %>% 
    add_column(start_age = bins,
               .before = 1)
  
  # and predict 
  dat_max <- data_set %>%
    spline(x = .$start_age,
           y = .$diversity, 
           ties = "max",
           xout = bins,
           method = "natural") %>% 
    pluck("y") %>% 
    as_tibble_col(column_name = "max_div") %>% 
    add_column(start_age = bins,
               .before = 1)
  
  # combine
  dat_full <- dat_mean %>% 
    full_join(dat_min) %>% 
    full_join(dat_max) %>%
    mutate(min_div = if_else(min_div < 0, 0, min_div)) %>% 
    left_join(stage_cor) %>% 
    filter(start_age > 0)
  
  return(dat_full)
  
}

# set up summary function
sum_per_stage <- function(data_set) {
  data_set %>% 
    group_by(start_age) %>% 
    summarise(mean_div = mean(diversity), 
              min_div = min(diversity), 
              max_div = max(diversity)) %>% 
    filter(start_age > 0) %>% 
    left_join(stage_cor)
}


# double check spline model
div_per_stage(dat_pyrate_species) %>% 
  ggplot(aes(start_age)) +
  geom_ribbon(aes(ymin = min_div, 
                  ymax = max_div), 
              alpha = 0.2) +
  geom_point(aes(y = mean_div)) +
  geom_line(aes(y = mean_div), 
            data = dat_pyrate_species %>% 
              group_by(start_age) %>% 
              summarise(mean_div = mean(diversity)), 
            colour = "coral2") +
  scale_x_reverse() +
  theme_minimal()

# fit

# species
div_per_stage(dat_pyrate_species) %>% 
  add_column(metric = "pyrate") %>% 
  full_join(sum_per_stage(dat_raw) %>%
              add_column(metric = "raw")) %>% 
  full_join(sum_per_stage(dat_sqs) %>%
              add_column(metric = "sqs")) %>% 
  full_join(dat_divvy_species %>%
              add_column(metric = "divvy")) %>% 
  # save 
  write_xlsx(here("data", "taxa_per_stage_per_metric_species.xlsx"))
  
# genus
div_per_stage(dat_pyrate_genus) %>% 
  add_column(metric = "pyrate") %>% 
  full_join(sum_per_stage(dat_raw_genus) %>%
              add_column(metric = "raw")) %>% 
  full_join(sum_per_stage(dat_sqs_genus) %>%
              add_column(metric = "sqs")) %>% 
  full_join(dat_divvy_genus %>%
              add_column(metric = "divvy")) %>% 
  # save 
  write_xlsx(here("data", "taxa_per_stage_per_metric_genus.xlsx"))


# 
# # set up perc change function ---------------------------------------------------------
# 
# get_perc_change <- function(data_set, start_age_vec) {
#   
#   data_set %>% 
#     filter(start_age %in% start_age_vec) %>%
#     arrange(start_age) %>%
#     group_by(start_age) %>%
#     summarise(diversity = mean(diversity)) %>%
#     pivot_wider(names_from = start_age,
#                 values_from = diversity) %>%
#     mutate(perc_change = ((pick(1) - pick(2))/pick(2))*100)
#   
# }
# 
# 
# 
# # apply -------------------------------------------------------------------
# 
# # campanian-selandian pyrate
# get_perc_change(dat_pyrate_species, c(79.872, 60.058))
# 
# # maastrichtian-danian sqs
# get_perc_change(dat_sqs, c(72.1, 66))
# 
# # danian-selandian sqs
# get_perc_change(dat_sqs, c(66, 61.6))
# 
# # campanian-maastrichtian
# get_perc_change(dat_sqs, c(83.6, 72.1))
# get_perc_change(dat_pyrate_species, c(83.679, 74.122))
# 
# # campanian-maastrichtian deepdive
# get_perc_change(dat_pyrate_species, c(83.679, 61.258))
# 
# 
#   
