library(tidyverse)
library(here)


# read data ---------------------------------------------------------------

# species level
dat_species <- read_rds(here("data",
                             "fins_filtered_species.rds"))

# genus level
dat_genus <- read_rds(here("data", 
                           "fins_filtered_genus.rds"))

# attach the time scale object
data(stages, 
     package = "divDyn")


# bin to stages -----------------------------------------------------------

# use geological stages and age estimates fully falling 
# into discrete bins
dat_species %>% 
  # bin occurrences to stages
  mutate(bin_max = 95 - cut(max_ma, breaks = stages$bottom,
                            include.lowest = TRUE,
                            labels = FALSE),
         bin_min = 96 - cut(min_ma, breaks = stages$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_max, bin_min) %>% 
  filter(bin_max == bin_min) %>% 
  select(accepted_name, rank, genus, family, order, superorder, 
         stg = bin_min) %>% 
  write_rds(here("data", 
                 "species_binned_stages.rds"))


# same for genus level
dat_genus %>% 
  # bin occurrences to stages
  mutate(bin_max = 95 - cut(max_ma, breaks = stages$bottom,
                            include.lowest = TRUE,
                            labels = FALSE),
         bin_min = 96 - cut(min_ma, breaks = stages$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_max, bin_min) %>% 
  filter(bin_max == bin_min) %>% 
  select(accepted_name, rank, genus, family, order, superorder, 
         stg = bin_min) %>% 
  write_rds(here("data", 
                 "genus_binned_stages.rds"))


