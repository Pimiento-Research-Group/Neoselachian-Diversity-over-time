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
data(tens, 
     package = "divDyn")


# bin to 10myr bins -----------------------------------------------------------

# use geological stages and age estimates fully falling 
# into discrete bins
dat_species %>% 
  # bin occurrences to stages
  mutate(bin_max = 49 - cut(max_ma, breaks = tens$bottom,
                            include.lowest = TRUE,
                            labels = FALSE), 
         bin_min = 50 - cut(min_ma, breaks = tens$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_max, bin_min) %>% 
  mutate(reference_no = as.numeric(as.factor(reference))) %>% 
  filter(bin_max == bin_min) %>% 
  select(accepted_name, rank, genus, 
         family, order, superorder, 
         stg = bin_min, collection_no, 
         reference_no) %>% 
  write_rds(here("data", 
                 "species_binned_10myr.rds"))


# same for genus level
dat_genus %>% 
  # bin occurrences to 10myr
  mutate(bin_max = 49 - cut(max_ma, breaks = tens$bottom,
                            include.lowest = TRUE,
                            labels = FALSE),
         bin_min = 50 - cut(min_ma, breaks = tens$top,
                            include.lowest = TRUE,
                            labels = FALSE)) %>% 
  drop_na(bin_max, bin_min) %>% 
  mutate(reference_no = as.numeric(as.factor(reference))) %>% 
  filter(bin_max == bin_min) %>% 
  select(accepted_name, rank, genus, 
         family, order, superorder, 
         stg = bin_min, collection_no, 
         reference_no) %>% 
  write_rds(here("data", 
                 "genus_binned_10myr.rds"))

