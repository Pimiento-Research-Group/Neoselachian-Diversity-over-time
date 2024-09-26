library(here)
library(tidyverse)
library(divDyn)

# read data ---------------------------------------------------------------

# species level
dat_species <- read_rds(here("data",
                             "fins_filtered_species.rds"))

# genus level
dat_genus <- read_rds(here("data", 
                           "fins_filtered_genus.rds"))

# stage level data
data(stages, package = "divDyn")

# define the bins
bins <- sort(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
                 89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
                 47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
                 13.820, 11.630, 7.246, 5.333, 2.580, 0), 
             decreasing = TRUE)


# set up function to calculate continuous diversity ------------------------

get_div <- function(data_set, tax_level, div_metric) {
  
  # create age sequence
  dat_age <- data_set %>% 
    mutate(reference_no = as.numeric(as.factor(reference))) %>% 
    select(accepted_name, rank, genus, 
           family, order, superorder, 
           max_ma, min_ma, collection_no, 
           reference_no) %>% 
    mutate(age_mid = map2(.x = min_ma, .y = max_ma, 
                          .f = ~ runif(100, .x, .y)))
  # set up empty list
  list_div <- vector("list", 100)
  
  if (div_metric == "raw") {
    
    # iterate over age sequence and save in list
    for (i in 1:100) {
      list_div[[i]] <- dat_age %>% 
        mutate(age_sel = map_dbl(age_mid, 
                                 ~pluck(.x, i))) %>% 
        mutate(stg = cut(age_sel, 
                         breaks = bins, 
                         include.lowest = TRUE, 
                         labels = FALSE)) %>% 
        divDyn(., bin = "stg", tax = "accepted_name") %>% 
        as_tibble() %>% 
        add_column(start_age = rev(bins[-30])) %>% 
        filter(between(start_age, 0, 150)) %>% 
        select(stg, start_age,
               divRT, divBC, divSIB) %>% 
        rename_with(~str_replace(., "div", tax_level), contains("div")) %>%
        add_column(run = i)
    }
    
  } else if (div_metric == "sqs") {
    
    for (i in 1:100) {
      list_div[[i]] <- dat_age %>% 
        mutate(age_sel = map_dbl(age_mid, 
                                 ~pluck(.x, i))) %>% 
        mutate(stg = cut(age_sel, 
                              breaks = bins, 
                              include.lowest = TRUE, 
                              labels = FALSE)) %>% 
        drop_na(stg) %>% 
        subsample(bin = "stg",
                  tax = "accepted_name",
                  iter = 100, q = 0.3, type = "sqs", 
                  ref = "reference_no",
                  singleton = "ref") %>% 
        add_column(start_age = rev(bins[-30])) %>% 
        filter(between(start_age, 0, 150)) %>% 
        select(stg, start_age,
               divRT, divBC, divSIB) %>% 
        rename_with(~str_replace(., "div", tax_level), contains("div")) %>%
        add_column(run = i)
    }
  }
  
  # check if there is variation in estimates across age vector
  list_div %>% 
    bind_rows() 
 
}


# get diversity estimates -------------------------------------------------


dat_div_spec <- get_div(dat_species, "species", "raw")

dat_div_gen <- get_div(dat_genus, "genus", "raw")

dat_div_spec_sqs <- get_div(dat_species, "species", "sqs")

dat_div_gen_sqs <- get_div(dat_genus, "genus", "sqs")


# save data ---------------------------------------------------------------

# raw estimates
dat_div_spec %>% 
  full_join(dat_div_gen) %>% 
  write_rds(here("data",
                 "diversity_continuous_raw.rds"), 
            compress = "gz")

# subsampled estimates
dat_div_spec_sqs %>% 
  as_tibble() %>% 
  full_join(dat_div_gen_sqs %>% 
              as_tibble()) %>% 
  write_rds(here("data",
                 "diversity_continuous_sqs.rds"), 
            compress = "gz")

