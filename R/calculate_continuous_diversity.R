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

# define the bins
bins <- sort(c(0, 0.0042, 0.0082, 0.0117, 0.126, 0.781, 1.80, 2.58, 3.6, 5.333, 7.246, 11.63, 13.82,
               15.97, 20.44, 23.03, 28.1, 33.9, 37.8, 41.2, 47.8, 56.0, 59.2,
               61.6, 66.0, 72.1, 83.6, 86.3, 89.8, 93.9, 100.5, 113., 125., 129.4,
               132.9, 139.8, 155), decreasing=TRUE)

data(stages, package = "divDyn")

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
        add_column(start_age = rev(bins[-37])) %>% 
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
        add_column(start_age = rev(bins[-37])) %>% 
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

# simplify those cases where there is no variation across
# age estimates
dat_div_spec %>% 
  full_join(dat_div_gen) %>% 
  write_rds(here("data",
                 "diversity_continuous_raw.rds"), 
            compress = "gz")

dat_div_spec_sqs %>% 
  as_tibble() %>% 
  full_join(dat_div_gen_sqs %>% 
              as_tibble()) %>% 
  write_rds(here("data",
                 "diversity_continuous_sqs.rds"), 
            compress = "gz")

