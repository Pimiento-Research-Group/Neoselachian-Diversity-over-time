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

# attach the time scale object
data(stages)



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
        mutate(stg = 95 - cut(max_ma, 
                              breaks = stages$bottom, 
                              include.lowest = TRUE, 
                              labels = FALSE)) %>% 
        divDyn(., bin = "stg", tax = "accepted_name") %>% 
        as_tibble() %>% 
        add_column(mid_age = stages$mid[1:94]) %>% 
        filter(between(stg, 70, 94)) %>% 
        select(stg, mid_age,
               divRT, divBC, divSIB) %>% 
        rename_with(~str_replace(., "div", tax_level), contains("div")) %>% 
        add_column(run = i)
    }
    
  } else if (div_metric == "sqs") {
    
    for (i in 1:100) {
      list_div[[i]] <- dat_age %>% 
        mutate(age_sel = map_dbl(age_mid, 
                                 ~pluck(.x, i))) %>% 
        mutate(stg = 95 - cut(max_ma, 
                              breaks = stages$bottom, 
                              include.lowest = TRUE, 
                              labels = FALSE)) %>% 
        drop_na(stg) %>% 
        subsample(bin = "stg",
                  tax = "accepted_name",
                  iter = 100, q = 0.3, type = "sqs", 
                  ref = "reference_no",
                  singleton = "ref") %>% 
        as_tibble() %>% 
        add_column(mid_age = stages$mid[1:94]) %>% 
        filter(between(stg, 70, 94)) %>% 
        select(stg, mid_age,
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
  distinct(pick(-contains("run"))) %>% 
  full_join(dat_div_gen %>% 
              distinct(pick(-contains("run")))) %>% 
  pivot_longer(cols = -c(stg, mid_age), 
               names_to = "metric", 
               values_to = "diversity") %>% 
  write_csv(here("data",
                 "diversity_continuous_raw.csv"))

dat_div_spec_sqs %>% 
  full_join(dat_div_gen_sqs) %>% 
  pivot_longer(cols = -c(stg, mid_age, run), 
               names_to = "metric", 
               values_to = "diversity") %>% 
  write_csv(here("data",
                 "diversity_continuous_sqs.csv"))

