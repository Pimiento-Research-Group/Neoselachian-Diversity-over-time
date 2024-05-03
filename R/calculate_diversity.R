library(here)
library(tidyverse)
library(divDyn)


# read data ---------------------------------------------------------------

dat_species <- readRDS(here("data",
                            "species_binned_stages.rds"))

dat_genus <- readRDS(here("data",
                          "genus_binned_stages.rds"))

# stage data
data(stages)


# calculate raw diversity metrices ----------------------------------------


dat_div_spec <- divDyn(dat_species,
                       bin = "stg",
                       tax = "accepted_name") %>% 
  as_tibble() %>% 
  add_column(mid_age = stages$mid[1:94]) %>% 
  filter(between(stg, 70, 94)) %>% 
  select(stg, mid_age, 
         RT_species = divRT, BC_species = divBC, 
         SIB_species = divSIB)


dat_div_gen <- divDyn(dat_genus, 
                      bin = "stg",
                      tax = "accepted_name") %>% 
  as_tibble() %>% 
  add_column(mid_age = stages$mid[1:94]) %>% 
  filter(between(stg, 70, 94)) %>% 
  select(stg, mid_age, 
         RT_genus = divRT, BC_genus = divBC, 
         SIB_genus = divSIB)


# calculate sqs metrices --------------------------------------------

dat_div_spec_sqs <- subsample(dat_species,
                          bin = "stg",
                          tax = "accepted_name",
                          iter = 1000, q = 0.3, type = "sqs", 
                          ref = "reference_no",
                          singleton = "ref") %>% 
  as_tibble() %>% 
  add_column(mid_age = stages$mid[1:94]) %>% 
  filter(between(stg, 70, 94)) %>% 
  select(stg, mid_age, 
         RT_species_sqs = divRT, BC_species_sqs = divBC, 
         SIB_species_sqs = divSIB)


dat_div_gen_sqs <- subsample(dat_genus, 
                      bin = "stg",
                      tax = "accepted_name",
                      iter = 1000, q = 0.3, type = "sqs", 
                      ref = "reference_no",
                      singleton = "ref") %>% 
  as_tibble() %>% 
  add_column(mid_age = stages$mid[1:94]) %>% 
  filter(between(stg, 70, 94)) %>% 
  select(stg, mid_age, 
         RT_genus_sqs = divRT, BC_genus_sqs = divBC, 
         SIB_genus_sqs = divSIB)



# save data ---------------------------------------------------------------

# merge together
dat_full <- dat_div_gen %>% 
  full_join(dat_div_spec) %>% 
  full_join(dat_div_gen_sqs) %>% 
  full_join(dat_div_spec_sqs) %>% 
  pivot_longer(cols = -c(stg, mid_age), 
               names_to = "metric", 
               values_to = "diversity")

write.csv(dat_full, here("data",
                         "diversity_stages.csv"))


write.csv(dat_div_gen, here("data", 
                             "genus_diversity_stages.csv"))
