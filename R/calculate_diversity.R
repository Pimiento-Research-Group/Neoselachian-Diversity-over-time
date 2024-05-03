library(here)
library(divDyn)


# read data ---------------------------------------------------------------

dat_species <- readRDS(here("data",
                            "species_binned_stages.rds"))

dat_genus <- readRDS(here("data",
                          "genus_binned_stages.rds"))

# stage data
data(stages)


# calculate diversity metrices --------------------------------------------

dat_div_spec <- divDyn(dat_species, 
                       bin = "stg", 
                       tax = "accepted_name")

dat_div_spec$mid_age <- stages$mid[1:94]

dat_div_spec <- dat_div_spec[70:94, c("stg", "mid_age",
                                      "divRT", "divBC", "divSIB")]


dat_div_gen <- divDyn(dat_genus, 
                       bin = "stg", 
                       tax = "accepted_name")


dat_div_gen$mid_age <- stages$mid[1:94]

dat_div_gen <- dat_div_gen[70:94, c("stg", "mid_age",
                                    "divRT", "divBC", "divSIB")]


# save data ---------------------------------------------------------------

write.csv(dat_div_spec, here("data", 
                             "species_diversity_stages.csv"))


write.csv(dat_div_gen, here("data", 
                             "genus_diversity_stages.csv"))
