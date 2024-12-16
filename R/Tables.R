library(readxl)
library(tidyverse)
library(xlsx)
library(here)


# read data ---------------------------------------------------------------


# species level
div <- read_xlsx(here("data", 
                      "DataS1.xlsx"))

# genus level
div_gen <- read_xlsx(here("data", 
                          "taxa_per_stage_genus.xlsx"))

# different metrics on species level
div_metric <- read_xlsx(here("data", 
                             "DataS2.xlsx"))

# different metrics on genus level
div_metric_gen <- read_xlsx(here("data", 
                             "taxa_per_stage_per_metric_genus.xlsx"))
                        
                        
# per superorder ----------------------------------------------------------

# select superorder
div %>% 
  filter(taxon %in% c("Neoselachii", "Selachii", "Batoidea")) %>%
  select(taxon, stage, mean_div) %>%
  group_by(taxon) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Berriasian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "Table1.xlsx")


# per order ---------------------------------------------------------------

# select order
div %>% 
  filter(!taxon %in% c("Neoselachii", "Selachii", "Batoidea")) %>%
  select(taxon, stage, mean_div) %>%
  group_by(taxon) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Valanginian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "Table2.xlsx")



# superorder genus level --------------------------------------------------

# select superorder
div_gen %>% 
  filter(taxon %in% c("Neoselachii", "Selachii", "Batoidea")) %>%
  select(taxon, stage, mean_div) %>%
  add_row(tibble(taxon = "Selachii", stage = "Berriasian", mean_div = 1)) %>% 
  group_by(taxon) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Berriasian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "TableS1.xlsx")


# per order genus level ---------------------------------------------------------------

# select order
div_gen %>% 
  filter(!taxon %in% c("Neoselachii", "Selachii", "Batoidea")) %>%
  select(taxon, stage, mean_div) %>%
  group_by(taxon) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Valanginian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "TableS2.xlsx")



# metric comparison species -----------------------------------------------

div_metric %>% 
  select(metric = Metric, stage, mean_div) %>%
  group_by(metric) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Berriasian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "TableS3.xlsx")


# metric comparison genus -----------------------------------------------

div_metric_gen %>% 
  select(metric, stage, mean_div) %>%
  group_by(metric) %>% 
  pivot_wider(names_from = stage, 
              values_from = mean_div) %>% 
  summarise(climb = -(100 - ((Campanian * 100) / Berriasian)), 
            descent = -(100 - ((Selandian * 100) / Campanian)), 
            max = -(100 - ((Lutetian * 100) / Selandian)), 
            fall_1 = -(100 - ((Chattian * 100) / Lutetian)), 
            fall_2 = -(100 - ((Pliocene * 100) /  Chattian)), 
            fall_3 = -(100 - ((Pleistocene * 100) / Pliocene)), 
            fall = -(100 - ((Pleistocene * 100) / Lutetian))) %>% 
  write.xlsx(file = "TableS4.xlsx")

