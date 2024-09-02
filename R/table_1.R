library(here)
library(tidyverse)
library(writexl)

# read data ---------------------------------------------------------------

# neoselachii
dat_deep_species <- read_csv(here("data",
                                  "deepdive_species",
                                  "all_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "all_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))

# batoidea
dat_bat_species <- read_csv(here("data",
                                 "deepdive_species",
                                 "batoidea_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "batoidea_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))


# selachii
dat_sel_species <- read_csv(here("data",
                                 "deepdive_species",
                                 "selachii_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "selachii_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))




# calculate percentage change ---------------------------------------------

# neoselachii
dat_perc <- dat_deep_species %>% 
  pivot_wider(names_from = start_age, 
              values_from = mean_div) %>% 
  add_column(type = "Neoselachii") %>% 
  bind_rows(# selachii
    dat_sel_species %>% 
      pivot_wider(names_from = start_age, 
                  values_from = mean_div) %>% 
      add_column(`145` = 1, 
                 type = "Selachii")) %>% 
  bind_rows(# batiodea
    dat_bat_species %>% 
              pivot_wider(names_from = start_age, 
                          values_from = mean_div) %>% 
            add_column(type = "Batoidea")) %>% 
  mutate(cret_rise = ((`83.6` - `145`) / `145`)*100, # Berriasian - Campanian
            cret_paleo_desc = ((`61.6` - `83.6`) / `83.6`)*100, # Campanian - Selandian
            KPg = ((`61.6` - `72.1`) / `72.1`)*100, # Maastrichtian - Danian
            pal_eo = ((`47.8` - `61.6`) / `61.6`)*100, # Selandian - Lutetian
            eo_rec = ((`0` - `47.8`) / `47.8`)*100) %>% # Lutetian - Recent
  select(cret_rise, cret_paleo_desc, KPg, pal_eo, eo_rec, type) %>% 
  pivot_longer(-type, names_to = "Phase") %>% 
  pivot_wider(values_from = value, 
              names_from = type) %>% 
  add_column(Start = c(145, 83.6, 72.1, 61.6, 47.8),
             End = c(83.6, 61.6, 61.6, 47.8, 0),
             .after = "Phase") %>% 
  mutate(across(c("Neoselachii", "Selachii", "Batoidea"), 
                ~ round(.x, 0)))


# save as excel table
dat_perc %>% 
  write_xlsx(here("data", "table_1.xlsx"))
