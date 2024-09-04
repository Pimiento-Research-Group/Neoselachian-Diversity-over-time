library(here)
library(tidyverse)
library(writexl)

# read data ---------------------------------------------------------------

# neoselachii
dat_neo_species <- read_csv(here("data",
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

# per order
dat_ord_species <- list.files(path = here("data", "deepdive_order_species"),
           recursive = TRUE,
           pattern = "\\.csv$",
           full.names = TRUE) %>% 
  map(function(.x) {
    dat <- read_csv(.x, show_col_types = FALSE) 
    colnames(dat) <- rev(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
                           89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
                           47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
                           13.820, 11.630, 7.246, 5.333, 2.580, 0))
    return(dat)
  }) %>% 
  bind_rows() %>% 
  mutate(taxon = rep(list.files(path = here("data", "deepdive_order_species"),
                                recursive = TRUE,
                                pattern = "\\.csv$",
                                full.names = FALSE) %>% 
                       word(., sep = "_") %>% 
                       str_to_sentence(), 
                     each = 100)) %>% 
  replace_na(list(`145` = 0)) %>% 
  pivot_longer(cols = -taxon, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(taxon, start_age) %>% 
  summarise(mean_div = mean(DeepDive))



# set up stages -----------------------------------------------------------

# set up epochs for plotting
data(epochs, package = "deeptime")

epoch_cor <- epochs %>%
  as_tibble() %>% 
  filter(between(max_age, 0, 145))

# set up stages for plotting
data(stages, package = "deeptime")

stages_cor <- stages %>% 
  as_tibble() %>% 
  filter(!name  %in% c("Meghalayan", "Northgrippian", "Greenlandian", 
                       "Late Pleistocene", "Chibanian", "Calabrian",
                       "Piacenzian", "Zanclean", "Gelasian")) %>% 
  add_row(epoch_cor %>% 
            filter(name %in% c("Pleistocene", "Pliocene"))) %>% 
  arrange(max_age) %>% 
  select(stage = name, start_age = max_age) 

# combine in one dataframe ------------------------------------------------

dat_neo_species %>% 
  add_column(taxon = "Neoselachii", 
             .before = "start_age") %>% 
  bind_rows(dat_bat_species %>% 
              add_column(taxon = "Batoidea", 
                         .before = "start_age")) %>% 
  bind_rows(dat_sel_species %>% 
              add_column(taxon = "Selachii", 
                         .before = "start_age")) %>% 
  bind_rows(dat_ord_species) %>% 
  filter(start_age > 0) %>% 
  left_join(stages_cor) %>% 
  write_xlsx(here("data", "taxa_per_stage_species.xlsx"))
  


#  same for genus level ---------------------------------------------------

# neoselachii
dat_neo_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "all_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "all_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))

# batoidea
dat_bat_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "batoidea_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "batoidea_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))


# selachii
dat_sel_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "selachii_64.csv")) %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "selachii_128.csv"))) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(DeepDive))

# per order
dat_ord_genus <- list.files(path = here("data", "deepdive_order_genus"),
                              recursive = TRUE,
                              pattern = "\\.csv$",
                              full.names = TRUE) %>% 
  map(function(.x) {
    dat <- read_csv(.x, show_col_types = FALSE) 
    colnames(dat) <- rev(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
                           89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
                           47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
                           13.820, 11.630, 7.246, 5.333, 2.580, 0))
    return(dat)
  }) %>% 
  bind_rows() %>% 
  mutate(taxon = rep(list.files(path = here("data", "deepdive_order_genus"),
                                recursive = TRUE,
                                pattern = "\\.csv$",
                                full.names = FALSE) %>% 
                       word(., sep = "_") %>% 
                       str_to_sentence(), 
                     each = 100)) %>% 
  replace_na(list(`145` = 0)) %>% 
  pivot_longer(cols = -taxon, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(taxon, start_age) %>% 
  summarise(mean_div = mean(DeepDive))

# combine in one dataframe 
dat_neo_genus %>% 
  add_column(taxon = "Neoselachii", 
             .before = "start_age") %>% 
  bind_rows(dat_bat_genus %>% 
              add_column(taxon = "Batoidea", 
                         .before = "start_age")) %>% 
  bind_rows(dat_sel_genus %>% 
              add_column(taxon = "Selachii", 
                         .before = "start_age")) %>% 
  bind_rows(dat_ord_genus) %>% 
  filter(start_age > 0) %>% 
  left_join(stages_cor) %>% 
  write_xlsx(here("data", "taxa_per_stage_genus.xlsx"))


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
