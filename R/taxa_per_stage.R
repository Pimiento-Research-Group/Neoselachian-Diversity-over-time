library(here)
library(tidyverse)
library(writexl)

# read data ---------------------------------------------------------------

# set up function
get_data <- function(file_order, 
                     taxon_name) {
  read_csv(here("data",
                file_order,
                paste0(taxon_name, ".csv"))) %>% 
    rowid_to_column("run") %>% 
    pivot_longer(cols = -run, 
                 names_to = "start_age", 
                 values_to = "DeepDive") %>% 
    mutate(start_age = as.double(start_age)) %>% 
    group_by(start_age) %>% 
    summarise(mean_div = mean(DeepDive), 
              min_div = min(DeepDive), 
              max_div = max(DeepDive))
  
}


# neoselachii
dat_neo_species <- get_data("deepdive_species", "all")

# batoidea
dat_bat_species <- get_data("deepdive_species", "batoidea")

# selachii
dat_sel_species <- get_data("deepdive_species", "selachimorpha")

# per order
dat_ord_species <- list.files(path = here("data", "deepdive_order_species"),
                              recursive = TRUE,
                              pattern = "\\.csv$",
                              full.names = TRUE) %>% 
  map(function(.x) {
    dat <- read_csv(.x, show_col_types = FALSE)
    if(!"145.0" %in% colnames(dat)){
      dat <- add_column(dat, "145.0" = 0)
    }
    if("139.0" %in% colnames(dat)){
      dat <- select(dat, -"139.0")
    }
    if("132.0" %in% colnames(dat)){
      dat <- select(dat, -"132.0")
    }
    return(dat)
  }) %>%
  bind_rows() %>% 
  mutate(taxon = rep(list.files(path = here("data", "deepdive_order_species"),
                                recursive = TRUE,
                                pattern = "\\.csv$",
                                full.names = FALSE) %>% 
                       str_remove("\\.csv$") %>% 
                       str_to_sentence(), 
                     each = 400)) %>% 
  replace_na(list(`145` = 0)) %>% 
  pivot_longer(cols = -taxon, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(taxon, start_age) %>% 
  summarise(mean_div = mean(DeepDive), 
            min_div = min(DeepDive), 
            max_div = max(DeepDive))

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds")) %>% 
  select(stage = name, start_age = max_age)


# combine in one dataframe ------------------------------------------------

dat_neo_species %>% 
  add_column(taxon = "Neoselachii", 
             .before = "start_age") %>% 
  bind_rows(dat_bat_species %>% 
              add_column(taxon = "Batoidea", 
                         .before = "start_age")) %>% 
  bind_rows(dat_sel_species %>% 
              add_column(taxon = "Selachimorpha", 
                         .before = "start_age")) %>% 
  bind_rows(dat_ord_species) %>% 
  filter(start_age > 0) %>% 
  left_join(stage_cor) %>% 
  write_xlsx(here("data", "taxa_per_stage_species.xlsx"))
  

#  same for genus level ---------------------------------------------------

# neoselachii
dat_neo_genus <- get_data("deepdive_genus", "all")

# batoidea
dat_bat_genus <- get_data("deepdive_genus", "batoidea")

# selachii
dat_sel_genus <- get_data("deepdive_genus", "selachimorpha")

# per order
dat_ord_genus <- list.files(path = here("data", "deepdive_order_genus"),
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE) %>% 
  map(function(.x) {
    dat <- read_csv(.x, show_col_types = FALSE) 
    if(!"145.0" %in% colnames(dat)){
      dat <- add_column(dat, "145.0" = 0)
    }
    if("139.0" %in% colnames(dat)){
      dat <- select(dat, -"139.0")
    }
    if("132.0" %in% colnames(dat)){
      dat <- select(dat, -"132.0")
    }
    return(dat)
  }) %>% 
  bind_rows() %>% 
  mutate(taxon = rep(list.files(path = here("data", "deepdive_order_genus"),
                                recursive = TRUE,
                                pattern = "\\.csv$",
                                full.names = FALSE) %>% 
                       str_remove("\\.csv$") %>% 
                       str_to_sentence(), 
                     each = 400)) %>% 
  replace_na(list(`145` = 0)) %>% 
  pivot_longer(cols = -taxon, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(taxon, start_age) %>% 
  summarise(mean_div = mean(DeepDive), 
            min_div = min(DeepDive), 
            max_div = max(DeepDive))

# combine in one dataframe 
dat_neo_genus %>% 
  add_column(taxon = "Neoselachii", 
             .before = "start_age") %>% 
  bind_rows(dat_bat_genus %>% 
              add_column(taxon = "Batoidea", 
                         .before = "start_age")) %>% 
  bind_rows(dat_sel_genus %>% 
              add_column(taxon = "Selachimorpha", 
                         .before = "start_age")) %>% 
  bind_rows(dat_ord_genus) %>% 
  filter(start_age > 0) %>% 
  left_join(stage_cor) %>% 
  write_xlsx(here("data", "taxa_per_stage_genus.xlsx"))


# # calculate percentage change ---------------------------------------------
# 
# # neoselachii
# dat_perc <- dat_deep_species %>% 
#   pivot_wider(names_from = start_age, 
#               values_from = mean_div) %>% 
#   add_column(type = "Neoselachii") %>% 
#   bind_rows(# selachii
#     dat_sel_species %>% 
#       pivot_wider(names_from = start_age, 
#                   values_from = mean_div) %>% 
#       add_column(`145` = 1, 
#                  type = "Selachii")) %>% 
#   bind_rows(# batiodea
#     dat_bat_species %>% 
#               pivot_wider(names_from = start_age, 
#                           values_from = mean_div) %>% 
#             add_column(type = "Batoidea")) %>% 
#   mutate(cret_rise = ((`83.6` - `145`) / `145`)*100, # Berriasian - Campanian
#             cret_paleo_desc = ((`61.6` - `83.6`) / `83.6`)*100, # Campanian - Selandian
#             KPg = ((`61.6` - `72.1`) / `72.1`)*100, # Maastrichtian - Danian
#             pal_eo = ((`47.8` - `61.6`) / `61.6`)*100, # Selandian - Lutetian
#             eo_rec = ((`0` - `47.8`) / `47.8`)*100) %>% # Lutetian - Recent
#   select(cret_rise, cret_paleo_desc, KPg, pal_eo, eo_rec, type) %>% 
#   pivot_longer(-type, names_to = "Phase") %>% 
#   pivot_wider(values_from = value, 
#               names_from = type) %>% 
#   add_column(Start = c(145, 83.6, 72.1, 61.6, 47.8),
#              End = c(83.6, 61.6, 61.6, 47.8, 0),
#              .after = "Phase") %>% 
#   mutate(across(c("Neoselachii", "Selachii", "Batoidea"), 
#                 ~ round(.x, 0)))
# 
# 
# # save as excel table
# dat_perc %>% 
#   write_xlsx(here("data", "table_1.xlsx"))
# 
# 
# 
# # same for pyrate ---------------------------------------------------------
# 
# dat_pyrate <- read_rds(here("data", "pyrate_species_binned.rds"))
# 
#   