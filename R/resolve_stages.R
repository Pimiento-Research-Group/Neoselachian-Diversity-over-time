library(tidyverse)
library(here)

# set up epochs for plotting
data("periods", package = "deeptime")

periods_cor <- periods %>%
  as_tibble() %>% 
  mutate(max_age = ifelse(name == "Cretaceous", 150, max_age)) %>% 
  filter(max_age <= 150)

# set up epochs for plotting
data(stages, package = "divDyn")

# get epoch age into epochs
epoch_age <- stages %>% 
  as_tibble() %>% 
  group_by(series) %>% 
  summarise(epoch = min(top)) %>% 
  arrange(epoch) %>% 
  filter(epoch <= 140) %>% 
  pull(epoch) %>% 
  {.[-c(1, 2)]}

# set up epochs for plotting
data(epochs, package = "deeptime")

epoch_cor <- epochs %>%
  as_tibble() %>% 
  filter(between(max_age, 0, 145)) %>% 
  mutate(color = c("#FEF6F2", 
                   "#FFF1C4", 
                   "#FFF7B2", 
                   "#FFED00", 
                   "#FBCC98", 
                   "#FAC18A", 
                   "#F8B77D", 
                   "#BAD25F", 
                   "#A0C96D")) %>% 
  mutate(abbr = str_replace_all(abbr, "LC", "UC"), 
         abbr = str_replace_all(abbr, "EC", "LC"), 
         name = str_replace_all(name, "Late Cretaceous", "Upper Cretaceous"), 
         name = str_replace_all(name, "Early Cretaceous", "Lower Cretaceous"), 
         name = str_replace_all(name, "Pliocene", "Pli"), 
         name = str_replace_all(name, "Pleistocene", "Ple")) %>% 
  mutate(max_age = ifelse(name == "Lower Cretaceous", 145.0, max_age))


# set up stages for plotting
data(stages, package = "deeptime")

stage_cor <- stages %>%
  as_tibble() %>% 
  filter(!name  %in% c("Meghalayan", "Northgrippian", "Greenlandian", 
                       "Late Pleistocene", "Chibanian", "Calabrian",
                       "Piacenzian", "Zanclean", "Gelasian", "Aptian",
                       "Barremian", "Berriasian", "Hauterivian", "Valanginian")) %>% 
  add_row(epoch_cor %>% 
            filter(name %in% c("Ple", "Pli"))) %>% 
  add_row(tibble(name = c("Aptian", "Barremian", "Hauterivian", "Valanginian", "Berriasian"),
                 max_age = c(121.4, 129.400, 132.6, 139.8, 145.0), 
                 min_age = c(113.2, 121.4, 129.4, 132.6, 139.8), 
                 abbr = c("Ap", "Brrm", "Htr", "Vl", "Brrs"), 
                 lab_color = "black", 
                 color = c("#BFE48A", "#B3DF7F", "#A6D975", "#99D36A", "#8CCD60")))%>%
 
  arrange(max_age) %>% 
  filter(max_age <= 145) 

# save data ---------------------------------------------------------------

write_rds(epoch_age, here("data", 
                          "epoch_age.rds"))

write_rds(periods_cor, here("data", 
                          "periods_cor.rds"))

write_rds(epoch_cor, here("data", 
                          "epoch_cor.rds"))

write_rds(stage_cor, here("data", 
                          "stage_cor.rds"))
