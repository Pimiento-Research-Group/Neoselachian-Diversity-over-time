library(tidyverse)
library(here)


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
         name = str_replace_all(name, "Pleistocene", "Ple"))


# set up stages for plotting
data(stages, package = "deeptime")

stage_cor <- stages %>%
  as_tibble() %>% 
  filter(!name  %in% c("Meghalayan", "Northgrippian", "Greenlandian", 
                       "Late Pleistocene", "Chibanian", "Calabrian",
                       "Piacenzian", "Zanclean", "Gelasian", "Aptian",
                       "Barremian")) %>% 
  add_row(epoch_cor %>% 
            filter(name %in% c("Pleistocene", "Pliocene"))) %>% 
  add_row(tibble(name = c("Aptian", "Barremian"),
                 max_age = c(125, 129.400), 
                 min_age = c(113, 125), 
                 abbr = c("Ap", "Brrm"), 
                 color = c("#BFE48A", "#B3DF7F"))) %>% 
  arrange(max_age)


# save data ---------------------------------------------------------------

write_rds(epoch_age, here("data", 
                          "epoch_age.rds"))

write_rds(epoch_cor, here("data", 
                          "epoch_cor.rds"))

write_rds(stage_cor, here("data", 
                          "stage_cor.rds"))
