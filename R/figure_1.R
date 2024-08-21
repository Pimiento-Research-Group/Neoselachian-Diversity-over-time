library(tidyverse)
library(here)
library(patchwork)
library(deeptime)


# read data ---------------------------------------------------------------


dat_bat_species <- read_csv(here("data",
                                 "deepdive_batoidea_species.csv")) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age))

dat_sel_species <- read_csv(here("data",
                                 "deepdive_selachii_species.csv")) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age))

data(stages, package = "divDyn")

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



# load svgs ---------------------------------------------------------------


# set up function ---------------------------------------------------------

plot_div <- function(data_set, 
                     colour_man, 
                     show_geoscale = FALSE) {
  
  data_set %>%
    ggplot(aes(start_age, DeepDive)) +
    geom_vline(xintercept = epoch_age,
               colour = "grey90") +
    geom_step(aes(group = run), 
              alpha = 0.05, 
              colour = colour_man) +
    geom_step(data = . %>% 
                group_by(start_age) %>% 
                summarise(DeepDive = mean(DeepDive)), 
              linewidth = 0.3, 
              colour = colour_man) +
    labs(y = "Number of Species",
         x = "Myr",
         colour = NULL) +
    scale_x_reverse(breaks = seq(140, 0, by = -20), 
                    limits = c(145, 0)) +
    {if(show_geoscale) 
      coord_geo(dat = list("epochs", 
                         "periods"),
              pos = list("b", "b"),
              alpha = 0.2,
              height = list(unit(0.5, "line"), 
                            unit(1, "line")),
              size = list(6/.pt, 10/.pt),
              lab_color = "grey40",
              color = "grey40",
              abbrv = list(TRUE, TRUE),
              fill = "white",
              expand = FALSE,
              lwd = list(0.1, 0.4))} +
    {if(!show_geoscale) 
      coord_geo(dat = "epochs",
                pos = "b",
                alpha = 0,
                lab_color = "white",
                color = "white",
                abbrv = TRUE,
                fill = "white",
                expand = FALSE)} +
    theme_minimal() +
    theme(legend.position = "none",
          axis.ticks = element_line(colour = "grey50"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    {if(!show_geoscale)
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())}
  
}

# plot --------------------------------------------------------------------

# all
#F95875

# selachii
plot_sel <- plot_div(dat_sel_species, 
                     colour_man = "#2F899D")
# batoidea
plot_bat <- plot_div(dat_bat_species,
                     colour_man = "#681270", 
                     show_geoscale = TRUE)


# patch together
plot_sel/ plot_bat +
  plot_annotation(tag_levels = "A")
