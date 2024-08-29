library(tidyverse)
library(here)
library(patchwork)
library(deeptime)
library(pammtools)

# read data ---------------------------------------------------------------

dat_all_species <- read_csv(here("data",
                                 "deepdive_species",
                                 "all_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "all_128.csv")) %>% 
              add_column(model = "128"))

dat_bat_species <- read_csv(here("data",
                                 "deepdive_species",
                                 "batoidea_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "batoidea_128.csv")) %>% 
              add_column(model = "128"))

dat_sel_species <- read_csv(here("data",
                                 "deepdive_species",
                                 "selachii_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "selachii_128.csv")) %>% 
              add_column(model = "128"))


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

# set up stages for plotting
data(stages, package = "deeptime")

stage_cor <- stages %>%
  as_tibble() %>% 
  filter(!name  %in% c("Meghalayan", "Northgrippian", "Greenlandian", 
                       "Late Pleistocene", "Chibanian", "Calabrian",
                       "Piacenzian", "Zanclean", "Gelasian"))

# set up function ---------------------------------------------------------

plot_div <- function(data_set, 
                     colour_man, 
                     taxon,
                     show_geoscale = FALSE) {
  
  data_set %>%
    rowid_to_column("run") %>% 
    pivot_longer(cols = -c(run, model), 
                 names_to = "start_age", 
                 values_to = "DeepDive") %>% 
    group_by(start_age) %>% 
    reframe(quant = quantile(DeepDive, 
                             probs = c(0.25, 0.5, 0.75)), 
            quart = c("ymin", "y", "ymax")) %>% 
    pivot_wider(values_from = quant, 
                names_from = quart) %>%  
    mutate(start_age = as.double(start_age)) %>% 
    ggplot(aes(start_age, y)) +
    geom_vline(xintercept = epoch_age,
               colour = "grey95", 
               linewidth = 0.4) +
    geom_stepribbon(aes(ymin = ymin, 
                        ymax = ymax), 
                    alpha = 0.15, 
                    fill = colour_man, 
                    colour = "white", 
                    linewidth = 0) +
    geom_step(colour = colour_man) +
    labs(y = "Species Diversity",
         x = "Myr",
         colour = NULL, 
         title = taxon) +
    scale_x_reverse(breaks = seq(140, 0, by = -20), 
                    limits = c(145, 0)) +
    scale_y_continuous(limits = c(0, NA)) +
    {if(show_geoscale) 
      coord_geo(dat = list(stage_cor, 
                           "epochs", 
                           "periods"),
                pos = list("b", "b", "b"),
                alpha = 0.2,
                height = list(unit(1.25, "line"), 
                              unit(0.75, "line"), 
                              unit(0.75, "line")),
                size = list(6/.pt, 6/.pt, 6/.pt),
                lab_color = "grey20",
                color = "grey20",
                abbrv = list(TRUE, TRUE, FALSE),
                rot = list(90, 0, 0),
                fill = "white",
                expand = FALSE,
                lwd = list(0.1, 0.1, 0.1))} +
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
          panel.grid.minor = element_blank(), 
          plot.title = element_text(size = 11)) +
    {if(!show_geoscale)
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())}
  
}


# plot --------------------------------------------------------------------

# all
plot_all <- plot_div(dat_all_species,
                     colour_man = "#F95875", 
                     taxon = "Neoselachii")
# selachii
plot_sel <- plot_div(dat_sel_species, 
                     colour_man = "#681270", 
                     taxon = "Selachii")
# batoidea
plot_bat <- plot_div(dat_bat_species,
                     colour_man = "#2F899D", 
                     show_geoscale = TRUE, 
                     taxon = "Batoidea")


# patch together
fig_1 <- plot_all/ plot_sel/ plot_bat +
  plot_annotation(tag_levels = "A")

# save
ggsave(fig_1, 
       filename = here("figures",
                       "fig_1.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")



# per order ---------------------------------------------------------------

# get csv names
plot_list_order <- list.files(path = here("data",
                                    "deepdive_order_species"),
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE) %>% 
  map(~read_csv(.x))

plot_list_order <- list(order_data = list.files(path = here("data",
                                         "deepdive_order_species"),
                             recursive = TRUE,
                             pattern = "\\.csv$",
                             full.names = TRUE) %>% 
       map(~read_csv(.x)), 
     order_names = list.files(path = here("data",
                                          "deepdive_order_species"),
                              recursive = TRUE,
                              pattern = "\\.csv$",
                              full.names = FALSE) %>% 
       str_remove_all(".csv"), 
     order_colour = c(rep("#681270", 5), 
                         rep("#2F899D", 2))) %>% 
  pmap(., 
       function(order_data,  order_colour, order_names){
         plot_div(order_data, 
               colour_man = order_colour, 
               taxon = order_names)})

# patch together
fig_2 <- (plot_list_order[[1]] / plot_list_order[[2]] / 
  plot_list_order[[3]] /plot_list_order[[4]]) |
  (plot_list_order[[5]] / plot_list_order[[6]] / 
  plot_list_order[[7]] / plot_spacer()) +
  plot_annotation(tag_levels = "A")


# save
ggsave(fig_2, 
       filename = here("figures",
                       "fig_2.pdf"), 
       width = 183*2, height = 100*5,
       units = "mm", 
       bg = "white")
