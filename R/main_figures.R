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


# stage data
epoch_age <- read_rds(here("data", 
                          "epoch_age.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                          "stage_cor.rds"))

  
# set up function ---------------------------------------------------------

plot_div <- function(data_set, 
                     colour_man, 
                     taxon,
                     show_geoscale = FALSE, 
                     y_label = "Species Diversity") {
  
  data_set %>%
    rowid_to_column("run") %>% 
    pivot_longer(cols = -c(run, model), 
                 names_to = "start_age", 
                 values_to = "DeepDive") %>% 
    group_by(start_age) %>% 
    summarise(ymin = min(DeepDive, na.rm = TRUE), 
              y = mean(DeepDive, na.rm = TRUE), 
              ymax = max(DeepDive, na.rm = TRUE)) %>% 
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
                    linewidth = 0.001) +
    geom_step(colour = colour_man) +
    labs(y = y_label,
         x = "Time (Ma)",
         colour = NULL, 
         title = taxon) +
    scale_x_reverse(breaks = seq(140, 0, by = -20), 
                    limits = c(145, 0)) +
    scale_y_continuous(limits = c(0, NA)) +
    {if(show_geoscale) 
      coord_geo(dat = list(stage_cor, 
                           epoch_cor, 
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
                # fill = "white",
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


# # added holocene
# plot_holo <- read_csv(here("data",
#               "deepdive_species",
#               "all_holo_64.csv")) %>% 
#   add_column(model = "64") %>% 
#   bind_rows(read_csv(here("data",
#                           "deepdive_species",
#                           "all_holo_128.csv")) %>% 
#               add_column(model = "128")) %>% 
#   plot_div(colour_man = "#F95875", 
#            show_geoscale = TRUE, 
#            taxon = "Neoselachii [Holocene]")
# # save
# ggsave(plot_holo, 
#        filename = here("figures",
#                        "species_holocene.pdf"), 
#        width = 183, height = 100,
#        units = "mm", 
#        bg = "white")


# per order ---------------------------------------------------------------

# get csv names
list_order <- list.files(path = here("data", "deepdive_order_species"),
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
  mutate(model = rep(list.files(path = here("data", "deepdive_order_species"),
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = FALSE) %>% 
           word(., sep = "_") %>% 
           str_to_sentence(), 
           each = 100)) %>% 
  replace_na(list(`145` = 0)) %>% 
  group_split(model)


# plot
plot_list_order <- list(order_data = list_order, 
                        order_names = list.files(path = here("data", "deepdive_order_species"),
                                                 recursive = TRUE,
                                                 pattern = "\\.csv$",
                                                 full.names = FALSE) %>% 
                          word(., sep = "_") %>% 
                          str_to_sentence() %>% 
                          unique(), 
                        order_colour = c(rep("#681270", 3), 
                                         "#2F899D", "#681270", 
                                         "#2F899D", "#2F899D", "#681270"), 
                        order_x_axis = c(FALSE, FALSE, FALSE, FALSE, 
                                         TRUE, FALSE, TRUE, FALSE)) %>% 
  pmap(., 
       function(order_data,  order_colour, 
                order_names, order_x_axis){
         plot_div(order_data, 
                  colour_man = order_colour, 
                  taxon = order_names, 
                  show_geoscale = order_x_axis)})

# patch together
fig_2 <- (plot_list_order[[1]] / plot_list_order[[2]] /
  plot_list_order[[3]] /plot_list_order[[5]] |
  plot_list_order[[8]] / plot_list_order[[4]] / 
  plot_list_order[[6]] / plot_list_order[[7]]) +
  plot_annotation(tag_levels = "A")


# save
ggsave(fig_2, 
       filename = here("figures",
                       "fig_2.pdf"), 
       width = 183*2, height = 100*3,
       units = "mm", 
       bg = "white")



# per genus ---------------------------------------------------------------

# read data 
dat_all_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "all_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "all_128.csv")) %>% 
              add_column(model = "128"))

dat_bat_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "batoidea_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "batoidea_128.csv")) %>% 
              add_column(model = "128"))

dat_sel_genus <- read_csv(here("data",
                                 "deepdive_genus",
                                 "selachii_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "selachii_128.csv")) %>% 
              add_column(model = "128"))
# all
plot_all <- plot_div(dat_all_genus,
                     colour_man = "#F95875", 
                     taxon = "Neoselachii", 
                     y_label = "Genus Diversity")
# selachii
plot_sel <- plot_div(dat_sel_genus, 
                     colour_man = "#681270", 
                     taxon = "Selachii", 
                     y_label = "Genus Diversity")
# batoidea
plot_bat <- plot_div(dat_bat_genus,
                     colour_man = "#2F899D", 
                     show_geoscale = TRUE, 
                     taxon = "Batoidea", 
                     y_label = "Genus Diversity")


# patch together
fig_S3 <- plot_all/ plot_sel/ plot_bat +
  plot_annotation(tag_levels = "A")

# save
ggsave(fig_S3, 
       filename = here("figures",
                       "fig_S3.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")


# per order 

# get csv names
list_order <- list.files(path = here("data", "deepdive_order_genus"),
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
  mutate(model = rep(list.files(path = here("data", "deepdive_order_genus"),
                                recursive = TRUE,
                                pattern = "\\.csv$",
                                full.names = FALSE) %>% 
                       word(., sep = "_") %>% 
                       str_to_sentence(), 
                     each = 100)) %>% 
  replace_na(list(`145` = 0)) %>% 
  group_split(model)


# plot
plot_list_order <- list(order_data = list_order, 
                        order_names = list.files(path = here("data", "deepdive_order_genus"),
                                                 recursive = TRUE,
                                                 pattern = "\\.csv$",
                                                 full.names = FALSE) %>% 
                          word(., sep = "_") %>% 
                          str_to_sentence() %>% 
                          unique(), 
                        order_colour = c(rep("#681270", 3), 
                                         "#2F899D", "#681270", 
                                         "#2F899D", "#2F899D", "#681270"), 
                        order_x_axis = c(FALSE, FALSE, FALSE, FALSE, 
                                         TRUE, FALSE, TRUE, FALSE)) %>% 
  pmap(., 
       function(order_data,  order_colour, 
                order_names, order_x_axis){
         plot_div(order_data, 
                  colour_man = order_colour, 
                  taxon = order_names, 
                  show_geoscale = order_x_axis, 
                  y_label = "Genus Diversity")})

# patch together
fig_s4 <- (plot_list_order[[1]] / plot_list_order[[2]] /
            plot_list_order[[3]] /plot_list_order[[5]] |
            plot_list_order[[8]] / plot_list_order[[4]] / 
            plot_list_order[[6]] / plot_list_order[[7]]) +
  plot_annotation(tag_levels = "A")


# save
ggsave(fig_s4, 
       filename = here("figures",
                       "fig_S4.pdf"), 
       width = 183*2, height = 100*3,
       units = "mm", 
       bg = "white")
