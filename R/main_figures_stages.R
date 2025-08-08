library(tidyverse)
library(here)
library(patchwork)
library(deeptime)
library(pammtools)

# read data ---------------------------------------------------------------

dat_all_species <- read_csv(here("data",
                                 "stage_data",
                                 "deepdive_species",
                                 "all.csv")) 

dat_bat_species <- read_csv(here("data",
                                 "stage_data",
                                 "deepdive_species",
                                 "batoidea.csv")) 

dat_sel_species <- read_csv(here("data",
                                 "stage_data",
                                 "deepdive_species",
                                 "selachimorpha.csv"))

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
    pivot_longer(cols = -c(run), 
                 names_to = "start_age", 
                 values_to = "DeepDive") %>% 
    group_by(start_age) %>% 
    summarise(ymin = min(DeepDive, na.rm = TRUE), 
              y = mean(DeepDive, na.rm = TRUE), 
              ymax = max(DeepDive, na.rm = TRUE)) %>% 
    mutate(start_age = as.double(start_age)) %>% 
    ggplot(aes(start_age, y)) +
    geom_hline(yintercept = 0, 
               linewidth = 0.1, 
               colour = "white") +
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
                abbrv = list(TRUE, FALSE, FALSE),
                rot = list(90, 0, 0),
                # fill = "white",
                expand = FALSE,
                lwd = list(0.1, 0.1, 0.1))} +
    {if(!show_geoscale) 
      coord_geo(dat = "epochs",
                pos = "b",
                alpha = 0,
                lab_color = "white",
                color = NA,
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
                     taxon = "Selachimorpha")
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
                       "fig_1_stages.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")




# per genus ---------------------------------------------------------------

# read data 
dat_all_genus <- read_csv(here("data",
                               "stage_data",
                               "deepdive_genus",
                               "all.csv")) 

dat_bat_genus <- read_csv(here("data",
                               "stage_data",
                               "deepdive_genus",
                               "batoidea.csv"))

dat_sel_genus <- read_csv(here("data",
                               "stage_data",
                               "deepdive_genus",
                               "selachimorpha.csv"))
# all
plot_all <- plot_div(dat_all_genus,
                     colour_man = "#F95875", 
                     taxon = "Neoselachii", 
                     y_label = "Genus Diversity")
# selachii
plot_sel <- plot_div(dat_sel_genus, 
                     colour_man = "#681270", 
                     taxon = "Selachimorpha", 
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
                       "fig_S3_stages.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")


