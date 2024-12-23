library(tidyverse)
library(here)
library(patchwork)
library(deeptime)
library(pammtools)


# read data ---------------------------------------------------------------


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
  mutate(start_age = as.double(start_age))

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds"))
# visualise ------------------------------------------------------------------

# reframe
dat_plot <-  dat_deep_species %>%
  group_by(start_age) %>% 
  summarise(ymin = min(DeepDive, na.rm = TRUE), 
            y = mean(DeepDive, na.rm = TRUE), 
            ymax = max(DeepDive, na.rm = TRUE)) %>%   
  mutate(start_age = as.double(start_age))

# line plot
plot_line <- dat_plot %>% 
  ggplot(aes(start_age, y)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_ribbon(aes(ymin = ymin, 
                  ymax = ymax), 
              alpha = 0.3, 
              fill = "grey20", 
              colour = "white") +
  geom_point(shape = 21, 
             colour = "grey20", 
             fill = "white") +
  geom_line(colour = "grey20") +
  labs(y = "Species Diversity",
       x = "Myr") +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(146, -5)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500), 
                     limits = c(0, 2800)) +
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
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# step plot
plot_step <- dat_plot %>%
  ggplot(aes(start_age, y)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_stepribbon(aes(ymin = ymin, 
                      ymax = ymax), 
                  alpha = 0.15, 
                  fill = "grey20",
                  colour = "white", 
                  linewidth = 0.001) +
  geom_step(colour = "grey20") +
  labs(y = "Genus Diversity",
       x = "Myr",
       colour = NULL) +
  labs(y = "Species Diversity",
       x = "Myr") +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(146, -5)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500), 
                     limits = c(0, 2800)) +
  coord_geo(dat = "epochs",
            pos = "b",
            alpha = 0,
            lab_color = "white",
            color = "white",
            abbrv = TRUE,
            fill = "white",
            expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# patch together
# plot_fin <- 
plot_step / plot_line

# save
ggsave(plot_fin, 
       filename = here("figures",
                       "fig_S1.pdf"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white")
