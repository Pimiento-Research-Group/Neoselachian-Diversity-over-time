library(tidyverse)
library(here)
library(patchwork)
library(deeptime)
library(pammtools)
library(readxl)


# read data ---------------------------------------------------------------

# species data
dat_spec <- read_xlsx(here("data", 
                           "taxa_per_stage_per_metric_species.xlsx"))

# deepdive species
dat_deep_spec <- read_csv(here("data",
                                 "deepdive_species",
                                 "all.csv")) 

# genus data
dat_gen <- read_xlsx(here("data", 
                           "taxa_per_stage_per_metric_genus.xlsx"))

# deepdive genus
dat_deep_gen <- read_csv(here("data",
                               "deepdive_genus",
                               "all.csv")) 

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

periods_cor <- read_rds(here("data",
                           "periods_cor.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds"))



# species level -----------------------------------------------------------

# merge data
dat_spec_full <- dat_deep_spec %>%
  pivot_longer(cols = everything(), 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(diversity), 
            min_div = min(diversity), 
            max_div = max(diversity)) %>% 
  filter(start_age > 0) %>% 
  left_join(stage_cor %>% 
              select(stage = name, start_age = max_age)) %>% 
  add_column(metric = "DeepDive") %>% 
  bind_rows(dat_spec %>% 
              mutate(metric = str_to_sentence(metric), 
                     metric = str_replace_all(metric, 
                                              "Pyrate", 
                                              "PyRate"), 
                     metric = str_replace_all(metric, 
                                              "Sqs", 
                                              "SQS"))) %>% 
  mutate(metric = ordered(metric, 
                          levels = c("Raw", 
                                     "SQS", 
                                     "Divvy",
                                     "PyRate",
                                     "DeepDive"))) 

# visualise
plot_spec_abs <- dat_spec_full %>%
  ggplot(aes(start_age, mean_div,
             colour = metric)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey95",
             linewidth = 0.4) +
  geom_stepribbon(aes(ymin = min_div, 
                      ymax = max_div), 
                  alpha = 0.3, 
                  colour = "grey80", 
                  linewidth = 0.001, 
                  fill = "grey20") +
  geom_step(linewidth = 0.3, 
            colour = "grey20") +
  labs(y = "Species Diversity",
       x = "Time (Ma)",
       colour = NULL) +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(145, 0)) +
  scale_y_continuous(limits = c(0, NA)) +
  coord_geo(dat = list(stage_cor, 
                       epoch_cor, 
                       periods_cor),
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
            expand = FALSE,
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric, 
             scales = "free_y", 
             nrow = 5, 
             strip.position = "right")


# save
ggsave(plot_spec_abs, 
       filename = here("figures",
                       "fig_S2.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")


# genus level -----------------------------------------------------------

# join data
# merge data
dat_gen_full <- dat_deep_gen %>%
  pivot_longer(cols = everything(), 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(diversity), 
            min_div = min(diversity), 
            max_div = max(diversity)) %>% 
  filter(start_age > 0) %>% 
  left_join(stage_cor %>% 
              select(stage = name, start_age = max_age)) %>% 
  add_column(metric = "DeepDive") %>% 
  bind_rows(dat_gen %>% 
              mutate(metric = str_to_sentence(metric), 
                     metric = str_replace_all(metric, 
                                              "Pyrate", 
                                              "PyRate"), 
                     metric = str_replace_all(metric, 
                                              "Sqs", 
                                              "SQS"))) %>% 
  mutate(metric = ordered(metric, 
                          levels = c("Raw", 
                                     "SQS", 
                                     "Divvy",
                                     "PyRate",
                                     "DeepDive"))) 

# visualise
plot_gen_abs <- dat_gen_full %>%
  ggplot(aes(start_age, mean_div,
             colour = metric)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey95",
             linewidth = 0.4) +
  geom_stepribbon(aes(ymin = min_div, 
                      ymax = max_div), 
                  alpha = 0.3, 
                  colour = "grey80", 
                  linewidth = 0.001, 
                  fill = "grey20") +
  geom_step(linewidth = 0.3, 
            colour = "grey20") +
  labs(y = "Genus Diversity",
       x = "Time (Ma)",
       colour = NULL) +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(145, 0)) +
  scale_y_continuous(limits = c(0, NA)) +
  coord_geo(dat = list(stage_cor, 
                       epoch_cor, 
                       periods_cor),
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
            expand = FALSE,
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric, 
             scales = "free_y", 
             nrow = 5, 
             strip.position = "right")

# save
ggsave(plot_gen_abs, 
       filename = here("figures",
                       "fig_S5.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")




