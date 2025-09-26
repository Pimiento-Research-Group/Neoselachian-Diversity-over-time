library(here)
library(tidyverse)
library(readxl)
library(divvy)

# read data ---------------------------------------------------------------

# species level
dat_species <- read_xlsx(here("data",
                              "species.xlsx"))

# genus level
dat_genus <- read_xlsx(here("data",
                            "genus.xlsx"))


# define the bins
bins <- sort(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
               89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
               47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
               13.820, 11.630, 7.246, 5.333, 2.580, 0), 
             decreasing = TRUE)




# temporally bin data -----------------------------------------------------

dat_spec_bin <- dat_species %>% 
  select(accepted_name, genus, 
         family, order, superorder, 
         paleolat, paleolon,
         max_ma, min_ma) %>% 
  mutate(age_mean = ((max_ma - min_ma)/2) + min_ma,
         stg = cut(age_mean, 
                   breaks = bins, 
                   include.lowest = TRUE, 
                   labels = FALSE))

dat_gen_bin <- dat_genus %>% 
  select(accepted_name, genus, 
         family, order, superorder, 
         paleolat, paleolon,
         max_ma, min_ma) %>% 
  mutate(age_mean = ((max_ma - min_ma)/2) + min_ma,
         stg = cut(age_mean, 
                   breaks = bins, 
                   include.lowest = TRUE, 
                   labels = FALSE))




# perform circular subsampling  within each bin -----------------------------------------------


# perform circular subsampling
list_div_spec <- dat_spec_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         cookies(xy = 1:2,
                                 iter = 500,
                                 nSite = 12,
                                 r = 7000,
                                 output = "full"), 
                       .progress = TRUE))
   


# same for genus level
# perform circular subsampling
list_div_gen <- dat_gen_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         cookies(xy = 1:2,
                                 iter = 500,
                                 nSite = 10,
                                 r = 5000,
                                 output = "full"), 
                       .progress = TRUE))




# perform nearest-neighbour clustering --------------------------------------------------------


# perform circular subsampling
list_div_spec_nn <- dat_spec_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         clustr(xy = 1:2,
                                 iter = 500,
                                 nSite = NULL,
                                 distMax = 10000,
                                 output = "full"), 
                       .progress = TRUE))



# same for genus level
# perform circular subsampling
list_div_gen_nn <- dat_gen_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         clustr(xy = 1:2,
                                iter = 500,
                                nSite = NULL,
                                distMax = 10000,
                                output = "full"), 
                       .progress = TRUE))



# perform rarefaction within bands of equal latitude ------------------------------------------

# perform circular subsampling
list_div_spec_lb <- dat_spec_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         bandit(xy = 1:2,
                                iter = 500,
                                nSite = 10,
                                bin = 20,
                                output = "full"), 
                       .progress = TRUE))



# same for genus level
# perform circular subsampling
list_div_gen_lb <- dat_gen_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         bandit(xy = 1:2,
                                iter = 500,
                                nSite = 10,
                                bin = 20,
                                output = "full"),
                       .progress = TRUE))



# plot individual estimates -------------------------------------------------------------------

library(pammtools)
library(deeptime)

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

periods_cor <- read_rds(here("data",
                             "periods_cor.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds"))

# first load deepdive data for comparison 
# deepdive species
dat_deep_spec <- read_csv(here("data",
                               "deepdive_species",
                               "all.csv")) 


# deepdive genus
dat_deep_gen <- read_csv(here("data",
                              "deepdive_genus",
                              "all.csv")) 

# for species
dat_div_spec <- map2_df(.x = list(list_div_spec, list_div_spec_lb, list_div_spec_nn), 
        .y = c("CS", "LB", "NN"), 
        .f = ~ .x %>% 
          mutate(n_occs = map(dat_sub, ~ .x %>%
                                map("accepted_name") %>%
                                map_dbl(n_distinct)), 
                 mean_div = map_dbl(n_occs, mean), 
                 min_div = map_dbl(n_occs, min), 
                 max_div = map_dbl(n_occs, max)) %>% 
          arrange(stg) %>% 
          add_column(start_age = rev(bins[3:29])) %>% 
          select(stg, mean_div, min_div, max_div, start_age) %>% 
          add_column(metric = .y))



# join with deepdive
dat_div_spec_full <- dat_deep_spec %>%
  pivot_longer(cols = everything(), 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(diversity), 
            min_div = min(diversity), 
            max_div = max(diversity)) %>% 
  filter(start_age > 0) %>% 
  add_column(metric = "DeepDive") %>% 
  bind_rows(dat_div_spec %>% 
              select(start_age, mean_div, min_div, max_div, metric) %>% 
              mutate(metric = case_when(
                metric == "CS" ~ "Circular subsampling", 
                metric == "LB" ~ "Equal latitudinal bands", 
                metric == "NN" ~ "Nearest Neighbour"
              ))) %>% 
  mutate(metric = ordered(metric, levels = rev(c("DeepDive", 
                                             "Circular subsampling", 
                                             "Equal latitudinal bands", 
                                             "Nearest Neighbour"))))




# visualise
plot_spec_div <- dat_div_spec_full %>%
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
ggsave(plot_spec_div, 
       filename = here("figures",
                       "fig_S6.pdf"), 
       width = 183, height = 100*2,
       units = "mm", 
       bg = "white")


# genus level 

## for genus
dat_div_gen <- map2_df(.x = list(list_div_gen, list_div_gen_lb, list_div_gen_nn), 
                        .y = c("CS", "LB", "NN"), 
                        .f = ~ .x %>% 
                          mutate(n_occs = map(dat_sub, ~ .x %>%
                                                map("accepted_name") %>%
                                                map_dbl(n_distinct)), 
                                 mean_div = map_dbl(n_occs, mean), 
                                 min_div = map_dbl(n_occs, min), 
                                 max_div = map_dbl(n_occs, max)) %>% 
                          arrange(stg) %>% 
                          add_column(start_age = rev(bins[3:29])) %>% 
                          select(stg, mean_div, min_div, max_div, start_age) %>% 
                          add_column(metric = .y))



# join with deepdive
dat_div_gen_full <- dat_deep_gen %>%
  pivot_longer(cols = everything(), 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = as.double(start_age)) %>% 
  group_by(start_age) %>% 
  summarise(mean_div = mean(diversity), 
            min_div = min(diversity), 
            max_div = max(diversity)) %>% 
  filter(start_age > 0) %>% 
  add_column(metric = "DeepDive") %>% 
  bind_rows(dat_div_gen %>% 
              select(start_age, mean_div, min_div, max_div, metric) %>% 
              mutate(metric = case_when(
                metric == "CS" ~ "Circular subsampling", 
                metric == "LB" ~ "Equal latitudinal bands", 
                metric == "NN" ~ "Nearest Neighbour"
              ))) %>% 
  mutate(metric = ordered(metric, levels = rev(c("DeepDive", 
                                                 "Circular subsampling", 
                                                 "Equal latitudinal bands", 
                                                 "Nearest Neighbour"))))




# visualise
plot_gen_div <- dat_div_gen_full %>%
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
ggsave(plot_gen_div, 
       filename = here("figures",
                       "fig_S7.pdf"), 
       width = 183, height = 100*2,
       units = "mm", 
       bg = "white")



# average metric ------------------------------------------------------------------------------

# for species
bind_rows(list_div_spec, list_div_spec_lb) %>% 
  bind_rows(list_div_spec_nn) %>%
  select(-data) %>% 
  unnest(dat_sub) %>% 
  mutate(n_occs = map_dbl(.x = dat_sub, 
                          ~ .x %>%
                            n_distinct("accepted_name"))) %>% 
  group_by(stg) %>% 
  summarise(mean_div = mean(n_occs),
            min_div = min(n_occs),
            max_div = max(n_occs)) %>% 
  arrange(stg) %>% 
  add_column(start_age = rev(bins[3:29])) %>% 
  select(stg, mean_div, min_div, max_div, start_age) %>% 
  write_rds(here("data",
                 "diversity_continuous_divvy_species.rds"), 
            compress = "gz")


# for genus
bind_rows(list_div_gen, list_div_gen_lb) %>% 
  bind_rows(list_div_gen_nn) %>%
  select(-data) %>% 
  unnest(dat_sub) %>% 
  mutate(n_occs = map_dbl(.x = dat_sub, 
                          ~ .x %>%
                            n_distinct("accepted_name"))) %>% 
  group_by(stg) %>% 
  summarise(mean_div = mean(n_occs),
            min_div = min(n_occs),
            max_div = max(n_occs)) %>% 
  arrange(stg) %>% 
  add_column(start_age = rev(bins[3:29])) %>% 
  select(stg, mean_div, min_div, max_div, start_age) %>% 
  write_rds(here("data",
                 "diversity_continuous_divvy_genus.rds"), 
            compress = "gz")



