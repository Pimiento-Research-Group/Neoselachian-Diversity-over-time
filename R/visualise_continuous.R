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
                                 "all_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_species",
                          "all_128.csv")) %>% 
              add_column(model = "128"))

# genus data
dat_gen <- read_xlsx(here("data", 
                           "taxa_per_stage_per_metric_genus.xlsx"))

# deepdive genus
dat_deep_gen <- read_csv(here("data",
                               "deepdive_genus",
                               "all_64.csv")) %>% 
  add_column(model = "64") %>% 
  bind_rows(read_csv(here("data",
                          "deepdive_genus",
                          "all_128.csv")) %>% 
              add_column(model = "128"))

# stage data
epoch_age <- read_rds(here("data", 
                           "epoch_age.rds"))

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

stage_cor <- read_rds(here("data", 
                           "stage_cor.rds"))



# species level -----------------------------------------------------------

# merge data
dat_spec_full <- dat_deep_spec %>%
  pivot_longer(cols = -model, 
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
            expand = FALSE,
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric, 
             scales = "free_y", 
             nrow = 4, 
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
  pivot_longer(cols = -model, 
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
            expand = FALSE,
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric, 
             scales = "free_y", 
             nrow = 4, 
             strip.position = "right")

# save
ggsave(plot_gen_abs, 
       filename = here("figures",
                       "fig_S5.pdf"), 
       width = 183, height = 100*2.5,
       units = "mm", 
       bg = "white")




# differences to deepdive -------------------------------------------------


# plot_spec_dif <- 
dat_raw %>%
  select(stg, start_age, run, Raw = speciesRT) %>% 
  full_join(dat_sqs %>% 
              select(stg, start_age, run, SQS = speciesRT)) %>% 
  left_join(dat_deep_species) %>% 
  pivot_longer(cols = c(Raw, SQS, DeepDive), 
               names_to = "metric", 
               values_to = "diversity") %>% 
  group_by(metric) %>% 
  # mean center diversity
  mutate(diversity = (diversity - min(diversity, na.rm = TRUE))/
           (max(diversity, na.rm = TRUE) - min(diversity, na.rm = TRUE))) %>% 
  pivot_wider(names_from = metric, 
              values_from = diversity) %>% 
  mutate(dif_raw = Raw - DeepDive, 
         dif_sqs = SQS - DeepDive) %>% 
  select(-c(Raw, SQS, DeepDive)) %>% 
  pivot_longer(cols = c(dif_raw, dif_sqs), 
               names_to = "metric", 
               values_to = "difference") %>% 
  ggplot(aes(start_age, difference,
             colour = metric)) +
  geom_hline(yintercept = 0, 
             colour = "grey20") +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(data = . %>% 
              group_by(start_age, metric) %>% 
              summarise(difference = mean(difference))) +
  labs(y = "Difference to DeepDive\n[species, scaled]",
       x = NULL,
       colour = NULL) +
  scale_color_manual(values = c("coral",
                                "#316286"), 
                     breaks = c("dif_raw", 
                                "dif_sqs"), 
                     labels = c("Raw", 
                                "SQS")) +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(145, 0)) +
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
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.9),
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save
ggsave(plot_gen_dif, 
       filename = here("figures",
                       "genus_difference.pdf"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white")



# per genus ---------------------------------------------------------------

# join data
dat_genus <- dat_raw %>%
  select(stg, start_age, run, Raw = genusRT) %>% 
  full_join(dat_sqs %>% 
              select(stg, start_age, run, SQS = genusRT)) %>% 
  left_join(dat_deep_genus %>% 
              rowid_to_column("run") %>% 
              pivot_longer(cols = -run, 
                           names_to = "start_age", 
                           values_to = "DeepDive") %>% 
              mutate(start_age = as.double(start_age))) %>% 
  pivot_longer(cols = c(Raw, SQS, DeepDive), 
               names_to = "metric", 
               values_to = "diversity") 


# visualise
plot_gen_abs <- dat_genus %>%
  ggplot(aes(start_age, diversity,
             colour = metric)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(aes(group = run), 
            alpha = 0.05) +
  geom_step(data = . %>% 
              group_by(start_age, metric) %>% 
              summarise(diversity = mean(diversity)), 
            linewidth = 0.3) +
  geom_step(aes(group = run), 
            data = dat_pyrate_genus, 
            alpha = 0.004) +
  geom_step(data = dat_pyrate_genus %>% 
              group_by(start_age, metric) %>% 
              summarise(diversity = mean(diversity)), 
            linewidth = 0.3) +
  labs(y = "Genus Diversity",
       x = "Myr",
       colour = NULL) +
  scale_color_manual(values = c("grey60", 
                                "#316286",
                                "#ad6d8aff",
                                "#413851")) +
  scale_x_reverse(breaks = seq(140, 0, by = -20), 
                  limits = c(145, 0)) +
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
            lwd = list(0.1, 0.1, 0.1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~ordered(metric, 
                      levels = c("Raw", 
                                 "SQS", 
                                 "PyRate",
                                 "DeepDive")), 
             scales = "free_y", 
             nrow = 4)

# save
ggsave(plot_gen_abs, 
       filename = here("figures",
                       "genus_comparison.pdf"), 
       width = 183, height = 100*1.25,
       units = "mm", 
       bg = "white")

# differences to DeepDive
plot_gen_dif <- dat_genus %>% 
  group_by(metric) %>% 
  # mean center diversity
  mutate(diversity = (diversity - min(diversity))/
           (max(diversity) - min(diversity))) %>% 
  pivot_wider(names_from = metric, 
              values_from = diversity) %>% 
  mutate(dif_raw = Raw - DeepDive, 
         dif_sqs = SQS - DeepDive) %>% 
  select(-c(Raw, SQS, DeepDive)) %>% 
  pivot_longer(cols = c(dif_raw, dif_sqs), 
               names_to = "metric", 
               values_to = "difference") %>% 
  ggplot(aes(start_age, difference,
             colour = metric)) +
  geom_hline(yintercept = 0, 
             colour = "grey20") +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  # geom_step(aes(group = run), 
  #           alpha = 0.07) +
  geom_step(data = . %>% 
              group_by(start_age, metric) %>% 
              summarise(difference = mean(difference))) +
  labs(y = "Difference to DeepDive\n[genus, scaled]",
       x = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff",
                                "#ffbc3cff"), 
                     breaks = c("dif_raw", 
                                "dif_sqs"), 
                     labels = c("Raw", 
                                "SQS")) +
  scale_x_reverse() +
  coord_geo(dat = list("periods"),
            pos = list("b"),
            alpha = 0.2,
            height = unit(0.8, "line"),
            size = list(10/.pt),
            lab_color = "grey40",
            color = "grey40",
            abbrv = list(TRUE),
            fill = "white",
            expand = TRUE,
            lwd = list(0.4)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.9),
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# save
ggsave(plot_gen_dif, 
       filename = here("figures",
                       "genus_difference.pdf"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white")



# visualise ---------------------------------------------------------------


# plot_spec <- 
dat_raw %>%
  ggplot(aes(start_age, speciesRT)) +
  geom_vline(xintercept = epoch_age, 
             colour = "grey70") +
  geom_step(aes(group = run), 
            alpha = 0.07, 
            colour = "#ffbc3cff") +
  geom_step(data = dat_raw %>% 
              group_by(start_age) %>% 
              summarise(speciesRT = mean(speciesRT)), 
            colour = "#ffbc3cff") +
  labs(y = "Species diversity", 
       x = NULL, 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  coord_geo(xlim = c(0, 140), 
            dat = list("periods"),
            pos = list("b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(10/.pt),
            lab_color = "grey20", 
            color = "grey20", 
            abbrv = list(TRUE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.4)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# plot_spec_sqs <- 
dat_sqs %>%
  filter( str_detect(metric, "species")) %>%
  mutate(metric = case_when(
    metric == "speciesRT" ~ "Range-Through",
    metric == "speciesBC" ~ "Boundary-Crosser",
    metric == "speciesSIB" ~ "Sampled-in-Bin")) %>%
  filter(metric == "Range-Through") %>% 
  ggplot(aes(mid_age, diversity,
             colour = metric,
             group = run)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey70") +
  geom_step(aes(group = run), 
            alpha = 0.07) +
  labs(y = "Species Diversity  [SQS]",
       x = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff",
                                "#ffbc3cff",
                                "coral3")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  coord_geo(xlim = c(0, 140),
            dat = list("periods"),
            pos = list("b"),
            alpha = 0.2,
            height = unit(0.8, "line"),
            size = list(10/.pt),
            lab_color = "grey20",
            color = "grey20",
            abbrv = list(TRUE),
            fill = "white",
            expand = TRUE,
            lwd = list(0.4)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric)

# same for genus
# plot_gen <- 
dat_raw %>%
  ggplot(aes(start_age, genusRT)) +
  geom_vline(xintercept = epoch_age, 
             colour = "grey70") +
  geom_step(aes(group = run), 
            alpha = 0.07, 
            colour = "#ffbc3cff") +
  geom_step(data = dat_raw %>% 
              group_by(start_age) %>% 
              summarise(genusRT = mean(genusRT)), 
            colour = "#ffbc3cff") +
  labs(y = "Genus diversity", 
       x = NULL, 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20))  +
  coord_geo(xlim = c(0, 140), 
            dat = list("periods"),
            pos = list("b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(10/.pt),
            lab_color = "grey20", 
            color = "grey20", 
            abbrv = list(TRUE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.4)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# plot_gen_sqs <- 
dat_sqs %>%
  filter(str_detect(metric, "genus")) %>%
  mutate(metric = case_when(
    metric == "genusRT" ~ "Range-Through",
    metric == "genusBC" ~ "Boundary-Crosser",
    metric == "genusSIB" ~ "Sampled-in-Bin")) %>%
  filter(metric == "Range-Through") %>% 
  ggplot(aes(mid_age, diversity,
             colour = metric,
             group = run)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey70") +
  geom_step(aes(group = run), 
            alpha = 0.07) +
  labs(y = "Genus Diversity  [SQS]",
       x = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff",
                                "#ffbc3cff",
                                "coral3"),
                     labels = c("Boundary-Crosser",
                                "Range-Through",
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20))  +
  coord_geo(xlim = c(0, 140),
            dat = list("periods"),
            pos = list("b"),
            alpha = 0.2,
            height = unit(0.8, "line"),
            size = list(10/.pt),
            lab_color = "grey20",
            color = "grey20",
            abbrv = list(TRUE),
            fill = "white",
            expand = TRUE,
            lwd = list(0.4)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric)

# patch together ----------------------------------------------------------

plot_raw <- plot_spec /
  plot_gen +
  plot_annotation(tag_levels = "a")

plot_sqs <- plot_spec_sqs /
  plot_gen_sqs +
  plot_annotation(tag_levels = "a")

# save
ggsave(plot_raw, 
       filename = here("figures",
                       "raw_diversity_continuous.png"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white", device = ragg::agg_png)

# save
ggsave(plot_sqs, 
       filename = here("figures",
                       "sqs_diversity_continuous.png"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white", device = ragg::agg_png)

