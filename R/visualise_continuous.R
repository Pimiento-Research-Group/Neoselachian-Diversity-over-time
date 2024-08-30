library(tidyverse)
library(here)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

dat_raw <- read_rds(here("data",
                          "diversity_continuous_raw.rds"))

dat_sqs <- read_rds(here("data",
                         "diversity_continuous_sqs.rds"))

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

dat_pyrate_genus <- read_delim(here("data", 
                                    "all_genus_PyRate_mcmcdiv.log")) %>% 
  select(it, contains("t_")) %>% 
  pivot_longer(-it, 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = str_remove_all(start_age, "t_"), 
         start_age = as.double(start_age)) %>% 
  rename(run = it) %>% 
  filter(run != 0) %>% 
  mutate(run = run/500) %>% 
  add_column(metric = "PyRate")

dat_pyrate_species <- read_delim(here("data", 
                                    "all_species_input_PyRate_mcmcdiv.log")) %>% 
  select(it, contains("t_")) %>% 
  pivot_longer(-it, 
               names_to = "start_age", 
               values_to = "diversity") %>% 
  mutate(start_age = str_remove_all(start_age, "t_"), 
         start_age = as.double(start_age)) %>% 
  rename(run = it) %>% 
  filter(run != 0) %>% 
  mutate(run = run/500) %>% 
  add_column(metric = "PyRate")

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



# species level -----------------------------------------------------------

# join data
dat_species <- dat_raw %>%
  select(stg, start_age, run, Raw = speciesRT) %>% 
  full_join(dat_sqs %>% 
              select(stg, start_age, run, SQS = speciesRT)) %>% 
  left_join(dat_deep_species) %>% 
  pivot_longer(cols = c(Raw, SQS, DeepDive), 
               names_to = "metric", 
               values_to = "diversity") %>% 
  group_by(start_age, metric) %>% 
  reframe(quant = quantile(diversity, 
                           probs = c(0.25, 0.5, 0.75), 
                           na.rm = TRUE), 
          quart = c("ymin", "diversity", "ymax")) %>% 
  pivot_wider(values_from = quant, 
              names_from = quart) %>% 
  mutate(metric = ordered(metric, 
                          levels = c("Raw", 
                                     "SQS", 
                                     "DeepDive")))


# visualise
plot_spec_abs <- dat_species %>%
  ggplot(aes(start_age, diversity,
             colour = metric)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey95",
             linewidth = 0.4) +
  geom_stepribbon(aes(ymin = ymin, 
                      ymax = ymax), 
                  alpha = 0.3, 
                  colour = "grey80", 
                  linewidth = 0.001, 
                  fill = "grey20") +
  geom_step(linewidth = 0.3, 
            colour = "grey20") +
  geom_stepribbon(aes(ymin = ymin, 
                      ymax = ymax), 
                  data = dat_pyrate_species %>%
                    group_by(start_age, metric) %>%
                    reframe(
                      quant = quantile(diversity, probs = c(0.25, 0.5, 0.75)),
                      quart = c("ymin", "diversity", "ymax")
                    ) %>%
                    pivot_wider(values_from = quant, names_from = quart), 
                  alpha = 0.3, 
                  colour = "grey80", 
                  linewidth = 0.001, 
                  fill = "grey20") +
  geom_step(data = dat_pyrate_species %>% 
              group_by(start_age, metric) %>%
              summarise(diversity = mean(diversity)), 
            linewidth = 0.3, 
            colour = "grey20") +
  labs(y = "Species Diversity",
       x = "Myr",
       colour = NULL) +
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
             nrow = 4, 
             strip.position = "right")

# save
ggsave(plot_spec_abs, 
       filename = here("figures",
                       "species_comparison.pdf"), 
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

