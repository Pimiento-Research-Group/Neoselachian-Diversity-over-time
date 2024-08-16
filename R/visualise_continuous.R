library(tidyverse)
library(here)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

dat_raw <- read_rds(here("data",
                          "diversity_continuous_raw.rds"))

dat_sqs <- read_rds(here("data",
                         "diversity_continuous_sqs.rds"))

dat_deep_genus <- read_csv(here("data", 
                                 "deepdive_all_genus.csv"))

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
               values_to = "diversity") %>% 
  mutate(metric = ordered(metric, 
                          levels = c("Raw", 
                                     "SQS", 
                                     "DeepDive")))


# visualise
plot_gen_abs <- dat_genus %>%
  ggplot(aes(start_age, diversity,
             colour = metric)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(aes(group = run), 
            alpha = 0.07) +
  geom_step(data = . %>% 
              group_by(start_age, metric) %>% 
              summarise(diversity = mean(diversity))) +
  labs(y = "Genus Diversity",
       x = NULL,
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff",
                                "#ffbc3cff",
                                "coral3")) +
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
  theme(legend.position = "none",
        axis.ticks = element_line(colour = "grey50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric, 
             scales = "free")


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
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(aes(group = run), 
            alpha = 0.07) +
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

