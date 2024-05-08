library(tidyverse)
library(here)
library(patchwork)

# read data ---------------------------------------------------------------

dat_raw <- read_csv(here("data",
                          "diversity_continuous_raw.csv"))

dat_sqs <- read_csv(here("data",
                         "diversity_continuous_sqs.csv"))

# visualise ---------------------------------------------------------------


plot_spec <- dat_raw %>%
  filter(str_detect(metric, "species")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Species diversity", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_spec_sqs <- dat_sqs %>%
  filter( str_detect(metric, "species")) %>% 
  mutate(metric = case_when(
    metric == "speciesRT" ~ "Range-Through", 
    metric == "speciesBC" ~ "Boundary-Crosser",
    metric == "speciesSIB" ~ "Sampled-in-Bin")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric, 
             group = run)) +
  geom_line(alpha = 0.1) +
  labs(y = "Species Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~metric) 

# same for genus
plot_gen <- dat_raw %>%
  filter(str_detect(metric, "genus")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Genus diversity", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_gen_sqs <- dat_sqs %>%
  filter(str_detect(metric, "genus")) %>% 
  mutate(metric = case_when(
    metric == "genusRT" ~ "Range-Through", 
    metric == "genusBC" ~ "Boundary-Crosser",
    metric == "genusSIB" ~ "Sampled-in-Bin")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric, 
             group = run)) +
  geom_line(alpha = 0.1) +
  labs(y = "Genus Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
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

