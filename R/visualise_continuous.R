library(tidyverse)
library(here)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

dat_raw <- read_csv(here("data",
                          "diversity_continuous_raw.csv"))

# dat_sqs <- read_csv(here("data",
#                          "diversity_continuous_sqs.csv"))

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

# visualise ---------------------------------------------------------------


plot_spec <- dat_raw %>%
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

# plot_spec_sqs <- dat_sqs %>%
#   filter( str_detect(metric, "species")) %>% 
#   mutate(metric = case_when(
#     metric == "speciesRT" ~ "Range-Through", 
#     metric == "speciesBC" ~ "Boundary-Crosser",
#     metric == "speciesSIB" ~ "Sampled-in-Bin")) %>% 
#   ggplot(aes(mid_age, diversity, 
#              colour = metric, 
#              group = run)) +
#   geom_vline(xintercept = epoch_age, 
#              colour = "grey70") +
#   geom_line(alpha = 0.1) +
#   labs(y = "Species Diversity  [SQS]", 
#        x = NULL, 
#        colour = NULL) +
#   scale_color_manual(values = c("#ad6d8aff", 
#                                 "#ffbc3cff",
#                                 "coral3")) +
#   scale_x_reverse(breaks = seq(140, 0, -20)) +
#   coord_geo(xlim = c(0, 140), 
#             dat = list("periods"),
#             pos = list("b"),
#             alpha = 0.2, 
#             height = unit(0.8, "line"), 
#             size = list(10/.pt),
#             lab_color = "grey20", 
#             color = "grey20", 
#             abbrv = list(TRUE), 
#             fill = "white",
#             expand = TRUE, 
#             lwd = list(0.4)) +
#   theme_minimal() +
#   theme(legend.position = "none", 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~metric) 

# same for genus
plot_gen <- dat_raw %>%
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

# plot_gen_sqs <- dat_sqs %>%
#   filter(str_detect(metric, "genus")) %>% 
#   mutate(metric = case_when(
#     metric == "genusRT" ~ "Range-Through", 
#     metric == "genusBC" ~ "Boundary-Crosser",
#     metric == "genusSIB" ~ "Sampled-in-Bin")) %>% 
#   ggplot(aes(mid_age, diversity, 
#              colour = metric, 
#              group = run)) +
#   geom_vline(xintercept = epoch_age, 
#              colour = "grey70") +
#   geom_line(alpha = 0.1) +
#   labs(y = "Genus Diversity  [SQS]", 
#        x = NULL, 
#        colour = NULL) +
#   scale_color_manual(values = c("#ad6d8aff", 
#                                 "#ffbc3cff",
#                                 "coral3"), 
#                      labels = c("Boundary-Crosser", 
#                                 "Range-Through", 
#                                 "Sampled-in-Bin")) +
#   scale_x_reverse(breaks = seq(140, 0, -20))  +
#   coord_geo(xlim = c(0, 140), 
#             dat = list("periods"),
#             pos = list("b"),
#             alpha = 0.2, 
#             height = unit(0.8, "line"), 
#             size = list(10/.pt),
#             lab_color = "grey20", 
#             color = "grey20", 
#             abbrv = list(TRUE), 
#             fill = "white",
#             expand = TRUE, 
#             lwd = list(0.4)) +
#   theme_minimal() +
#   theme(legend.position = "none", 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~metric)

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

