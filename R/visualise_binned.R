library(tidyverse)
library(here)
library(patchwork)
library(deeptime)

# read data ---------------------------------------------------------------

dat_full <- read_csv(here("data",
                             "diversity_stages.csv"))

dat_full_tens <- read_csv(here("data",
                          "diversity_10myr.csv"))

# bin data
data(stages, package = "divDyn")
data(tens, package = "divDyn")

# visualise ---------------------------------------------------------------


plot_spec <- dat_full %>%
  select(-1) %>% 
  filter(!str_detect(metric, "sqs"), 
         str_detect(metric, "species")) %>% 
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_spec_sqs <- dat_full %>% 
  select(-1) %>% 
  filter(str_detect(metric, "sqs"), 
         str_detect(metric, "species")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Species Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# same for genus
plot_gen <- dat_full %>%
  select(-1) %>% 
  filter(!str_detect(metric, "sqs"), 
         str_detect(metric, "genus")) %>% 
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_gen_sqs <- dat_full %>% 
  select(-1) %>% 
  filter(str_detect(metric, "sqs"), 
         str_detect(metric, "genus")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Genus Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# patch together ----------------------------------------------------------

plot_final <- (plot_spec + plot_spec_sqs) /
  (plot_gen + plot_gen_sqs)+
  plot_annotation(tag_levels = "a")

# save
ggsave(plot_final, 
       filename = here("figures",
                       "sqs_diversity_stages.png"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white", device = ragg::agg_png)



# same for 10myrs ---------------------------------------------------------

plot_spec_tens <- dat_full_tens %>%
  select(-1) %>% 
  filter(!str_detect(metric, "sqs"), 
         str_detect(metric, "species")) %>% 
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_spec_sqs_tens <- dat_full_tens %>% 
  select(-1) %>% 
  filter(str_detect(metric, "sqs"), 
         str_detect(metric, "species")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Species Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# same for genus
plot_gen_tens <- dat_full_tens %>%
  select(-1) %>% 
  filter(!str_detect(metric, "sqs"), 
         str_detect(metric, "genus")) %>% 
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_gen_sqs_tens <- dat_full_tens %>% 
  select(-1) %>% 
  filter(str_detect(metric, "sqs"), 
         str_detect(metric, "genus")) %>% 
  ggplot(aes(mid_age, diversity, 
             colour = metric)) +
  geom_line() +
  labs(y = "Genus Diversity  [SQS]", 
       x = "Age [Myr]", 
       colour = NULL) +
  scale_color_manual(values = c("#ad6d8aff", 
                                "#ffbc3cff",
                                "coral3"), 
                     labels = c("Boundary-Crosser", 
                                "Range-Through", 
                                "Sampled-in-Bin")) +
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
            lwd = list(0.4, 0.5)) +
  scale_x_reverse(breaks = seq(140, 0, -20)) +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# patch together 

plot_final_tens <- (plot_spec_tens + plot_spec_sqs_tens) /
  (plot_gen_tens + plot_gen_sqs_tens)+
  plot_annotation(tag_levels = "a")

# save
ggsave(plot_final_tens, 
       filename = here("figures",
                       "sqs_diversity_10myr.png"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white", device = ragg::agg_png)


