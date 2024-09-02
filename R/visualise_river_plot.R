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



# get epoch age into epochs
epoch_age <- stages %>% 
  as_tibble() %>% 
  group_by(series) %>% 
  summarise(epoch = min(top)) %>% 
  arrange(epoch) %>% 
  filter(epoch <= 140) %>% 
  pull(epoch) %>% 
  {.[-c(1, 2)]}

# set up epochs for plotting
data(epochs, package = "deeptime")

epoch_cor <- epochs %>%
  as_tibble() %>% 
  filter(between(max_age, 0, 145)) %>% 
  mutate(color = c("#FEF6F2", 
                   "#FFF1C4", 
                   "#FFF7B2", 
                   "#FFED00", 
                   "#FBCC98", 
                   "#FAC18A", 
                   "#F8B77D", 
                   "#BAD25F", 
                   "#A0C96D"))


# set up stages for plotting
data(stages, package = "deeptime")

stage_cor <- stages %>%
  as_tibble() %>% 
  filter(!name  %in% c("Meghalayan", "Northgrippian", "Greenlandian", 
                       "Late Pleistocene", "Chibanian", "Calabrian",
                       "Piacenzian", "Zanclean", "Gelasian")) %>% 
  add_row(epoch_cor %>% 
            filter(name %in% c("Pleistocene", "Pliocene"))) %>% 
  arrange(max_age)


# visualise ------------------------------------------------------------------

# reframe
dat_plot <-  dat_deep_species %>%
  group_by(start_age) %>% 
  reframe(quant = quantile(DeepDive, 
                           probs = c(0.25, 0.5, 0.75), 
                           na.rm = TRUE), 
          quart = c("ymin", "y", "ymax")) %>%  
  pivot_wider(values_from = quant, 
              names_from = quart) %>%  
  mutate(start_age = as.double(start_age))

# line plot
plot_line <- dat_plot %>% 
  ggplot(aes(start_age, y)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_ribbon(aes(ymin = ymin, 
                  ymax = ymax), 
              alpha = 0.3) +
  geom_point(shape = 21) +
  geom_line() +
  labs(y = "Species Diversity",
       x = "Myr") +
  scale_x_reverse(breaks = seq(140, 0, by = -20)) +
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
                  colour = "white", 
                  linewidth = 0) +
  geom_step() +
  labs(y = "Genus Diversity",
       x = "Myr",
       colour = NULL) +
  labs(y = "Species Diversity",
       x = "Myr") +
  scale_x_reverse(breaks = seq(140, 0, by = -20)) +
  # coord_geo(dat = list(stage_cor, 
  #                      epoch_cor, 
  #                      "periods"),
  #           pos = list("b", "b", "b"),
  #           alpha = 0.2,
  #           height = list(unit(1.25, "line"), 
  #                         unit(0.75, "line"), 
  #                         unit(0.75, "line")),
  #           size = list(6/.pt, 6/.pt, 6/.pt),
  #           lab_color = "grey20",
  #           color = "grey20",
  #           abbrv = list(TRUE, TRUE, FALSE),
  #           rot = list(90, 0, 0),
  #           # fill = "white",
  #           expand = FALSE,
  #           lwd = list(0.1, 0.1, 0.1)) +
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
plot_fin <- plot_step / plot_line

# save
ggsave(plot_fin, 
       filename = here("figures",
                       "fig_S1.pdf"), 
       width = 183, height = 100*1.5,
       units = "mm", 
       bg = "white")
