library(here)
library(tidyverse)
library(deeptime)

# read data ---------------------------------------------------------------

# define the bins
bins <- sort(c(0, 0.0042, 0.0082, 0.0117, 0.126, 0.781, 1.80, 2.58, 3.6, 5.333, 7.246, 11.63, 13.82,
               15.97, 20.44, 23.03, 28.1, 33.9, 37.8, 41.2, 47.8, 56.0, 59.2,
               61.6, 66.0, 72.1, 83.6, 86.3, 89.8, 93.9, 100.5, 113., 125., 129.4,
               132.9, 139.8, 155), decreasing=TRUE)

# deepdive diversity
dat_deep_genus <- read_csv(here("data", 
                                "deepdive_all_genus.csv")) %>% 
  rowid_to_column("run") %>% 
  pivot_longer(cols = -run, 
               names_to = "start_age", 
               values_to = "DeepDive") %>% 
  mutate(start_age = as.double(start_age))

# deepdive range-through diversity
dat_rt <- read_csv(here("data", 
                        "rt_genus_all.csv")) %>% 
  select(range_through_div) %>% 
  rowid_to_column("stg") %>% 
  left_join(tibble(stg = 37:1, start_age = bins)) %>% 
  select(-stg)


# visualise ---------------------------------------------------------------

# stage data
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



plot_genus <- dat_deep_genus %>%
  ggplot(aes(start_age, DeepDive)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(aes(group = run), 
            colour = "coral3",
            alpha = 0.07) +
  geom_step(data = . %>% 
              group_by(start_age) %>% 
              summarise(DeepDive = mean(DeepDive)), 
            colour = "coral3") +
  geom_step(aes(y = range_through_div), 
            colour = "#ad6d8aff",
            data = dat_rt) +
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
        panel.grid.minor = element_blank()) 

# save
ggsave(plot_genus, 
       filename = here("figures",
                       "genus_range_through.pdf"), 
       width = 183, height = 100,
       units = "mm", 
       bg = "white")
