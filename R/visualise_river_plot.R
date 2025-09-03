# load libraries
library(here)
library(readxl)
library(tidyverse)
library(ggalluvial)




dat_ocean <- read_xlsx(here("data", "Paleo_ocean_timeline.xlsx"), 
                       sheet = 'Time_Series')

dat_ocean_age <- read_xlsx(here("data", "Paleo_ocean_timeline.xlsx"),
                           sheet = 1) %>% 
  select(basin = `Paleo-Ocean Basin`, 
         epoch = `Start Epoch`, 
         age = `Start MA`)

epoch_cor <- read_rds(here("data",
                           "epoch_cor.rds"))

dat_fins <- read_xlsx(here("data",
                           "species.xlsx"))



dat_ocean_clean <- dat_ocean %>%
  select(-Count) %>% 
  pivot_longer(cols = everything(), 
               values_to = "basin", 
               names_to = "epoch") %>% 
  mutate(epoch = str_replace_all(epoch, "_", " ")) %>% 
  left_join(dat_fins %>% 
              group_by(paleoocean, epoch = early_epoch) %>% 
              summarise(pal_lat = mean(latitude)) %>% 
              ungroup() %>% 
              select(basin = paleoocean, pal_lat, epoch) %>% 
              distinct(epoch, basin, pal_lat)) %>% 
  left_join(dat_fins %>% 
              distinct(accepted_name, paleoocean, early_epoch) %>%
              count(basin = paleoocean, epoch = early_epoch)) %>% 
  left_join(epoch_cor %>% 
              select(epoch = name, start_age = max_age) %>% 
              mutate(epoch = str_replace_all(epoch, "Late", "Upper"), 
                     epoch = str_replace_all(epoch, "Early", "Lower"), 
                     epoch = str_replace_all(epoch, "Pli", "Pliocene"),
                     epoch = str_replace_all(epoch, "Ple", "Pleistocene"))) %>% 
  add_column(id = rep(1:11, each = 9)) %>%
  left_join(tibble(basin = unique(.$basin), 
                   basin_leg = c("Pacific", "WIS", 
                                 "Atlantic", "WT", "Tethys", 
                                 "Neo-Tethys", "TS", "SAES", 
                                 "Arctic", "TSS", 
                                 "Mediterranean", "Southern", "Indian"))) %>% 
  filter(epoch != "Holocene") %>% 
  mutate(epoch = str_replace_all(epoch, " ", "\n"), 
         epoch = fct_reorder(epoch, start_age),
         basin_leg = fct_reorder(basin_leg, desc(pal_lat), 
                                 .na_rm = TRUE)) %>% 
  replace_na(list(n = 1)) 


plot_ocean <- dat_ocean_clean %>%
  ggplot(aes(x = epoch, stratum = basin_leg, log(1+n),
             alluvium = id, fill = basin_leg, label = basin_leg)) +
  geom_flow(aes(colour = basin_leg), 
            stat = "alluvium", lode.guidance = "frontback", 
            alpha = 1, 
            width = 0) +
  geom_label(aes(colour = basin_leg),
             stat = "stratum",  
             label.r = unit(0.5, "lines"),
             label.size = 0, 
             size = 6/.pt,
             fill = "white", 
             show.legend = FALSE) +
  scale_size_identity(guide = "legend") +
  scale_fill_viridis_d(labels = dat_ocean_clean %>%
                         mutate(basin = fct_reorder(basin,
                                                    desc(pal_lat),
                                                    .na_rm = TRUE)) %>%
                         distinct(basin) %>%
                         arrange(basin) %>%
                         pull(basin)) +
  scale_colour_viridis_d(guide = 'none') +
  scale_x_discrete(limits = rev) +
  theme_minimal() +
  guides(fill = guide_legend(ncol = 4, byrow = TRUE)) +
  labs(x = NULL, y = "Occurrences",
       fill = NULL, colour = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.5), 
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        # legend.position = c(0.85, 0.9),
        legend.position = "bottom",
        legend.key.size = unit(3, "mm"), 
        legend.text = element_text(size = 7))

# save
ggsave(plot_ocean, 
       filename = here("figures",
                       "fig_S1.pdf"), 
       width = 183*1.5, height = 150,
       units = "mm", 
       bg = "white")
