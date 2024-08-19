
plot_kris <- dat_pyrate_genus %>% 
  group_by(start_age) %>% 
  summarise(min_div = min(diversity), 
            max_div = max(diversity), 
            mean_div = mean(diversity)) %>% 
  ggplot(aes(start_age, mean_div)) +
  geom_ribbon(aes(ymin = min_div, 
                  ymax = max_div), 
              alpha = 0.3) +
  geom_point(shape = 21) +
  geom_line() +
  scale_x_reverse(breaks = seq(140, 0, by = -20)) +
  coord_geo(dat = list("periods"),
            pos = list("b"),
            alpha = 0.2,
            height = unit(1, "line"),
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


plot_silv <- dat_pyrate_genus %>% 
  ggplot(aes(start_age, diversity)) +
  geom_vline(xintercept = epoch_age,
             colour = "grey90") +
  geom_step(aes(group = run), 
            alpha = 0.007) +
  geom_step(data = . %>% 
              group_by(start_age, metric) %>% 
              summarise(diversity = mean(diversity)), 
            linewidth = 0.3) +
  labs(y = "Genus Diversity",
       x = "Myr",
       colour = NULL) +
  scale_x_reverse(breaks = seq(140, 0, by = -20)) +
  coord_geo(dat = list("periods"),
            pos = list("b"),
            alpha = 0.2,
            height = unit(1, "line"),
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

plot_fin <- plot_kris / plot_silv

ggsave(plot_fin, 
       filename = here("figures",
                       "comp_kris_dan.pdf"), 
       width = 183, height = 100*1.75,
       units = "mm", 
       bg = "white")
