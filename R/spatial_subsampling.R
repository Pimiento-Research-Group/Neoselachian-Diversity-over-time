library(here)
library(tidyverse)
library(readxl)
library(divvy)

# read data ---------------------------------------------------------------

# species level
dat_species <- read_xlsx(here("data",
                              "species.xlsx"))

# genus level
dat_genus <- read_xlsx(here("data",
                            "genus.xlsx"))


# define the bins
bins <- sort(c(145, 139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
               89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
               47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
               13.820, 11.630, 7.246, 5.333, 2.580, 0), 
             decreasing = TRUE)




# temporally bin data -----------------------------------------------------

dat_spec_bin <- dat_species %>% 
  select(accepted_name, genus, 
         family, order, superorder, 
         paleolat, paleolon,
         max_ma, min_ma) %>% 
  mutate(age_mean = ((max_ma - min_ma)/2) + min_ma,
         stg = cut(age_mean, 
                   breaks = bins, 
                   include.lowest = TRUE, 
                   labels = FALSE))

dat_gen_bin <- dat_genus %>% 
  select(accepted_name, genus, 
         family, order, superorder, 
         paleolat, paleolon,
         max_ma, min_ma) %>% 
  mutate(age_mean = ((max_ma - min_ma)/2) + min_ma,
         stg = cut(age_mean, 
                   breaks = bins, 
                   include.lowest = TRUE, 
                   labels = FALSE))




# perform circular subsampling  within each bin -----------------------------------------------


# perform circular subsampling
list_div_spec <- dat_spec_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         cookies(xy = 1:2,
                                 iter = 500,
                                 nSite = 8,
                                 r = 1000,
                                 output = "full"), 
                       .progress = TRUE))
   
# summarise and save
list_div_spec %>% 
  mutate(n_occs = map(dat_sub, ~ .x %>%
                           map("accepted_name") %>%
                           map_dbl(n_distinct)), 
         mean_div = map_dbl(n_occs, mean), 
         min_div = map_dbl(n_occs, min), 
         max_div = map_dbl(n_occs, max)) %>% 
  arrange(stg) %>% 
  add_column(start_age = rev(bins[3:29])) %>% 
  select(stg, mean_div, min_div, max_div, start_age) %>% 
  write_rds(here("data",
                 "diversity_continuous_divvy_species.rds"), 
            compress = "gz")
  

# same for genus level
# perform circular subsampling
list_div_gen <- dat_gen_bin %>% 
  filter(stg <= 27) %>% 
  group_by(stg) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(dat_sub = map(data,
                       ~ .x %>%
                         select(paleolon, paleolat, everything()) %>%
                         as.data.frame() %>%
                         cookies(xy = 1:2,
                                 iter = 500,
                                 nSite = 8,
                                 r = 1000,
                                 output = "full"), 
                       .progress = TRUE))


# summarise and save
list_div_gen %>% 
  mutate(n_occs = map(dat_sub, ~ .x %>%
                        map("accepted_name") %>%
                        map_dbl(n_distinct)), 
         mean_div = map_dbl(n_occs, mean), 
         min_div = map_dbl(n_occs, min), 
         max_div = map_dbl(n_occs, max)) %>% 
  arrange(stg) %>% 
  add_column(start_age = rev(bins[3:29])) %>% 
  select(stg, mean_div, min_div, max_div, start_age) %>% 
  write_rds(here("data",
                 "diversity_continuous_divvy_genus.rds"), 
            compress = "gz")


