#### GOAL: CREATE ALLUVIAL DIAGRAM W/ MAPS TO SHOW PALEOOCEAN BASINS THROUGH TIME ####
## V1
## 16.03.23
## Author: Amanda Gardiner
## Goal: Create an alluvial diagram to show the paleoocean basins and how they morphed through time
##       Create maps showing the world at set points in time to put above alluvial chart for reference.
####


# Library necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(plotly)

ocean_dat <- read_xlsx('/Users/amandagardiner/Downloads/Paleo_ocean_timeline.xlsx', 
                       sheet = 'Time_Series')

ocean_dat[ocean_dat == "Western Tethys"] <- "WT"
ocean_dat[ocean_dat == "Tethys Seaway"] <- "TS"
ocean_dat[ocean_dat == "SEAS"] <- "SAES"

colorfill <- c("#8CCD57", "#A6D84A", 
               "#FDA75F", "#FDB46C", "#FEC07A", "#FFFF00", 
               "#FFFF99", "#FFEFAF", "#FEEBD2")

alluvialplot <- ggplot(ocean_dat,
       aes(y = Count,
           axis1 = Lower_Cretaceous, axis2 = Upper_Cretaceous, 
           axis3 = Paleocene, axis4 = Eocene, axis5 = Oligocene, 
           axis6 = Miocene, axis7 = Pliocene, 
           axis8 = Pleistocene, axis9 = Holocene)) + 
  geom_alluvium(aes(fill = Lower_Cretaceous),
                width = 0.5, knot.pos = 0.2, reverse = FALSE, 
                aes.bind = FALSE) +
  scale_x_continuous(breaks = 1:9, labels = c("Lower \n Cretaceous", "Upper \n Cretaceous", 
                                              "Paleocene", 'Eocene', 'Oligocene', 'Miocene',
                                              'Pliocene', 'Pleistocene', 'Holocene')) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
            reverse = FALSE, size = 5, color = "black") + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        plot.background=element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "none"
  ) + 
  coord_flip() + 
  scale_fill_manual(values = c(Arctic = "#deebf7", Atlantic = "#c6dbef", Pacific = "#9ecae1", 
                               SAES = "#74a9cf", Tethys = "#4292c6", TS = "#2171b5", 
                               WIS = "#08519c", WT = "#08306b"))

alluvialplot


alluvialplot2 <- ggplot(ocean_dat,
                        aes(y = Count,
                            axis1 = Lower_Cretaceous, axis2 = Upper_Cretaceous, 
                            axis3 = Paleocene, axis4 = Eocene, axis5 = Oligocene, 
                            axis6 = Miocene, axis7 = Pliocene, 
                            axis8 = Pleistocene, axis9 = Holocene)) + 
  geom_alluvium(aes(fill = Holocene),
                width = 0.5, knot.pos = 0.2, reverse = FALSE, 
                aes.bind = FALSE) +
  scale_x_continuous(breaks = 1:9, labels = c("Lower \n Cretaceous", "Upper \n Cretaceous", 
                                              "Paleocene", 'Eocene', 'Oligocene', 'Miocene',
                                              'Pliocene', 'Pleistocene', 'Holocene')) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
            reverse = FALSE, size = 5, color = "black") + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        plot.background=element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "none"
  ) + 
  coord_flip() +
  scale_fill_manual(values = c(Arctic = "#c6dbef", Atlantic = "#74a9cf", Pacific = "#4292c6", Southern = "#2171b5", 
                              Indian = "#08519c", Mediterranean = "#08306b"))

alluvialplot2



ggsave(plot=alluvialplot2, filename="alluvialplot_V3.pdf", width=10, height=7)

