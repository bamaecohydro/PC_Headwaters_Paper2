#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Explore NHD VAA 
# Coders: Delaney Peterson, Adam Price, and Nate Jones
# Date: 3/7/2024
# Purpose: Explore NHD VAA from EPA 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace ------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
remove(list=ls())

# library
library(sf)
library(ggplot2)
library(tidyverse)

#download data
catchment <- st_read(
  dsn ="data//SO12catchments_Susquehanna//NHDPLUS_H_0205_HU4_GDB.gdb", 
  layer = "NHDPlusFlowlineVAA")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
catchment %>% 
  select(Slope) %>% 
  filter(Slope>0) %>% 
  ggplot(aes(y=Slope)) + 
    geom_boxplot() + 
    scale_y_log10() +
    theme_bw()

#  plot boxplot based on 1st, 2nd, lowland, and all

