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
df <- st_read(
        dsn ="data//SO12catchments_Susquehanna//NHDPLUS_H_0205_HU4_GDB.gdb", 
        layer = "NHDPlusFlowlineVAA") %>% 
  as_tibble()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Delineate groups
df2 <- bind_rows(
  df %>% 
    filter(StreamOrde==1) %>% 
    mutate(cat = "one"), 
  df %>% 
    filter(StreamOrde==1 | StreamOrde==2) %>% 
    mutate(cat = "one+two"), 
  df %>% 
    mutate(cat = "all")) 
 
#Plot Channel Slopes 
df2 %>% 
  select(Slope, cat) %>% 
  filter(Slope>0) %>% 
  ggplot(aes(y=Slope, x=cat)) + 
    geom_boxplot() + 
    scale_y_log10() +
    theme_bw()

#  plot boxplot based on 1st, 2nd, lowland, and all

