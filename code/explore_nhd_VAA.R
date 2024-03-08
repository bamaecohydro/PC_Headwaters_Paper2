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
library(patchwork)

# download data
df <- st_read(
        dsn ="data//SO12catchments_Susquehanna//NHDPLUS_H_0205_HU4_GDB.gdb", 
        layer = "NHDPlusFlowlineVAA") %>% 
  as_tibble()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Create Plots ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# delineate groups
df2 <- bind_rows(
  df %>% 
    filter(StreamOrde==1) %>% 
    mutate(cat = "1st"), 
  df %>% 
    filter(StreamOrde==1 | StreamOrde==2) %>% 
    mutate(cat = "1st and 2nd"), 
  df %>% 
    mutate(cat = "All Streams")) 
 
df2$cat <- factor(df2$cat, levels = c('1st','1st and 2nd', 'All Streams'),ordered = TRUE)

# plot Channel Slopes 
slope_plot <- df2 %>% 
  select(Slope, cat) %>% 
  filter(Slope>0) %>% 
  ggplot(aes(y=Slope, x=cat)) + 
    geom_boxplot(outlier.shape = NA) + 
    theme_bw() + 
      scale_y_log10() +    
      xlab(NULL) +
      ylab("Slope [m/m]") +
      theme(
        axis.title.y = element_text(size = 14), 
        axis.text.y  = element_text(size = 10), 
        axis.text.x  = element_text(size = 14)
      )
      
# plot channel elevations
elevation_plot <- df2 %>% 
  select(MaxElevSmo, cat) %>% 
  filter(MaxElevSmo>0) %>% 
  ggplot(aes(y=MaxElevSmo, x=cat)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() + 
  scale_y_log10(limits = c(1e4, 1e5)) +    
  xlab(NULL) +
  ylab("Max Reach Elevation") +
  theme(
    axis.title.y = element_text(size = 14), 
    axis.text.y  = element_text(size = 10), 
    axis.text.x  = element_text(size = 14)
  ) 
elevation_plot


# plot watershed areas
watershed_area_plot <- df2 %>% 
  select(TotDASqKm, cat) %>% 
  filter(TotDASqKm>0) %>% 
  ggplot(aes(y=TotDASqKm, x=cat)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_bw() + 
  scale_y_log10(limits = c(1e-2, 1e3)) +    
  xlab(NULL) +
  ylab("Drainage Area [km^2]") +
  theme(
    axis.title.y = element_text(size = 14), 
    axis.text.y  = element_text(size = 10), 
    axis.text.x  = element_text(size = 14)
  ) 
watershed_area_plot

# print plots 
(elevation_plot / slope_plot / watershed_area_plot) +
    plot_layout(axes = "collect")

ggsave("docs//VAA_headwater_plots.png", width = 5, height = 7, units = "in")
