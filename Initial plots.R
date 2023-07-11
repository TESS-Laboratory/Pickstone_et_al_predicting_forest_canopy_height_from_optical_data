
# load in required packages  ----------------------------------------------
library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(raster)
library(dplyr)
# load in data ------------------------------------------------------------

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
kat_planet
# create a plot of Sen-2  -------------------------------------------------




# Canopy Height Plot ------------------------------------------------------
# Read LiDAR dtm and dsm file 
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

# Calculate Canopy Height Model  ------------------------------------------

CHM <- dsm - dtm
names(CHM) <- "CHM"

height.plot <- ggplot() + 
  theme_bw() +
  geom_spatraster(data = CHM) + 
  scale_fill_gradientn(
    name = "Canopy Height (m)",
    colors = viridisLite::turbo(n = 100),
    na.value = 'transparent',
    limits = c(0, 55)
  ) 

