##This script is used for visualisation of
##Katingan Mentaya Project, in Central Kalimantan, Indonesia
# load in required packages  ----------------------------------------------
library(tidyverse)
library(tmap)

#define data path
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#load in Katingan Mentaya Project 
kat_boundary <- st_read(file.path(data_path, "katingan_aoi.gpkg"))

tm_basic <- function() {
  tmap::tm_basemap(tmap::providers$Esri.WorldImagery) +
    tmap::tm_basemap(tmap::providers$OpenStreetMap.HOT) +
    tmap::tm_basemap(tmap::providers$Stamen.Terrain) +
    tmap::tm_basemap(tmap::providers$CartoDB.DarkMatter)
}

(kat_plot <- tm_shape(kat_boundary) + 
  tm_borders(lwd = 2, col = "gray60") +   # Adding a border to the boundary outline
  tm_basic())  

tmap_save(kat_plot, "kat_map.png", dpi = 600)
