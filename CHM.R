# Install and load required packages
library(terra)
library(tidyterra)
library(colorspace)
library(sf)
library(tidyverse)
library(raster)
library(dplyr)
library(geosphere)
library(gridExtra)

#set working directory 
setwd("~/OneDrive - University of Exeter/University/Dissertation/Data")

# Import data -------------------------------------------------------------

# Read LiDAR dtm and dsm file 
dtm <- rast("katingan_DEMS/katingan_DTM.tif")
dsm <- rast("katingan_DEMS/katingan_DSM.tif")

#read in the katingan geopackage
kat <- st_read("katingan_aoi.gpkg")


# create plots of data  ---------------------------------------------------

#create plot of digital terrain model 
dtm.plot <- 
  ggplot() +
  theme_bw()+
  geom_spatraster(data = dtm) +
  scale_fill_continuous_sequential('Terrain 2', na.value = 'transparent', 
                                   rev = TRUE, name = "Terrain Elevation (m)",
                                   limits = c(0, 10)) +
  geom_sf(data = kat, fill = "transparent", color = "black") +
  coord_sf(ylim = c(9664011, 9690000)) +
  annotate("text", x = -Inf, y = Inf, label = "A.", 
           fontface = "bold", size = 5, hjust = -0.5, vjust = 2) +
  labs(x = NULL, y = NULL)

dtm.plot

dsm
#create plot of digital surface model 
dsm.plot <- ggplot() +
  theme_bw()+
  geom_spatraster(data = dsm) +
  scale_fill_continuous_sequential('Viridis', na.value = 'transparent', 
                                   rev = TRUE, name = "Surface Elevation (m)", 
                                   limits = c(0, 40)) +
  geom_sf(data = kat, fill = "transparent", color = "black") +
  coord_sf(ylim = c(9664011, 9690000)) +
  annotate("text", x = -Inf, y = Inf, label = "B.", fontface = "bold", 
           size = 5, hjust = -0.5, vjust = 2) +
  labs(x = NULL, y = NULL)

dsm.plot

# Arrange plots side by side
grid.arrange(dtm.plot, dsm.plot, ncol = 2)

# calculate canopy height -------------------------------------------------

# Calculate canopy height
canopy_height <- dsm - dtm
canopy_height

# Plot the canopy height model
height.plot = 
  ggplot()+ 
  theme_bw()+
  geom_spatraster(data=canopy_height)+ 
  scale_fill_continuous_sequential('Emrld',na.value='transparent',rev=T, 
                                   name = "Canopy Height (m)", 
                                   limits = c(0, 55))+ 
  geom_sf(data = kat, fill = "transparent", color = "black")+ 
  coord_sf(ylim = c(9664011, 9690000))

height.plot


# calculation of other landscape metrics ----------------------------------

slope = terrain(dtm, v = 'slope', unit = "degrees")
aspect = terrain(dtm, v = 'aspect')
slope

# Plot slope and aspect ---------------------------------------------------

slope.plot <- 
  ggplot() + 
  theme_bw() +
  geom_spatraster(data = slope) + 
  scale_fill_continuous_sequential('Mako', na.value = 'transparent', rev = TRUE, 
                                   name = "Slope (\u00B0)", limits = c(0, 50)) + 
  geom_sf(data = kat, fill = "transparent", color = "black") + 
  coord_sf(ylim = c(9664011, 9690000))+ 
  annotate("text", x = -Inf, y = Inf, label = "A.", fontface = "bold", 
           size = 5, hjust = -0.5, vjust = 2) +
  labs(x = NULL, y = NULL)

slope.plot

aspect.plot <- 
ggplot() + 
  theme_bw() +
  geom_spatraster(data = aspect) + 
  scale_fill_continuous_sequential('Sunset', na.value = 'transparent', rev = TRUE, 
                                   name = "Aspect (\u00B0)") + 
  geom_sf(data = kat, fill = "transparent", color = "black") + 
  coord_sf(ylim = c(9664011, 9690000))+ 
  annotate("text", x = -Inf, y = Inf, label = "B.", fontface = "bold", 
           size = 5, hjust = -0.5, vjust = 2) +
  labs(x = NULL, y = NULL)

aspect.plot

grid.arrange(slope.plot, aspect.plot, ncol = 2)
# Calculate min curvature and mean curvature ------------------------------

curvature <- geosphere::curvature(slope, aspect)


# random selection  -------------------------------------------------------



# Calculate the number of cells to sample
n_cells <- ncell(canopy_height)
sample_size <- floor(0.8 * n_cells)

# Randomly select cells to sample
sampled_cells <- sample(1:n_cells, size = sample_size)

# Create a subset of the canopy_height using the sampled cells
sampled_canopy <- canopy_height[sampled_cells]
sampled_canopy

# Plot the sampled canopy height model
height.plot_80 <- 
  ggplot() + as.d
  theme_bw() +
  geom_spatraster(data = sampled_canopy) + 
  scale_fill_continuous_sequential('Emrld', na.value = 'transparent', rev = TRUE, 
                                   name = "Canopy Height (m)", 
                                   limits = c(0, 55)) + 
  geom_sf(data = kat, fill = "transparent", color = "black") + 
  coord_sf(ylim = c(9664011, 9690000))

height.plot_80


