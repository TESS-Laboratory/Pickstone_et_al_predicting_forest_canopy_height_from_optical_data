##This script is used for visualisation of the Canopy Height Model and 
#the Planet Labs & Sentinel-2  Data

# load in required packages  ----------------------------------------------
library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(raster)
library(patchwork)
library(viridisLite)

# set data path ------------------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#load in data
PScope_3m_cube <- rast(file.path(data_path,"PScope_3m.tif"))
S2_cube <- rast(file.path(data_path,"S2_comb_data.tif")) #Sentinel 2 - 10m 
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))

# Read LiDAR dtm and dsm file 
dtm.LiDAR <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

# Canopy Height Plot ------------------------------------------------------

# Calculate Canopy Height Model
CHM <- dsm - dtm.LiDAR
names(CHM) <- "CHM"

#crop the CHM to the Kat Boundary and 3m CHM
CHM.3m <- project(CHM, PScope_3m_cube)
CHM.3m <- mask(CHM.3m, PScope_3m_cube)

#plot the canopy height model

# Create the plot
(CHM.3mplot <- ggplot() + 
  theme_bw() +
  geom_spatraster(data = CHM.3m, aes(fill = lyr1)) +  # Use aes() here to specify the fill color based on 'value'
  theme(
    axis.text = element_text(size = 3, family = "Times New Roman"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 3, family = "Times New Roman"), 
    legend.title = element_text(size = 3, family = "Times New Roman"), 
    axis.ticks = element_line(linewidth = 0.1)
  ) +
  scale_fill_gradientn(
    name = "Canopy Height (m)",
    colors = viridisLite::viridis(n = 100),
    na.value = 'transparent',
    limits = c(0, 55), 
    guide = guide_colorbar(barwidth = .5, barheight = 2)
  ))


#project the CHM to 10m 
# planet labs plot --------------------------------------------------------
CHM.10m <- project(CHM, S2_cube)
CHM.10m <- mask(CHM.10m, S2_cube)

# Create the plot
(CHM.10mplot <- ggplot() + 
    theme_bw() +
    geom_spatraster(data = CHM.10m, aes(fill = lyr1)) +
    theme(
      axis.text = element_text(size = 3, family = "Times New Roman"), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.text = element_text(size = 3, family = "Times New Roman"), 
      legend.title = element_text(size = 3, family = "Times New Roman"), 
      axis.ticks = element_line(linewidth = 0.1), 
      legend.position = "none") +
    scale_fill_gradientn(
      name = "Canopy Height (m)",
      colors = viridisLite::viridis(n = 100),
      na.value = 'transparent',
      limits = c(0, 55)
    ))

# Planet Labs Plot ---------------------------------------------------------
#first convert the coordinate reference system to match that of the LiDAR
PScope_3m_cube <- mask(PScope_3m_cube, CHM.3m)

projected_cube.3m <- project(PScope_3m_cube, crs("+proj=longlat +datum=WGS84"))

PS_to_rgb_df <- function(r, .min=0.001, .max= 0.1){
  as.data.frame(r, xy=TRUE) |>
    mutate(red = case_when(red<.min ~ .min,
                           red>.max ~ .max,
                           TRUE ~ red),
           red =scales::rescale(red, c(0,1)),
           green = case_when(green<.min ~ .min,
                             green>.max ~ .max,
                             TRUE ~ green),
           green = scales::rescale(green, c(0,1)),
           blue = case_when(blue<.min ~ .min,
                            blue>.max ~ .max,
                            TRUE ~ blue),
           blue = scales::rescale(blue, c(0,1)))
  
}

#' create an RGB raster map with {ggplot2}
#'
#' @param .x SpatRaster object that contains bands named red, green and blue.
#'
#' @return A ggplot
#' 

rgb_plot <- function(.x){
  PS_df <- PS_to_rgb_df(.x[[c("red", "green", "blue")]]) |>  # run the function to get a dataframe
    tidyr::drop_na()
  
  ggplot(data=PS_df, aes(x=x, y=y, fill=rgb(red,green,blue))) +
    theme_bw()+
    geom_raster() +
    theme(axis.text = element_text(size = 3, family = "Times New Roman"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 3, family = "Times New Roman"), 
          legend.title = element_text(size = 3, family = "Times New Roman"), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.ticks = element_line(linewidth = 0.1))+
    scale_fill_identity() +
    coord_fixed()+
    coord_sf(crs = 4326)
}

#plot the rgb_plot onto the 3 m PlanetScope data
(planet.plot.3m <- rgb_plot(projected_cube.3m))


# Sentinel-2 Data plot ---------------------------------
S2_cube <- mask(S2_cube, CHM.10m)
projected_cube.S2 <- project(S2_cube, crs("+proj=longlat +datum=WGS84"))


S2_to_rgb_df <- function(r, .min=0.001, .max=0.13, .yr=2021){
  as.data.frame(r, xy=TRUE) |>
    mutate(red = case_when(red<.min ~ .min,
                           red>.max ~ .max,
                           TRUE ~ red),
           red =scales::rescale(red, c(0,1)),
           green = case_when(green<.min ~ .min,
                             green>.max ~ .max,
                             TRUE ~ green),
           green = scales::rescale(green, c(0,1)),
           blue = case_when(blue<.min ~ .min,
                            blue>.max ~ .max,
                            TRUE ~ blue),
           blue = scales::rescale(blue, c(0,1)), 
           Year = .yr)
  
}

#' create an RGB raster map with {ggplot2}
#'
#' @param .x SpatRaster object that contains bands named red, green and blue.
#'
#' @return A ggplot
#' 

rgb_plot <- function(.x){
  S2_df <- S2_to_rgb_df(.x[[c("red", "green", "blue")]]) |>  # run the function to get a dataframe
    tidyr::drop_na()
  
  ggplot(data=S2_df, aes(x=x, y=y, fill=rgb(red,green,blue))) +
    theme_bw()+
    geom_raster() +
    theme(axis.text = element_text(size = 3, family = "Times New Roman"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 3, family = "Times New Roman"), 
          legend.title = element_text(size = 3, family = "Times New Roman"), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.ticks = element_line(linewidth = 0.1))+
    scale_fill_identity() +
    coord_fixed()+
    coord_sf(crs = 4326)
}

#project the rgb plot onto the Sentinel-2 spatraster 
(S2_plot.10m <- rgb_plot(projected_cube.S2))


# combine the two plots and save ------------------------------------------

CHM.3mplot + planet.plot.3m + CHM.10mplot + S2_plot.10m


ggsave(file="combined_data_plot.png", dpi = 600)

