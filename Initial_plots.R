
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
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))

# Canopy Height Plot ------------------------------------------------------
# Read LiDAR dtm and dsm file 
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

# Calculate Canopy Height Model  ------------------------------------------

CHM <- dsm - dtm
names(CHM) <- "CHM"

CHM.plot <- ggplot() + 
  theme_bw() +
  geom_spatraster(data = CHM)+
  scale_fill_gradientn(
    name = "Canopy Height (m)",
    colors = viridisLite::turbo(n = 100),
    na.value = 'transparent',
    limits = c(0, 55)
  ) 
CHM.plot

# Sentinel 2 Plot ---------------------------------------------------------


#change coordinate system so that it matches the Canopy Height Plot, and 
#the coordinates within the introduction (EPSG:4326)

# Define the target CRS (WGS84, EPSG:4326)
target_crs <- "EPSG:4326"

# Project the raster to the target CRS
projected_cube <- project(cube, target_crs)

#convert to data table 
proj_df <- as.data.frame(projected_cube, xy=TRUE) %>%
  tidyr::drop_na()


# create a plot of Sen-2  -------------------------------------------------
S2_to_rgb_df <- function(proj_df, .min=0.001, .max=1.2){
  as.data.frame(proj_df, xy=TRUE) |>
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
rgb_plot <- function(.x){
  S2_df <- S2_to_rgb_df(.x[[c("red", "green", "blue")]]) |>  # run the function to get a dataframe
    tidyr::drop_na()
  
  ggplot(data=S2_df, aes(x=x, y=y, fill=rgb(red,green,blue))) +
    geom_raster() +
    scale_fill_identity() +
    theme_bw() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
    coord_fixed()
}


rgb_plot(projected_cube)
