##This script is used for visualisation of the Canopy Height Model and 
#the Planet Labs Optical Data

# load in required packages  ----------------------------------------------
library(terra)
library(tidyterra)
library(sf)
library(tidyverse)
library(raster)
library(patchwork)

# load in data ------------------------------------------------------------

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))

projected_cube <- project(cube, crs("+proj=longlat +datum=WGS84"))

# Canopy Height Plot ------------------------------------------------------
# Read LiDAR dtm and dsm file 
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

# Calculate Canopy Height Model  ------------------------------------------

CHM <- dsm - dtm
names(CHM) <- "CHM"

#plot the canopy height model
(CHM.plot <- ggplot() + 
  theme_bw() +
  geom_spatraster(data = CHM)+
  theme(axis.text = element_text(size = 4, family = "Times New Roman"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 4, family = "Times New Roman"), 
        legend.title = element_text(size = 4, family = "Times New Roman"))+
  scale_fill_gradientn(
    name = "Canopy Height (m)",
    colors = viridisLite::viridis(n = 100),
    na.value = 'transparent',
    limits = c(0, 55)))

CHM.plot


# Planet Labs Plot ---------------------------------------------------------

S2_to_rgb_df <- function(df, .min=0.001, .max=1.2){
  as.data.frame(df, xy=TRUE) |>
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
    theme_bw()+
    geom_raster() +
    theme(axis.text = element_text(size = 4, family = "Times New Roman"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 4, family = "Times New Roman"), 
          legend.title = element_text(size = 4, family = "Times New Roman"), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank())+
    scale_fill_identity() +
    coord_fixed()+
    coord_sf(crs = 4326)
}


rgb_plot(projected_cube)

planet.plot <- rgb_plot(projected_cube)



# combine the two plots and save ------------------------------------------

combined_plot <- CHM.plot + planet.plot
combined_plot

ggsave(file="combined_data_plot.png", dpi = 600)

