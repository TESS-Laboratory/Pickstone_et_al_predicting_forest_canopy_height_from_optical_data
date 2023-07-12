#The following code is used for pre-processing the Sentinel-2 and LiDAR data
#This includes calculation of vegetation spectral indices and topographic metrics. 
#As well as resampling the data so they all have the same resolution 


# import packages needed for the analysis ---------------------------------
library(terra)
library(viridisLite)
library(colorspace)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)

# Import Planet Labs Data -------------------------------------------------

#data_path <- "/Users/bri/Library/CloudStorage/OneDrive-UniversityofExeter/University/Dissertation/Data"
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))

print(names(kat_planet))

# import the LiDAR data ---------------------------------------------------

# Read LiDAR dtm and dsm file 
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

dsm
# Crop the data so they have the same extent ------------------------------
#this will also save computing time for the calculations 
kat_planet <- crop(kat_planet, dtm)
dtm <- crop(dtm, kat_planet)
dsm <- crop(dsm, kat_planet)


#scale down the 8 existing bands
kat_planet$coastal_blue <- (kat_planet$coastal_blue/10000)
kat_planet$blue <- (kat_planet$blue/10000)
kat_planet$green_i <- (kat_planet$green_i/10000)
kat_planet$green <- (kat_planet$green/10000)
kat_planet$yellow <- (kat_planet$yellow/10000)
kat_planet$red <- (kat_planet$red/10000)
kat_planet$rededge <- (kat_planet$rededge/10000)
kat_planet$nir <- (kat_planet$nir/10000)


# Do the band math - HG examples
kat_planet$NDVI <- (kat_planet$nir - kat_planet$red)/(kat_planet$nir +kat_planet$red)
kat_planet$NDRE <- (kat_planet$nir - kat_planet$rededge)/(kat_planet$nir +kat_planet$rededge)
kat_planet$EVI <- 2.5 * ((kat_planet$nir) - (kat_planet$red)) /
  ((kat_planet$nir) + 6 * (kat_planet$red) - 7.5 * (kat_planet$blue) + 1)

#Calculations by BP
kat_planet$AVI <- (kat_planet$nir * (1 - kat_planet$red)*(kat_planet$nir - kat_planet$red))^(1/3)
kat_planet$RDVI <- (kat_planet$nir - kat_planet$red)/((kat_planet$nir + kat_planet$red)^0.5)
kat_planet$CIRE <- ((kat_planet$nir/kat_planet$rededge)-1)
kat_planet$GNDVI <- (kat_planet$nir - kat_planet$green)/
  (kat_planet$nir + kat_planet$green)
kat_planet$RGBVI <- (kat_planet$green^2 - kat_planet$blue * kat_planet$red)/
  (kat_planet$green^2 + kat_planet$blue * kat_planet$red)
kat_planet$BNDVI <- (kat_planet$nir - kat_planet$blue)/(kat_planet$nir + kat_planet$blue)
kat_planet$CVI <- (kat_planet$nir * kat_planet$red)/(kat_planet$green^2)
kat_planet$GBNDVI <- (kat_planet$nir - (kat_planet$green + kat_planet$blue))/
  (kat_planet$nir + (kat_planet$green + kat_planet$blue))
kat_planet$GLI <- (2 * kat_planet$green - kat_planet$red - kat_planet$blue)/
  (2 * kat_planet$green + kat_planet$red + kat_planet$blue)
kat_planet$GEMI <- ((2 *((kat_planet$nir^2.0)-(kat_planet$red^2.0)) + 1.5 * 
                       kat_planet$nir + 0.5 * kat_planet$red)/(kat_planet$nir + kat_planet$red + 0.5)) * 
  (1.0 - 0.25 * ((2.0 *((kat_planet$nir^2.0)- (kat_planet$red^2)) + 
                    1.5 * kat_planet$nir + 0.5 * kat_planet$red)/(kat_planet$nir + kat_planet$red + 0.5))) - 
  ((kat_planet$red - 0.125)/(1 - kat_planet$red))


# Calculate Canopy Height Model  ------------------------------------------
CHM <- dsm - dtm
names(CHM) <- "CHM"

# calculate topographic metrics from the dtm 
slope = terrain(dtm, v = 'slope')
aspect = terrain(dtm, v = 'aspect')
TRI = terrain(dtm, v = 'TRIrmsd')
names(TRI) <- "TRI"
rough = terrain(dtm, v = "roughness")
names(rough) <- "rough"


# build-cube for LiDAR data
lidar_cube <- c(dtm, aspect, slope, TRI, rough, CHM)

# combine all layers from S2 and LiDAR ------------------------------------

#first ensure both cubes have the same resolution 
lidar_cube_r <- project(lidar_cube, kat_planet)

#combine Sentinel 2 and Lidar Data
comb_dat <- c(kat_planet, lidar_cube_r)

#export file, so that the combined raster file can be used in other analyses 
writeRaster(comb_dat, filename = "/raid/home/bp424/Documents/MTHM603/Data/comb_cube.tif")

#convert to data table 
comb_df <- as.data.frame(comb_dat, xy=TRUE) %>%
  tidyr::drop_na()

#export data table so it can be used in other analyses 
write.csv(comb_df, file = "/raid/home/bp424/Documents/MTHM603/Data/final_df.csv", row.names = FALSE)


#create a histogram of the canopy heights
heights <- df$CHM
min(heights)
max(heights)
# Create a histogram
hist(heights, breaks = 10, main = "Frequency Histogram of CHM Heights", xlab = "Height", ylab = "Frequency")
