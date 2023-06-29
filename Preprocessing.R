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
kat_cube <- rast(file.path(data_path, "kat_cube_repro.tif"))
lidar_cube <- rast(file.path(data_path, "lidar_cube2.tif"))
comb_cube <- rast(file.path(data_path, "comb_dat.tif"))


# Crop the data so they have the same extent ------------------------------

# Resample kat_planet to match the dimensions, resolution, and extent of dtm
#kat_planet_resampled <- resample(kat_planet, dtm)

# crop the data -----------------------------
dtm <- crop(dtm, kat_planet)
dsm <- crop(dsm, kat_planet)
kat_planet <- crop(kat_planet, dtm)


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


#kat_planet$GEMI <- ((2 *((kat_planet$nir^2.0)-(kat_planet$red^2.0)) + 1.5 * 
#                       kat_planet$nir + 0.5 * kat_planet$red)/kat_planet$nir 
#                    + kat_planet$red + 0.5)) * (1.0 - 0.25 * ((2.0 *((kat_planet$nir^2.0)- 
#                                                                       (kat_planet$red^2))))


kat_planet <- kat_cube
# Export data to tif file
writeRaster(kat_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/kat_cube.tif")


# Calculate Canopy Height Model  ------------------------------------------
CHM <- dsm - dtm
names(CHM) <- "CHM"
dtm_layer <- dtm - 0
names(dtm_layer) <- "dtm"

# calculate covariates from the dtm 
slope = terrain(dtm, v = 'slope')
aspect = terrain(dtm, v = 'aspect')
TRI = terrain(dtm, v = 'TRIrmsd')
names(TRI) <- "TRI"
rough = terrain(dtm, v = "roughness")
names(rough) <- "rough"

# label: build-cube for LiDAR data

lidar_cube <- c(dtm_layer, aspect, slope, TRI, rough, CHM)

#export lidar cube
writeRaster(lidar_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/lidar_cube2.tif")

# combine all layers from S2 and LiDAR ------------------------------------

kat_repro <- project(kat_cube, lidar_cube)
writeRaster(kat_repro, filename = "/raid/home/bp424/Documents/MTHM603/Data/kat_cube_repro.tif")


kat_df <- as.data.frame(kat_cube, xy=TRUE, na.rm = TRUE) %>%
  as_tibble()

lidar_df <- as.data.frame(lidar_cube, xy=TRUE, na.rm = TRUE) %>%
  as_tibble()

comb_dat <- c(kat_repro,lidar_cube)

writeRaster(comb_dat, filename = "/raid/home/bp424/Documents/MTHM603/Data/comb_dat.tif")

# this will be our dataframe for the ML section
comb_df <- as.data.frame(comb_cube, xy=TRUE, na.rm = TRUE) %>%
  as_tibble()

write.csv(lidar_df, file = "/raid/home/bp424/Documents/MTHM603/Data/lidar_df.csv", row.names = FALSE)


write.csv(comb_df, file = "/raid/home/bp424/Documents/MTHM603/Data/comb_df.csv", row.names = FALSE)


# plotting the spectral bands -------------------------------------

#plot the first 8 bands 

{par(mfrow=c(2,4))
  plot(kat_planet$coastal_blue, main="B1_coastal_blue", col=viridisLite::turbo(256)) 
  plot(kat_planet$blue, main="B2_blue", col=viridisLite::turbo(256)) 
  plot(kat_planet$green_i, main="B3_green_i", col=viridisLite::turbo(256))
  plot(kat_planet$green, main="B4_green", col=viridisLite::turbo(256))
  plot(kat_planet$yellow, main="B5_yellow", col=viridisLite::turbo(256))
  plot(kat_planet$red, main="B6_red", col=viridisLite::turbo(256))
  plot(kat_planet$rededge, main="B7_rededge", col=viridisLite::turbo(256))
  plot(kat_planet$nir, main="B8_nir", col=viridisLite::turbo(256))
  par(mfrow=c(1,1))}


#plot the maps...
{par(mfrow=c(1,3))
  plot(kat_planet$NDVI, main="NDVI", col=viridisLite::mako(256))
  plot(kat_planet$NDRE, main="NDRE", col=viridisLite::inferno(256))
  plot(kat_planet$EVI, main="EVI", col=viridisLite::viridis(256))
  par(mfrow=c(1,1))}

{par(mfrow=c(2,3))
  plot(kat_planet$AVI, main="AVI", col=viridisLite::turbo(256))
  plot(kat_planet$RDVI, main="RDVI", col=viridisLite::turbo(256))
  plot(kat_planet$CIRE, main="CIRE", col=viridisLite::turbo(256))
  plot(kat_planet$GNDVI, main="GNDVI", col=viridisLite::turbo(256))
  plot(kat_planet$RGBVI, main="RGBVI", col=viridisLite::turbo(256))
  plot(kat_planet$BNDVI, main="BNDVI", col=viridisLite::turbo(256))
  par(mfrow=c(1,1))}
