library(terra)
library(viridisLite)
library(colorspace)
library(tidyverse)
library(sf)
library(raster)


# Import Planet Labs Data -------------------------------------------------

#data_path <- "/Users/bri/Library/CloudStorage/OneDrive-UniversityofExeter/University/Dissertation/Data"
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
kat_planet

print(names(kat_planet))



# import the LiDAR data ---------------------------------------------------

# Read LiDAR dtm and dsm file 
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))

#scale down the 8 existing spectral bands
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
kat_planet$AVI <- (kat_planet$nir * (1.0 - kat_planet$red) * 
                     (kat_planet$nir - kat_planet$red))/3
kat_planet$RDVI <- (kat_planet$nir - kat_planet$red)/((kat_planet$nir + kat_planet$red)^0.5)
kat_planet$CIRE <- ((kat_planet$nir/kat_planet$rededge)-1)
kat_planet$GNDVI <- (kat_planet$nir - kat_planet$green)/
  (kat_planet$nir + kat_planet$green)
kat_planet$RGBVI <- (kat_planet$green^2 - kat_planet$blue * kat_planet$red)/
  (kat_planet$green^2 + kat_planet$blue * kat_planet$red)
kat_planet$BNDVI <- (kat_planet$nir - kat_planet$blue)/(kat_planet$nir + kat_planet$blue)
kat_planet$CVI <- (kat_planet$nir * kat_planet$red)/(kat_planet$green^2.0)
kat_planet$GBNDVI <- (kat_planet$nir - (kat_planet$green + kat_planet$blue))/
  (kat_planet$nir + (kat_planet$green + kat_planet$blue))
kat_planet$gli <- (2.0 * kat_planet$green - kat_planet$red - kat_planet$blue)/
  (2.0 * kat_planet$green + kat_planet$red + kat_planet$blue)

#kat_planet$GEMI <- ((2 *((kat_planet$nir^2.0)-(kat_planet$red^2.0)) + 1.5 * 
#                       kat_planet$nir + 0.5 * kat_planet$red)/kat_planet$nir 
#                    + kat_planet$red + 0.5)) * (1.0 - 0.25 * ((2.0 *((kat_planet$nir^2.0)- 
#                                                                       (kat_planet$red^2))))


# Calculate Canopy Height Model  ------------------------------------------
kat_planet$CHM <- dsm - dtm
canopy_height


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
