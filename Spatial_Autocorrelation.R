
# load in packages --------------------------------------------------------
library(geoR)
library(gstat)
library(terra)
library(sf)
library(spdep)
library(sp)
library(sfExtras)
library(tidyverse)

# load in required datafiles ----------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))
CHM <- dsm - dtm
names(CHM) <- "CHM"

# Pre-process CHM  ------------------------------------------------
# Change the resolution to 100x100 meters
CHM_100 <- aggregate(CHM, fact = 100)

# Convert the CHM raster to a spatial points data frame
CHM_points <- as.data.frame(CHM_100, xy = TRUE)
coordinates(CHM_points) <- ~x + y

# Create Variogram --------------------------------------------------------

# Create variogram model
variogram_model <- variogram(CHM ~ 1, data = CHM_points)

# Plot the variogram to visualize the autocorrelation structure
plot(variogram_model, xlim = c(0, 10000), ylim = c(0, 60))



# Check spatial correlation using Moran's I -------------------------------


# Create a spatial weights matrix
CHM_points
coords <- CHM_points[, 1:2]
weights <- dnearneigh(coords, 0, 8000)
listw <- nb2listw(weights)

# Perform Moran's I analysis
moran_result <- moran.test(CHM_points$CHM, listw)

# Check the Moran's I test results
moran_result
