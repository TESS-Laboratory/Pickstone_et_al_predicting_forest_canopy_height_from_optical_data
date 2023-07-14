
# load in packages --------------------------------------------------------
library(geoR)
library(gstat)
library(terra)
library(sf)
library(spdep)
library(sp)
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
file_name <- "variogram.png"
dpi <- 600

# Create the PNG device with high resolution
png(file = file_name, width = 8, height = 6, units = "in", res = dpi)

# Plot the variogram to visualize the autocorrelation structure
# Set the font family to Times New Roman
par(family = "Times New Roman", cex.lab = 1.5)

# Plot the variogram model without axes
plot(variogram_model, xlim = c(0, 10000), ylim = c(0, 60),
     xlab = "Distance (m)", ylab = "Semi-variance")

dev.off()

# Check spatial correlation using Moran's I -------------------------------

spcor <- terra::autocor(CHM_100, method="moran", global=TRUE)

