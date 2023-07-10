
# load in packages --------------------------------------------------------
library(geoR)
library(gstat)
library(raster)
library(sf)
library(spdep)
library(sp)
library(sfExtras)

# load in required datafiles ----------------------------------------------
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))
CHM <- dsm - dtm
names(CHM) <- "CHM"

# change resolution of CHM ------------------------------------------------
# Change the resolution to 100x100 meters
CHM_100 <- aggregate(CHM, fact = 100)

CHM_100

library(sp)
# Convert the CHM raster to a spatial points data frame
CHM_points <- as.data.frame(CHM_100, xy = TRUE)

vgm1 <- variogram(log(CHM)~1, data = CHM_points)

variogram_model <- variog(coords = CHM_points[, 1:2], data = CHM_points$CHM)

v <- variogram(log(C) ~ 1, data = CHM_points)

v.fit = fit.variogram(v, vgm(1, "Sph", 900, 1))
v.fit
# Plot the variogram to visualize the autocorrelation structure
plot(variogram_model)

# Convert the CHM raster to a spatial points data frame
chm_points <- as.data.frame(cube$CHM, xy = TRUE)
#Subset the points to a random sample of 10,000 points
subset_points <- chm_points[sample(nrow(chm_points), 100000), ]
library(spdep)
# Create a spatial weights matrix
coords <- chm_points[, 1:2]
weights <- dnearneigh(coords, 0, 1000)
listw <- nb2listw(weights)

# Perform Moran's I analysis
moran_result <- moran.test(cube$CHM, listw)

# Check the Moran's I test results
moran_result
