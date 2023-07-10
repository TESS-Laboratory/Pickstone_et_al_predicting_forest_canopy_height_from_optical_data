
# load in packages --------------------------------------------------------



library(geoR)
library(gstat)
library(raster)
library(sf)
library(spdep)
library(sp)
library(sfExtras)
cube <- rast(file.path(data_path,"comb_cube.tif"))
cube

CHM <- df$CHM
CHM


# Convert the CHM raster to a spatial points data frame
chm_points <- as.data.frame(cube$CHM, xy = TRUE)
chm_points
library(geoR)


subset_points <- chm_points[sample(nrow(chm_points), 100000), ]
variogram_model <- variog(coords = subset_points[, 1:2], data = subset_points$CHM)


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
