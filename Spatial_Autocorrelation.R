
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



# Visualise the resampling strategy  --------------------------------------

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))

# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df_10,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

#create the resampling strategy for the train and test set 
resample <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

# Create the autoplot with custom font and tilted x-axis labels
autoplot(resample, task = task, fold_id = 1:1) +
  theme(
    text = element_text(size = 7, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 2:2) +
  theme(
    text = element_text(size = 7,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 3:3) +
  theme(
    text = element_text(size = 7,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file="Fold_3.png", dpi = 600)



