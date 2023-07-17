library(mlr3)
library(mlr3spatiotempcv)
library(raster)
library(sf)
library(ggplot2)


cube_10 <- rast(file.path(data_path,"comb_cube_10m.tif"))
library(raster)
library(sf)

# Subset the dataframe based on the spatial extent of the spatraster
extent <- ext(cube_10)
subset_df <- df_10[df_10$x >= extent[1] & df_10$x <= extent[2] & df_10$y >= extent[3] & df_10$y <= extent[4], ]

# Randomly select 10% of the data points
sampled_df <- subset_df %>% sample_frac(0.1)


sampled_df

autoplot(sampled_df)

  sampled_df
# Load the spatraster

xmin
# Create the grid
grid_size <- 4500
nrow <- ceiling(nrow(cube_10) / grid_size)
ncol <- ceiling(ncol(cube_10) / grid_size)
xmin <- extent(cube_10)[1]
xmax <- extent(cube_10)[2]
ymin <- extent(cube_10)[3]
ymax <- extent(cube_10)[4]
grid <- raster(nrows = nrow, ncols = ncol, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax)
res(grid) <- grid_size  # Set the resolution of the grid cells

#Convert the grid to a SpatialPolygonsDataFrame
grid_polygons <- rasterToPolygons(grid, dissolve = TRUE)

# Convert the grid to a SpatialPolygons object
grid_polygons <- as(grid, "SpatialPolygons")

## Iterate over the grid cells and extract data from the spatraster
for (i in 1:length(grid_polygons)) {
  grid_cell <- grid_polygons[i]
  
  # Convert the grid cell to a RasterLayer
  grid_cell_raster <- rasterize(grid_cell, cube_10, getCover=TRUE)
  
  # Extract data from spatraster within the grid cell
  grid_data <- extract(cube_10, grid_cell_raster)
  
  # Perform further operations with the grid_data...
}
#calculate the grid cell indices for each point
spatial_df$grid_x <- floor(spatial_df / grid_size)
spatial_df$grid_y <- floor(spatial_df / grid_size)

# Split the spatial dataframe into spatially defined grids
grids <- st_split(spatial_df, st_as_sfc(st_bbox(spatial_df), crs = st_crs(spatial_df)))

# Access individual grid dataframes
# For example, to access the first grid dataframe:
grid_1 <- grids[[1]]

set.seed(1234)

library(raster)


library(terra)



# Define the grid size in meters
grid_size <- c(4500, 4500)



# Get the extent of the SpatRaster
extent_cube_10 <- ext(cube_10)

# Create an empty list to store the grid rasters
grid_rasters <- list()



library(data.table)
library(dplyr)
# Define the grid size in meters
grid_size <- 4500



# Calculate the gdrid cell indices for each coordinate
df_10$grid_x <- floor(df_10$x / grid_size)
df_10$grid_y <- floor(df_10$y / grid_size)
# Calculate the grid cell indices for each coordinate
df_10 <- df_10 %>%
  mutate(grid_x = floor(df_10$x / grid_size),
         grid_y = floor(df_10$y / grid_size))

# Identify unique grid cells
unique_grids <- unique(df_10[, c("grid_x", "grid_y")])

# Randomly select a grid cell
random_grid <- unique_grids[sample(nrow(unique_grids), 1), ]

# Filter the dataframe based on the selected random grid
random_grid_data <- df_10 %>%
  filter(grid_x == random_grid$grid_x, grid_y == random_grid$grid_y)
Please ensure that the grid_size variable is properly defined before running this code.






Regenerate response
# Access individual grid rasters
# For example, to access the first grid raster:
grid_1 <- grid_rasters[[1]]

Define the grid size in meters
grid_size <- c(4500, 4500)

# Split the SpatRaster into grids
grid_rasters <- tile(cube_10, width = grid_size[1], height = grid_size[2])

# Access individual grid rasters
# For example, to access the first grid raster:
grid_1 <- grid_rasters[[1]]

grid = st_make_grid(cube_10,cellsize=4500,square = T)

# Define the grid size in meters
grid_size <- 4500

grid_int = st_geometry(grid, cube_10)

grid_int
grid

# Create a raster layer representing the grid
grid_raster <- rast(cube_10)
res(grid_raster) <- c(grid_size, grid_size)

# Crop the grid to the extent of the SpatRaster
grid_raster_cropped <- crop(grid_raster, cube_10)

# Visualize the grid raster
plot(grid_raster_cropped)
grid_raster_cropped

# Calculate the number of rows in df_10
num_rows <- nrow(df_10)

# Calculate the number of rows to select for the 10% subset
num_rows_subset <- ceiling(num_rows * 0.1)

# Randomly select the starting row index for the subset
start_row <- sample(1:(num_rows - num_rows_subset + 1), 1)

# Create the subset by selecting consecutive rows
subset_10_percent <- df_10 %>%
  slice(start_row:(start_row + num_rows_subset - 1))


# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "kat_base_CHM",
  backend = subset_10_percent,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

# Define your model
learner <- lrn("regr.lm")

resampling <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

measure <- msr("regr.rsq")

afs=auto_fselector( 
  fselector=fs("random_search"), 
  learner=learner, 
  resampling=resample, 
  measure=measure, 
  term_evals=10)

#optimizefeaturesubsetandfitfinalmodel 
progressr::with_progress(expr = {
  afs$train(task)
})


predictions <- predict(afs, newdata = df_10)
predictions

