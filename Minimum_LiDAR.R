
# load in required packages -----------------------------------------------

library(mlr3)
library(mlr3spatiotempcv)
library(raster)
library(sf)
library(ggplot2)
library(terra)
library(dplyr)


set.seed(1234)
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube_10 <- rast(file.path(data_path,"comb_cube_10m.tif"))


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
    crs = terra::crs(cube_10)
  )
)

# Define your model
learner <- lrn("regr.lm")

resampling <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

measure <- msr("regr.rsq")

autoplot(resample, task = task, fold_id = 1:3)

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

model <- afs$train(task)

subset_rest <- df_10 %>%
  slice(start_row:(start_row + num_rows_subset - 1))

rest_predictions <- predict(model, newdata = subset_rest)

library(Metrics)
rmse <- rmse(rest_predictions, subset_rest$CHM)
r_squared <- cor(rest_predictions, subset_rest$CHM)^2
mae <- mae(rest_predictions, subset_rest$CHM)

rmse
mae
r_squared
rest_predictions


predictions <- predict(afs, newdata = df_10)
predictions

