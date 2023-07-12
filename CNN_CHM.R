#load in packages 
library(mlr3verse)
library(mlr3)
library(mlr3keras)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(terra)
library(dplyr)
library(tidyverse)
library(ggpmisc)

remotes::install_github("mlr-org/mlr3keras")
reticulate::install_miniconda()
reticulate::conda_create(
  envname = "mlr3keras",
  packages = c("pandas", "python=3.8")
)
keras::install_keras("conda", tensorflow="2.3.1", envname="mlr3keras")

reticulate::use_condaenv("mlr3keras")
library(mlr3keras)

#set seed 
set.seed(1234)


data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))


# Create the CNN ----------------------------------------------------------
# Split the dataframe into feature set and target variable
features <- df[, !(names(df) %in% c("x", "y", "CHM"))]
target <- df$CHM

features_array <- array(features)

# Define your CNN model using Keras

# define the task ---------------------------------------------------------

# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)
architecture <- mlr3keras::keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = 27) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  


learner <- mlr_learners$get("regr.keras", model = architecture)
resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)
measure <- msr("regr.rsq")

benchmark <- benchmark_grid(
  tasks = task,
  learners = learner,
  resampling = resampling,
  measures = measure
)
results <- benchmark$aggregate()


LearnerRegrKeras$new()
mlr3::mlr_learners$get("regr.keras")
mlr3::lrn("regr.keras")

# Set Learner Hyperparams
lrn$param_set$values$epochs = 50
lrn$param_set$values$layer_units = 12


# an alternative option  --------------------------------------------------

library(keras)
model = keras_model_sequential() %>%
  layer_dense(units = 12L, input_shape = 10L, activation = "relu") %>%
  layer_dense(units = 12L, activation = "relu") %>%
  layer_dense(units = 1L, activation = "linear") %>%
  compile(optimizer = optimizer_sgd(),
          loss = "mean_squared_error",
          metrics = "mean_squared_error")
# Create the learner

learner <- LearnerRegrKeras$new()
learner$param_set$values$model = model
learner$train(task)

