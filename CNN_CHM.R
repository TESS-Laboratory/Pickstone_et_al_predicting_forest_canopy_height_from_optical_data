#load in packages 
library(mlr3verse)
library(mlr3)
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
lrn = LearnerClassifKerasFF$new()

#set seed 
set.seed(1234)

lrn = LearnerClassifKerasFF$new()
model <- keras_model_sequential()

py_config()data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
df <- read_csv(file.path(data_path, "final_df.csv"))
# convert to dataframe ----------------------------------------------------

# Create the CNN ----------------------------------------------------------
# Split the dataframe into feature set and target variable
features <- df[, !(names(df) %in% c("x", "y", "CHM"))]
target <- df$CHM

features_array <- array(features)


# Define your CNN model using Keras
model <- keras_model_sequential()


# Compile the model with appropriate loss function and optimizer
model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam"
)

# Train the model
model %>% fit(
  input_train, output_train,
  epochs = 10,
  batch_size = 32
)