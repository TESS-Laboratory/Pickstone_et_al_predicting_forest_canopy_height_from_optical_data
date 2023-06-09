#load in packages 
library(mlr3verse)
library(terra)
library(dplyr)
library(tidyverse)
library(ggpmisc)
library(keras)


#set seed 
set.seed(1234)


# set working directory ---------------------------------------------------
setwd("~/OneDrive - University of Exeter/University/Dissertation/Data")

# Read LiDAR dtm and dsm file 
dtm <- rast("katingan_DEMS/katingan_DTM.tif")
dsm <- rast("katingan_DEMS/katingan_DSM.tif")


# convert to dataframe ----------------------------------------------------

data <- terra::as.data.frame(dtm, xy = T, na.rm = T)%>%
  tibble()

# Create the CNN ----------------------------------------------------------

# Define the input shape for the CNN
input_shape <- c( , , )  #change this depending on the input shape of the data 

# Preprocess the data and create feature and target matrices
X <- as.matrix(data[, c("aspect", "slope", "DTM", )])  # Feature matrix
Y <- as.matrix(data[,"CHM"])  # Target matrix

# Split the data into training and testing sets
train_ratio <- 0.8
train_size <- round(nrow(data) * train_ratio)

X_train <- X[1:train_size, , drop = FALSE]
Y_train <- Y[1:train_size, , drop = FALSE]
X_test <- X[(train_size+1):nrow(data), , drop = FALSE]
Y_test <- Y[(train_size+1):nrow(data), , drop = FALSE]

# Reshape the feature matrices to the desired input shape
X_train <- array_reshape(X_train, dim = c(train_size, input_shape[1], input_shape[2], input_shape[3]))
X_test <- array_reshape(X_test, dim = c(nrow(data) - train_size, input_shape[1], input_shape[2], input_shape[3]))

# Define the custom task for canopy height prediction
TaskCanopyHeight <- TaskRegr$new(id = "katingan_", 
                                 backend = data, 
                                 target = "CHM", 
                                 coordinate_names = c("x", "y"), 
                                 extra_args = list( 
                                   coords_as_features = FALSE, 
                                   crs = crs(INSERT_THE_TIFF_FILE_HERE)))

# Define the learner with keras
learning_rate <- 0.001
epochs <- 100
iterations <- 5

learner_cnn <- lrn("regr.keras", layers = list(
  layer_conv_2d(filter = 32, kernel_size = c(3, 3), activation = "relu", input_shape = input_shape),
  layer_max_pooling_2d(pool_size = c(2, 2)),
  layer_flatten(),
  layer_dense(units = 64, activation = "relu"),
  layer_dense(units = 1)
), optimizer = optimizer_adam(lr = learning_rate))

# Create the mlr3 machine learning pipeline
pipeline <- mlr_pipe(steps = list(
  step_normalize(),
  step_select(selector = selector("importance"), algorithm = importance(model = learner_cnn)),
  step_model(learner = learner_cnn)
))

# Train the model
pipeline$train(TaskCanopyHeight, learner = learner_cnn, control = ctrl_train(
  epochs = epochs,
  iter = iterations
))

# Predict on the test set
predictions <- pipeline$predict_newdata(data.frame(X = X_test))


# tune the model ----------------------------------------------------------

# Define the autotuner
autotuner <- AutoTuner$new(
  learner = learner_cnn,
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.mse"),
  search_space = ps(),
  terminator = trm("evals", n_evals = 10))

# Calculate the Mean Squared Error
mse <- mean((Y_test - predictions)^2)
print(mse)

# Calculate the R-squared value
r_squared <- rsq(Y_test, predictions)
print(r_squared)