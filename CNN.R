#load in packages 
library(mlr3verse)
library(mlr3)
library(mlr3keras)
library(keras)
library(tensorflow)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(terra)
library(dplyr)
library(tidyverse)
library(ggpmisc)
library(keras)
library(tensorflow)
library(caret)
remotes::install_github("mlr-org/mlr3keras", force = T)

reticulate::install_miniconda(force = TRUE)
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
  backend = sampled_data,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)
model <- keras_model_sequential()

architecture <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = 26) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)


learner <- mlr_learners$get("regr.keras", model = architecture)
resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)
measure <- msr("regr.rsq")

benchmark <- benchmark_grid(
  task = task,
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

num_classes <- 10  # Replace 10 with the actual number of classes in your problem

dim(x_train)
# Reshape x_train to 3D format (height, width, channels)
x_train_3d <- array_reshape(x_train, c(nrow(x_train), 28, 1))

dim(x_train_3d)



# Create a sequential model
cnn_model <- keras_model_sequential()

# Add 1D Convolution layers
cnn_model %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(28, 1)) %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = 'relu') %>% 
  layer_max_pooling_1d(pool_size = 2)

# Continue with the rest of your model definition
cnn_model %>%
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# Compile the model
cnn_model %>% compile(
  loss = "mse", # Use "mse" for regression tasks
  optimizer = optimizer_adam(),
  metrics = c("accuracy") # Use "mean_absolute_error" for regression tasks
)

# Train the model
history <- cnn_model %>% fit(
  x_train_3d, y_train,
  epochs = 50, # Specify the number of training epochs
  batch_size = 32, # Set the batch size for training
  validation_split = 0.1, # Optionally, use a validation split for monitoring validation loss/metrics
  verbose = 1 # Set to 1 for progress updates during training or 0 for silent mode
)

# Assuming you have preprocessed x_test_3d and y_test
evaluation <- cnn_model %>% evaluate(x_test_3d, y_test)

learner <- LearnerRegrKeras$new()
learner$param_set$values$model = cnn_model
learner$train(task)



# another attempt ---------------------------------------------------------

set.seed(123)

indexes = createDataPartition(boston$medv, p = .85, list = F)

train = boston[indexes,]
test = boston[-indexes,]

xtrain = as.matrix(train[,-14])
ytrain = as.matrix(train[,14])
xtest = as.matrix(test[,-14])
ytest = as.matrix(test[, 14])

#Next, we'll reshape the x input data by adding another one-dimension.

xtrain = array(xtrain, dim = c(nrow(xtrain), 13, 1))
xtest = array(xtest, dim = c(nrow(xtest), 13, 1))

dim(xtrain)
dim(xtest)

#Here, we can extract the input dimension for the keras model.

in_dim = c(dim(xtrain)[2:3])
print(in_dim)

model = keras_model_sequential() %>%
  layer_conv_1d(filters = 64, kernel_size = 2,
                input_shape = in_dim, activation = "relu") %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

model %>% compile(
  loss = "mse",
  optimizer = "adam")

model %>% summary()

#Next, we 'll fit the model with train data.

model %>% fit(xtrain, ytrain, epochs = 100, batch_size=16, verbose = 0)
scores = model %>% evaluate(xtrain, ytrain, verbose = 0)
print(scores)

#Now we can predict the test data with the trained model.

ypred = model %>% predict(xtest)

#We'll check the accuracy of prediction through the RMSE metrics.

cat("RMSE:", RMSE(ytest, ypred))


