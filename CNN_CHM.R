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
cnn_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = input_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes, activation = 'softmax')
# Create the learner

learner <- LearnerRegrKeras$new()
learner$param_set$values$model = model
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


