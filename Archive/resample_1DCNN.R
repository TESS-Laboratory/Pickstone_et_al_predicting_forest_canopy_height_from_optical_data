library(mlr3)
library(mlr3spatiotempcv)
library(tibble)
library(terra)
library(tidyverse)
library(keras)
library(caret)
library(tensorflow)

set.seed(5443)

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
# load in the data files ----------------------------------------------------
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))

df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

#set number of folds
nfolds <- 11

sampled_tibble <- df_PS.10m %>%
  sample_frac(0.3)

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(sampled_tibble), method=c("range"))

df_PS.10m_norm <- predict(process, as.data.frame(sampled_tibble))
# this is a dataset provided in mlr3spatiotempcv
# for your example you will have to create a spatioptemp class
# here we also have to remove incomplete rows (not relevant to you)
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_PS.10m_norm,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(PScope_10m.cube)  
  )
)


resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# plot in 3d (requires {plotly} to be installed.) not essential but gives some context
# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
               fold = 1,
               crs = crs(PScope_10m.cube), point_size = 3, axis_label_fontsize = 10,
               plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)

# get the cooks farm data as a tibble (this shouldn't be necessary in your case)
#insert input of backend
#cooksfarm_tibble <- as_tibble(task_st$data()) |>
#  dplyr::select(dplyr::where(is.numeric)) # only numeric columns
data.set <- as.matrix(df_PS.10m_norm)

dimnames(data.set) = NULL
dim(data.set)
summary(data.set[, 29])

resampling_results <- purrr::map(1:nfolds, function(i) {
  # define the train and test ids for each fold
  train_ids <- resampling$train_set(i)
  train_ids <- sample(train_ids, round(length(train_ids)*0.1))
  train_df <- data.set[train_ids, ]
  test_ids <- resampling$test_set(i)
  test_df <- data.set[test_ids, ]

  
  x_train <- train_df[, 3:28]
  y_train <- train_df[, 29]
  
  # For the test_df
  x_test <- test_df[, 3:28]
  y_test <- test_df[, 29]
  
  # Reshape the data for CNN (1D Convolution)
  x_train_cnn <- array_reshape(x_train, c(nrow(x_train), 26, 1))
  x_test_cnn <- array_reshape(x_test, c(nrow(x_test), 26, 1))
  
  #insert the model 
  # Define the 1D CNN model
  model <- keras_model_sequential() %>% 
    layer_conv_1d(filters = 16, kernel_size = 3, activation = "relu", 
                  input_shape = c(26, 1)) %>% 
    layer_max_pooling_1d(pool_size = 2) %>% 
    layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu") %>% 
    layer_max_pooling_1d(pool_size = 2) %>% 
    layer_flatten() %>% 
    layer_dense(units = 32, activation = "relu") %>% 
    layer_dropout(0.1) %>% 
    layer_dense(units = 1, activation = "linear")
  
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam"
    loss = "mse",
    metrics = c("mean_absolute_error"))
  
  # Train the model
  history <- model %>% fit(
    x_train_cnn, y_train,
    epochs = 50, # You can adjust the number of epochs
    batch_size = 32, # You can adjust the batch size
    validation_split = 0.1, 
    callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                          patience = 20))
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_cnn),
    truth = y_test
  )
}) |>
  
  purrr::set_names(paste0("fold_", 1:nfolds))

# gives rmse for each fold
resampling_results

fold_rmse <- purrr::map(
  resampling_results,
  ~ mlr3measures::rmse(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

fold_rsq <- purrr::map(
  resampling_results,
  ~ mlr3measures::rsq(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

# macro aggregated rmse
dplyr::summarise(fold_rmse, agg_rmse = mean(value))

#macro aggregate rsq
dplyr::summarise(fold_rsq, agg_rsq = mean(value))

# micro aggregated rmse
dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rmse = mlr3measures::rmse(truth, response))

# micro aggregated rsq
dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rsq = mlr3measures::rsq(truth, response))
