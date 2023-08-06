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
comb_cube <- rast(file.path(data_path,"comb_cube.tif"))

df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

#set number of folds
nfolds <- 11

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_PS.3m), method=c("range"))

df_PS.3m_norm <- predict(process, as.data.frame(df_PS.3m))
df_PS.3m_norm
# this is a dataset provided in mlr3spatiotempcv
# for your example you will have to create a spatioptemp class
# here we also have to remove incomplete rows (not relevant to you)
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_PS.3m,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(PScope_3m.cube)  
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


# Extract input features (columns 1 to 27) and target variable (column 28)

data.set <- as.matrix(df_PS.3m_norm)
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
  
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 10, 10, 26))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 10, 10, 26))
  
  #insert the model 
  # Define the 2D CNN model
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,26)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression
  
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mean_absolute_error"))
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50, # You can adjust the number of epochs
    batch_size = 32, # You can adjust the batch size
    validation_split = 0.1, 
    callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                          patience = 15))
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_array),
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



# For Sentinel 2 Data -----------------------------------------------------
#set number of folds
nfolds <- 11

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_s2), method=c("range"))

df_s2_norm <- predict(process, as.data.frame(df_s2))
df_s2_norm
# this is a dataset provided in mlr3spatiotempcv
# for your example you will have to create a spatioptemp class
# here we also have to remove incomplete rows (not relevant to you)
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_s2_norm,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(s2.cube)  
  )
)


resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# plot in 3d (requires {plotly} to be installed.) not essential but gives some context
# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
                fold = 1,
                crs = crs(s2.cube), point_size = 3, axis_label_fontsize = 10,
                plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)


# Extract input features (columns 1 to 27) and target variable (column 28)

data.set <- as.matrix(df_s2_norm)
data.set
resampling_results <- purrr::map(1:nfolds, function(i) {
  # define the train and test ids for each fold
  train_ids <- resampling$train_set(i)
  train_ids <- sample(train_ids, round(length(train_ids)*0.1))
  train_df <- data.set[train_ids, ]
  test_ids <- resampling$test_set(i)
  test_df <- data.set[test_ids, ]
  
  x_train <- train_df[, 3:31]
  y_train <- train_df[, 32]
  
  # For the test_df
  x_test <- test_df[, 3:31]
  y_test <- test_df[, 32]
  
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 20, 20, 29))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 20, 20, 29))
  
  #insert the model 
  # Define the 2D CNN model
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,29)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression
  
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mean_absolute_error"))
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50, # You can adjust the number of epochs
    batch_size = 32, # You can adjust the batch size
    validation_split = 0.1, 
    callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                          patience = 10))
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_array),
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


# CNN for combined data ---------------------------------------------------
nfolds <- 11

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_comb), method=c("range"))

df_comb_norm <- predict(process, as.data.frame(df_comb))
df_comb_norm
# this is a dataset provided in mlr3spatiotempcv
# for your example you will have to create a spatioptemp class
# here we also have to remove incomplete rows (not relevant to you)
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_comb_norm,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(s2.cube)  
  )
)


resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# plot in 3d (requires {plotly} to be installed.) not essential but gives some context
# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
                fold = 1,
                crs = crs(s2.cube), point_size = 3, axis_label_fontsize = 10,
                plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)


# Extract input features (columns 1 to 27) and target variable (column 28)

data.set <- as.matrix(df_comb_norm)
resampling_results <- purrr::map(1:nfolds, function(i) {
  # define the train and test ids for each fold
  train_ids <- resampling$train_set(i)
  train_ids <- sample(train_ids, round(length(train_ids)*0.1))
  train_df <- data.set[train_ids, ]
  test_ids <- resampling$test_set(i)
  test_df <- data.set[test_ids, ]
  
  x_train <- train_df[, 3:52]
  y_train <- train_df[, 53]
  
  # For the test_df
  x_test <- test_df[, 3:52]
  y_test <- test_df[, 53]
  
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 10, 10, 50))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 10, 10, 50))
  
  #insert the model 
  # Define the 2D CNN model
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,50)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression
  
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mean_absolute_error"))
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50, # You can adjust the number of epochs
    batch_size = 32, # You can adjust the batch size
    validation_split = 0.1, 
    callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                          patience = 10))
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_array),
    truth = y_test
  )
}) |>
  
  purrr::set_names(paste0("fold_", 1:nfolds))

# gives rmse for each fold
resampling_results
process$ranges

# Minimum and maximum values of CHM (target variable)
chm_min <- 0.007294444
chm_max <- 47.593669891


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

fold_mae <- purrr::map(
  resampling_results,
  ~ mlr3measures::mae(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

# macro aggregated rmse
macro_rmse <- dplyr::summarise(fold_rmse, agg_rmse = mean(value))
original_rmse <-  macro_rmse * (chm_max - chm_min)
original_rmse
#macro aggregate rsq
macro_rsq <- dplyr::summarise(fold_rsq, agg_rsq = mean(value))

macro_mae <- dplyr::summarise(fold_mae, agg_mae = mean(value))

original_mae <-  macro_mae * (chm_max - chm_min)


# micro aggregated rmse
micro_rmse <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rmse = mlr3measures::rmse(truth, response))
original_rmse_micro <-  micro_rmse * (chm_max - chm_min)
original_rmse_micro
# micro aggregated rsq
micro_rsq <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rsq = mlr3measures::rsq(truth, response))

micro_mae <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_mae = mlr3measures::mae(truth, response))
original_mae_micro <-  micro_mae * (chm_max - chm_min)

original_mae_micro

micro_rsq

#visualise the results
library(ggplot2)

# Extract the truth and response values from each fold's result
# Combine the results of all folds into a single data frame

results_df$truth_renorm <- results_df$truth * (chm_max - chm_min) + chm_min
results_df$response_renorm <- results_df$response * (chm_max - chm_min) + chm_min
results_df <- bind_rows(resampling_results)
results_df
write.csv(results_df, "/raid/home/bp424/Documents/MTHM603/Data/CNN_3m.csv", row.names=FALSE)



ggplot(results_df) +
  aes(x = truth_renorm, y = response_renorm) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
    breaks = c(0,10, 1000,10000, 80000), 
    guide = guide_colorbar(barwidth = .5, barheight = 2)) +
  geom_abline(slope = 1, linewidth = 0.2) +
  theme_light() +
  labs(x = "LiDAR Canopy Height (m)", y = "Predicted Canopy Height (m)", 
       fill = "Number of Observations")+ 
  theme(axis.text = element_text(size = 4, family = "Times New Roman"),
        axis.title = element_text(size = 4, family = "Times New Roman"),
        legend.text = element_text(size = 3, family = "Times New Roman"),
        legend.title = element_text(size = 4, family = "Times New Roman"))

ggsave(file="CNN.3m.plots.png", dpi = 600)
