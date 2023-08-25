##This script has been written for completing CNN on all 
##four different datasources 
##as MLR3 is yet to develop CNN for regression tasks, the following 
#code includes a resampling strategy defined within mlr3 and then the model completed
#in tensor flow and keras 

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


# PlanetScope 3m and 10 m ----------------------------------------------------------

#set number of folds
nfolds <- 10

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_PS.10m), method=c("range"))

df_PS10_norm <- predict(process, as.data.frame(df_PS.10m))


task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_PS10_norm,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(PScope_10m.cube)  
  )
)

resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
                fold = 1,
                crs = crs(PScope_3m.cube), point_size = 3, axis_label_fontsize = 10,
                plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)


# Extract input features (columns 1 to 27) and target variable (column 28)
data.set <- as.matrix(df_PS10_norm)
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
  
  #turn into an array for the CNN
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 10, 10, 26))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 10, 10, 26))
  
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,26)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam",  # Adjust learning rate
    loss = "mean_squared_error",  
    metrics = c("mean_absolute_error", "mean_squared_error")
  )
  
  summary(model)
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50,  # Increase the number of epochs
    batch_size = 64,
    validation_split = 0.1,
    callbacks = list(
      callback_early_stopping(monitor = "val_mean_absolute_error", patience = 15)
    )
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_array),
    truth = y_test
  )
  
}) |>
  
  purrr::set_names(paste0("fold_", 1:nfolds))

# Minimum and maximum values of CHM (target variable) - to turn back from 
#normalised data
process$ranges

#for 3 m resolution
chm_min <- 0.007294444
chm_max <- 47.593669891

#for 10 m resolution 
chm_min <- 0.1052788
chm_max <- 41.3926506

# gives rmse for each fold
resampling_results

#calculate RMSE
fold_rmse <- purrr::map(
  resampling_results,
  ~ mlr3measures::rmse(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

#Calculate RSQ
fold_rsq <- purrr::map(
  resampling_results,
  ~ mlr3measures::rsq(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

#Calculate MAE
fold_mae <- purrr::map(
  resampling_results,
  ~ mlr3measures::mae(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

# macro aggregated rmse
macro_rmse <- dplyr::summarise(fold_rmse, agg_rmse = mean(value))
original_rmse <-  macro_rmse * (chm_max - chm_min)

#macro aggregate rsq
macro_rsq <- dplyr::summarise(fold_rsq, agg_rsq = mean(value))

#calculate MAE
macro_mae <- dplyr::summarise(fold_mae, agg_mae = mean(value))
original_mae <-  macro_mae * (chm_max - chm_min)

# micro aggregated rmse
micro_rmse <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rmse = mlr3measures::rmse(truth, response))
original_rmse_micro <-  micro_rmse * (chm_max - chm_min)

# micro aggregated rsq
micro_rsq <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rsq = mlr3measures::rsq(truth, response))

# micro aggregated mae
micro_mae <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_mae = mlr3measures::mae(truth, response))
original_mae_micro <-  micro_mae * (chm_max - chm_min)

# Extract the truth and response values from each fold's result
# Combine the results of all folds into a single data frame
results_df <- bind_rows(resampling_results)
results_df$truth_renorm <- results_df$truth * (chm_max - chm_min) + chm_min
results_df$response_renorm <- results_df$response * (chm_max - chm_min) + chm_min

#export to create plots
write.csv(results_df, "/raid/home/bp424/Documents/MTHM603/Data/CNN_PS10.csv", row.names=FALSE)

#test on full set
X_all <- df_PS3_norm[, 3:28]
x_array_all <- array(as.matrix(X_all), dim = c(nrow(X_all), 10, 10, 26))

y_all <- df_PS3_norm[, 29]

response_all = model %>% predict(x_array_all)
response_all_original <- response_all * (chm_max - chm_min) + chm_min

x_and_y <- df_PS.3m[, 1:2]
PS10_df_CNN <- cbind(x_and_y, response_all_original)

#turn to spatraster for plotting 
PS10_cube <- as_spatraster(PS10_df_CNN, crs = crs(PScope_10m.cube))

#export for plotting later
writeRaster(PS10_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/CNN_predict_cube.tif", 
            overwrite = TRUE)

# For Sentinel 2 Data -----------------------------------------------------
#set number of folds
nfolds <- 10

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_s2), method=c("range"))

df_s2_norm <- predict(process, as.data.frame(df_s2))

# this is a dataset provided in mlr3spatiotempcv
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_s2_norm,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(s2.cube)  
  )
)

resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# plot in 3d 
# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
                fold = 1,
                crs = crs(s2.cube), point_size = 3, axis_label_fontsize = 10,
                plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)

data.set <- as.matrix(df_s2_norm)

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
  
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 10, 10, 29))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 10, 10, 29))

  
  # Print a summary of the model architecture
  summary(model)
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50,  # Increase the number of epochs
    batch_size = 64,
    validation_split = 0.1,
    callbacks = list(
      callback_early_stopping(monitor = "val_mean_absolute_error", patience = 15)
    )
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
  ~ mlr3measures::rmse(y_all, predictions_all)
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
dplyr::summarise(fold_rmse, agg_rmse = mean(value))

#macro aggregate rsq
dplyr::summarise(fold_rsq, agg_rsq = mean(value))

#macro aggregate mae
dplyr::summarise(fold_mae, agg_mae = mean(value))

# micro aggregated rmse
dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rmse = mlr3measures::rmse(truth, response))

# micro aggregated mae
dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_mae = mlr3measures::mae(truth, response))

# micro aggregated rsq
dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rsq = mlr3measures::rsq(truth, response))

# Extract the truth and response values from each fold's result
# Combine the results of all folds into a single data frame
results_df <- bind_rows(resampling_results)
results_df$truth_renorm <- results_df$truth * (chm_max - chm_min) + chm_min
results_df$response_renorm <- results_df$response * (chm_max - chm_min) + chm_min
results_df
write.csv(results_df, "/raid/home/bp424/Documents/MTHM603/Data/CNN_S2.csv", row.names=FALSE)

#to test on the full set 
X_all <- df_s2_norm[, 3:31]
x_array_all <- array(as.matrix(X_all), dim = c(nrow(X_all), 10, 10, 29))

y_all <- df_s2[, 32]

y_all
response_all = model %>% predict(x_array_all)
response_all_original <- response_all * (chm_max - chm_min) + chm_min

x_and_y <- df_s2[, 1:2]
S2_df_CNN <- cbind(x_and_y, response_all_original)

S2_CNN_cube <- as_spatraster(S2_df_CNN, crs = crs(s2.cube))
S2_CNN_cube
plot(S2_CNN_cube$response_all_original)

writeRaster(S2_CNN_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/S2_CNN_predict.tif", 
            overwrite = TRUE)


# CNN for combined data ---------------------------------------------------
nfolds <- 10

#normalise the data from 0 to 1 
process <- preProcess(as.data.frame(df_comb), method=c("range"))

df_comb_norm <- predict(process, as.data.frame(df_comb))

# this is a dataset provided in mlr3spatiotempcv
task_st <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_comb,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(comb_cube)  
  )
)

resampling <- mlr3::rsmp("repeated_spcv_coords", folds = nfolds, repeats = 2)

# plot in 3d 
# shows fold 1 only.
(pl <- autoplot(resampling, task_st,
                fold = 1,
                crs = crs(s2.cube), point_size = 3, axis_label_fontsize = 10,
                plot3D = FALSE))


# instantiate the resampling object using the spatial task.
resampling$instantiate(task_st)


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
  
  #to test on the full set 
  X_all <- df_comb_norm[, 3:52]
  x_array_all <- array(as.matrix(X_all), dim = c(nrow(X_all), 10, 10, 50))
  
  y_all <- df_comb_norm[, 53]
  
  x_train_array <- array(as.matrix(x_train), dim = c(nrow(x_train), 10, 10, 50))
  x_test_array <- array(as.matrix(x_test), dim = c(nrow(x_test), 10, 10, 50))
  
  #insert the model 
  # Define the 2D CNN model
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,50)) %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.1) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression
  
  
  # Compile the model with an appropriate optimizer and loss function
  model %>% compile(
    optimizer = "adam",  # Adjust learning rate
    loss = "mean_squared_error",  
    metrics = c("mean_absolute_error", "mean_squared_error")
  )
  
  # Print a summary of the model architecture
  summary(model)
  
  # Train the model
  history <- model %>% fit(
    x_train_array, y_train,
    epochs = 50,  # Increase the number of epochs
    batch_size = 64,
    validation_split = 0.1,
    callbacks = list(
      callback_early_stopping(monitor = "val_mean_absolute_error", patience = 15)
    )
  )
  
  # predict on the test set
  tb <- tibble(
    response = model %>% predict(x_test_array),
    truth = y_test
  )
  
  #predict the whole region
  tb_all <- tibble(
    response = model %>% predict(x_array_all),
    truth = y_all
  )
  
}) |>
  
  purrr::set_names(paste0("fold_", 1:nfolds))

# gives rmse for each fold
resampling_results
process$ranges

# Minimum and maximum values of CHM (target variable)
chm_min <- 0.1052788
chm_max <- 41.3926506

#calculate RMSE
fold_rmse <- purrr::map(
  resampling_results,
  ~ mlr3measures::rmse(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

#Calculate RSQ
fold_rsq <- purrr::map(
  resampling_results,
  ~ mlr3measures::rsq(.x$truth_all, .x$response_all)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

#Calculate MAE
fold_mae <- purrr::map(
  resampling_results,
  ~ mlr3measures::mae(.x$truth, .x$response)
) |>
  tibble::enframe() |>
  tidyr::unnest(value)

# macro aggregated rmse
macro_rmse <- dplyr::summarise(fold_rmse, agg_rmse = mean(value))
original_rmse <-  macro_rmse * (chm_max - chm_min)

#macro aggregate rsq
macro_rsq <- dplyr::summarise(fold_rsq, agg_rsq = mean(value))

#calculate MAE
macro_mae <- dplyr::summarise(fold_mae, agg_mae = mean(value))
original_mae <-  macro_mae * (chm_max - chm_min)

# micro aggregated rmse
micro_rmse <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rmse = mlr3measures::rmse(truth, response))
original_rmse_micro <-  micro_rmse * (chm_max - chm_min)
original_rmse_micro
# micro aggregated rsq
micro_rsq <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_rsq = mlr3measures::rsq(truth_all, response_all))
micro_rsq
# micro aggregated mae
micro_mae <- dplyr::bind_rows(resampling_results) |>
  dplyr::summarise(agg_mae = mlr3measures::mae(truth, response))
original_mae_micro <-  micro_mae * (chm_max - chm_min)

# Extract the truth and response values from each fold's result
# Combine the results of all folds into a single data frame
results_df <- bind_rows(resampling_results)
results_df$truth_renorm <- results_df$truth * (chm_max - chm_min) + chm_min
results_df$response_renorm <- results_df$response * (chm_max - chm_min) + chm_min

write.csv(results_df, "/raid/home/bp424/Documents/MTHM603/Data/CNN_comb.csv", row.names=FALSE)

ggplot(results_df) +
  aes(x = truth_renorm, y = response_renorm) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
    breaks = seq(0, 3000, length.out = 3)  # Adjust the range and number of breaks as needed
  ) +
  geom_abline(slope = 1, linewidth = 0.2) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
    axis.text = element_text(size = 10),              
    axis.title = element_text(size = 10),             
    legend.text = element_text(size = 8),            
    legend.title = element_text(size = 10)            
  ) +
  labs(x = "LiDAR Canopy Height (m)", 
       y = "Predicted Canopy Height (m)", 
       fill = "Count")+ 
  scale_x_continuous(breaks = seq(0, 40, by = 10)) +  # Set x-axis tick marks to go up in 10s
  scale_y_continuous(breaks = seq(-10, 40, by = 10), limits = c(-10, 40))

ggsave(file="CNN.comb.plots.png", dpi = 600)

