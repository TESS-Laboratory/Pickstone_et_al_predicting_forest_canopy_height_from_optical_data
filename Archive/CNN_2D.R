library(terra)
library(tidyverse)
library(keras)
library(caret)
library(tensorflow)
library(caret)

set.seed(123)
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
# load in the data files ----------------------------------------------------
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 


sampled_tibble <- df_s2 %>%
  sample_frac(0.05)
#scale the data between zero and 1
process <- preProcess(as.data.frame(sampled_tibble), method=c("range"))

df_s2_norm <- predict(process, as.data.frame(sampled_tibble))
df_PS.10m_norm


process$ranges
# Remove the first two columns from the df_s2
df_s2 <- df_s2_norm[, -c(1, 2)]




# Extract input features (columns 1 to 27) and target variable (column 28)
X <- df_s2[, 1:29]
y <- df_s2[, 30]

# Convert input features to a 4-dimensional array (samples, height, width, channels)
X <- array(as.matrix(X), dim = c(nrow(df_s2), 10, 10, 29))

# Convert y to a matrix with one column
y <- matrix(y, ncol = 1)

# Set the index to split the data
split_index <- floor(0.8 * nrow(df_s2))  # 80% for training, 20% for testing

# Create train and test sets
X_train <- X[1:split_index, , , ]
X_test <- X[(split_index + 1):nrow(df_s2), , , ]


# Split the target variable into train and test sets
y_train <- head(y, split_index)
y_test <- tail(y, nrow(df_s2) - split_index)

# Convert X_train and X_test to numeric arrays
X_train <- as.array(X_train)
X_test <- as.array(X_test)



y_train <- unlist(y_train)
y_test <- unlist(y_test)


#set up the CNN
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,29)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l1(0.02)) %>% 
  layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,29)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_conv_2d(filters =64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(10,10,50)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression

# Compile the model with adam
model %>% 
  compile(
    loss = "mse",
    optimizer = "adam",
    metrics = "mean_absolute_error")


history <- model %>% fit(
  X_train, y_train,
  epochs = 10, 
  batch_size = 64, 
  validation_split = 0.1, 
  callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                        patience = 20))
)


c(loss, mae) %<-% (model %>% evaluate(X_test, y_test, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))

y_pred = model %>% predict(X_test)

process$ranges
# Minimum and maximum values of CHM (target variable)
chm_min <- 0.1052788
chm_max <- 39.3324928


# Undo the normalisation on the predicted values (y_pred)
y_pred_original <- (y_pred * (chm_max - chm_min)) + chm_min

# Undo the normalisation on y_test (target variable)
y_test_original <- (y_test * (chm_max - chm_min)) + chm_min

# Calculate R-squared
rsq <- cor(y_test, y_pred)^2
rsq
# Calculate MAE
mae <- mean(abs(y_test_original - y_pred_original))

# Calculate RMSE
rmse <- sqrt(mean((y_test_original - y_pred_original)^2))

# Print the results
print(paste0("R-squared: ", sprintf("%.2f", rsq)))
print(paste0("Mean Absolute Error: ", sprintf("%.2f", mae)))
print(paste0("Root Mean Squared Error: ", sprintf("%.2f", rmse)))

