library(terra)
library(tidyverse)
library(keras)
library(caret)
library(tensorflow)


data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
# load in the data files ----------------------------------------------------
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

df_PS.10m

# Assuming your dataframe is named df_PS.10m
# Remove the first two columns from the dataframe
df_PS.10m <- df_PS.10m[, -c(1, 2)]
df_PS.10m

# Extract input features (columns 1 to 27) and target variable (column 28)
X <- df_PS.10m[, 1:26]
y <- df_PS.10m[, 27]

# Normalize columns
X[, 22:26] <- scale(X[, 22:26])

# Convert input features to a 4-dimensional array (samples, height, width, channels)
X <- array(as.matrix(X), dim = c(nrow(df_PS.10m), 20, 20, ncol(X)))


# Normalize the y variable
y <- scale(y)


# Convert y to a matrix with one column
y <- matrix(y, ncol = 1)

# Set the index to split the data
split_index <- floor(0.8 * nrow(df_PS.10m))  # 80% for training, 20% for testing

# Create train and test sets
X_train <- X[1:split_index, , , ]
X_test <- X[(split_index + 1):nrow(df_PS.10m), , , ]

# Split the target variable into train and test sets
# Split the target variable into train and test sets
y_train <- head(y, split_index)
y_test <- tail(y, nrow(df_PS.10m) - split_index)

# Convert X_train and X_test to numeric arrays
X_train <- as.array(X_train)
X_test <- as.array(X_test)


# Convert y_train and y_test to numeric arrays
# Convert y_train and y_test to numeric vectors
y_train <- unlist(y_train)
y_test <- unlist(y_test)


library(keras)

# Build the model
model <- keras_model_sequential()

# First convolutional layer
model %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = 'relu', input_shape = c(20, 20, 26)) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2)

# Second convolutional layer
model %>%
  layer_conv_2d(filters = 8, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_batch_normalization() %>%

# Third convolutional layer
model %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_batch_normalization() %>%

# Fourth (and last) convolutional layer
model %>%
  layer_conv_2d(filters = 1, kernel_size = c(3, 3), activation = 'relu') %>%
  layer_batch_normalization()

# Flatten the output for dense layers
model %>% 
  layer_flatten()

# Linear activation for regression
model %>% 
  layer_dense(units = 1, activation = 'linear')


#set up the CNN
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu', input_shape = c(30,30,26)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(30,30,26)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'linear')  # Use 'linear' activation for regression

#set up the CNN
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(30,30,26)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.2) %>% 
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
  epochs = 100, # You can adjust the number of epochs
  batch_size = 32, # You can adjust the batch size
  validation_split = 0.1, 
  callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                        patience = 20))
)

c(loss, mae) %<-% (model %>% evaluate(X_test, y_test, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))

ypred = model %>% predict(X_test)

# Calculate the R-squared using the mlr3 package
rsquared <- R2(y_test, ypred)

# Print the R-squared
print(rsquared)

cat("RMSE:", RMSE(y_test, ypred))
