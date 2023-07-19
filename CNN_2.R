library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3filters)
library(keras)
library(mlr3pipelines)
library(mlr3keras)
library(caret)
library(tensorflow)

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))
sampled_data <- df_10 %>% slice_sample(prop = 0.01)
set.seed(123)

# try again - this works ------------------------------------------------------

# attempt -----------------------------------------------------------------

# trying to turn it into a CNN --------------------------------------------
data.set <- as.matrix(df)
dimnames(data.set) = NULL
dim(data.set)
summary(data.set[, 29])

set.seed(123)
indx <- sample(2,
               nrow(data.set),
               replace = TRUE,
               prob = c(0.8, 0.2))

x_train <- data.set[indx == 1, 3:28]
x_test <- data.set[indx == 2, 3:28]
y_train <- data.set[indx == 1, 29]
y_test <- data.set[indx == 2, 29]

mean.train <- apply(x_train,
                    2,
                    mean)
sd.train <- apply(x_train,
                  2,
                  sd)
x_test <- scale(x_test,
                center = mean.train,
                scale = sd.train)
x_train <- scale(x_train)



# Reshape the data for CNN (1D Convolution)
x_train_cnn <- array_reshape(x_train, c(nrow(x_train), 26, 1))
x_test_cnn <- array_reshape(x_test, c(nrow(x_test), 26, 1))



# Define the CNN model
# Define the 1D CNN model
model <- keras_model_sequential() %>% 
  layer_conv_1d(filters = 96, kernel_size = 3, activation = "relu", input_shape = c(26, 1)) %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_conv_1d(filters = 96, kernel_size = 3, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>% 
  layer_dense(units = , activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(units = 1)


# from theh paper ---------------------------------------------------------

# Create the sequential model
model <- keras_model_sequential()

# Block 1
model %>%
  layer_conv_2d(filters = 96, kernel_size = c(3, 3), strides = c(1, 1), activation = "relu", input_shape = c(Your_Input_Shape)) %>%
  layer_dropout(rate = 0.2)

# Block 2
model %>%
  layer_conv_2d(filters = 96, kernel_size = c(3, 3), strides = c(1, 1), activation = "relu") %>%
  layer_dropout(rate = 0.2)

# Block 3
model %>%
  layer_conv_2d(filters = 96, kernel_size = c(3, 3), strides = c(2, 2), activation = "relu") %>%
  layer_dropout(rate = 0.2)

# Block 4
model %>%
  layer_conv_2d(filters = 192, kernel_size = c(3, 3), strides = c(1, 1), activation = "relu") %>%
  layer_dropout(rate = 0.2)

# Block 5
model %>%
  layer_conv_2d(filters = 192, kernel_size = c(3, 3), activation = "relu") %>%
  layer_dropout(rate = 0.2)

# Block 6
model %>%
  layer_conv_2d(filters = 192, kernel_size = c(3, 3), strides = c(2, 2), activation = "relu") %>%
  layer_dropout(rate = 0.2)

# Block 7
model %>%
  layer_flatten()

# Block 8
model %>%
  layer_dense(units = 128, activation = "relu")

# Block 9
model %>%
  layer_dense(units = 128, activation = "relu")

# Block 10
model %>%
  layer_dense(units = 2, activation = "linear")  # Use "linear" activation for regression tasks

# Compile the model
model %>% compile(
  loss = "mean_squared_error",  # Choose an appropriate loss function for your task
  optimizer = "adam",           # Choose an optimizer
  metrics = c("accuracy")       # Add more metrics if required
)

# Display model summary
model %>% summary()

model %>% summary()

# Compile the model
model %>% compile(loss = "mse",
                  optimizer = optimizer_adam(),
                  metrics = c("mean_absolute_error"))

# Train the model
history <- model %>% 
  fit(x_train_cnn,
      y_train,
      epochs = 50,
      batch_size = 32,
      validation_split = 0.1,
      callbacks = list(callback_early_stopping(monitor = "val_mean_absolute_error", patience = 5)),
      verbose = 2)

c(loss, mae) %<-% (model %>% evaluate(x_test_cnn, y_test, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))

# Fit the model to the training data
model %>% fit(x_train_cnn, y_train, epochs = 100, batch_size = 16, verbose = 0)

# Generate predictions on the test set
predictions <- model %>% predict(x_test_cnn)

# Calculate the R-squared using the mlr3 package
rsquared <- rsq(y_test, predictions)

# Print the R-squared
print(rsquared)


ypred = model %>% predict(x_test_cnn)


cat("RMSE:", RMSE(y_test, ypred))

x_axes = seq(1:length(ypred))
plot(x_axes, y_test, ylim = c(min(y_train), max(y_test)),
     col = "burlywood", type = "l", lwd = 2, ylab = "CHM (m)")
lines(x_axes, ypred, col = "red", type = "l", lwd = 2)
legend("topleft", legend = c("y-test", "y-pred"),
       col = c("burlywood", "red"), lty=1, cex=0.7, lwd=2, bty='n')


ggplot() +
  aes(x = ypred, y = y_test) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
  ) +
  geom_abline(slope = 1) +
  theme_light() +
  labs(x = "Predicted Canopy Height (m)", y = "Observed Canopy Height (m)")


