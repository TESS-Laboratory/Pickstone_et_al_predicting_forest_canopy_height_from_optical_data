library(terra)
library(tidyverse)
library(keras)
library(caret)
library(tensorflow)

set.seed(5443)
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
# load in the data files ----------------------------------------------------
#load in the cube packages (spatraster)
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))

#load in the dataframes 
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

#randomly select 300,000 rows to be used 
sampled_tibble <- df_PS.10m %>%
  sample_n(300000, replace = FALSE)

process <- preProcess(as.data.frame(sampled_tibble), method=c("range"))
process$ranges
sampled_tibble <- predict(process, as.data.frame(sampled_tibble))
data.set <- as.matrix(sampled_tibble)

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



# Reshape the data for CNN (1D Convolution)
x_train_cnn <- array_reshape(x_train, c(nrow(x_train), 26, 1))
x_test_cnn <- array_reshape(x_test, c(nrow(x_test), 26, 1))



# Define the CNN model
# Define the 1D CNN model
model <- keras_model_sequential() %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", 
                input_shape = c(26, 1)) %>% 
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu") %>% 
  layer_conv_1d(filters = 16, kernel_size = 3, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 2) %>% 
  layer_flatten() %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dropout(0.1) %>% 
  layer_dense(units = 1, activation = "linear")



# Compile the model with an appropriate optimizer and loss function
model %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mean_absolute_error"))

# Train the model
history <- model %>% fit(
  x_train_cnn, y_train,
  epochs = 10, # You can adjust the number of epochs
  batch_size = 32, # You can adjust the batch size
  validation_split = 0.2, 
  callbacks = c(callback_early_stopping(monitor = "val_mean_absolute_error",
                                        patience = 20))
)

c(loss, mae) %<-% (model %>% evaluate(x_test_cnn, y_test, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))

y_pred = model %>% predict(x_test_cnn)

# Minimum and maximum values of CHM (target variable)
chm_min <- 0.2346653
chm_max <- 37.9228210

# Undo the normalization on the predicted values (y_pred)
y_pred_original <- (y_pred * (chm_max - chm_min)) + chm_min

# Undo the normalization on y_test (target variable)
y_test_original <- (y_test * (chm_max - chm_min)) + chm_min

# Calculate R-squared (R^2)
rsq <- cor(y_test, y_pred)^2

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(y_test_original - y_pred_original))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((y_test_original - y_pred_original)^2))

# Print the results
print(paste0("R-squared: ", sprintf("%.2f", rsq)))
print(paste0("Mean Absolute Error: ", sprintf("%.2f", mae)))
print(paste0("Root Mean Squared Error: ", sprintf("%.2f", rmse)))
rsq
mae
rmse





# Evaluate the model on the test set
loss_and_metrics <- model %>% evaluate(x_test, y_test, verbose = 0)
cat("Test accuracy:", loss_and_metrics[2], "\n")

model %>% summary()


ypred = model %>% predict(x_test_cnn)

# Calculate the R-squared using the mlr3 package
rsquared <- R2(y_test, ypred)

# Print the R-squared
print(rsquared)


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


