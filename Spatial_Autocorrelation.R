
# load in packages --------------------------------------------------------
library(geoR)
library(gstat)
library(terra)
library(sf)
library(spdep)
library(sp)
library(tidyverse)

# load in required datafiles ----------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))
CHM <- dsm - dtm
names(CHM) <- "CHM"

# Pre-process CHM  ------------------------------------------------
# Change the resolution to 100x100 meters
CHM_100 <- aggregate(CHM, fact = 100)

# Convert the CHM raster to a spatial points data frame
CHM_points <- as.data.frame(CHM_100, xy = TRUE)
coordinates(CHM_points) <- ~x + y

# Create Variogram --------------------------------------------------------

vgm1 <- variogram(log(CHM)~1, CHM_points)
lzn.fit=fit.variogram(vgm1,model=vgm(1,"Exp",3000,1))


# Plot the variogram to visualize the autocorrelation structure
file_name <- "variogram.png"
dpi <- 600

# Create the PNG device with high resolution
png(file = file_name, width = 8, height = 6, units = "in", res = dpi)

plot(vgm1,lzn.fit, 
     xlab = "Distance (m)", ylab = "Semi-variance")

dev.off()

# Check spatial correlation using Moran's I -------------------------------

spcor <- terra::autocor(CHM_100, method="moran", global=TRUE)



# check for the appropriate amount of resamples ---------------------------
library(mlr3)
library(mlr3learners)

library(mlr3)
library(mlr3learners)

# Define the range of fold values to test
fold_values <- c(3, 4, 6, 10, 15)

# Create a task (replace 'your_task' with your actual task)
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df_10,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

spcv_plan_3 <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats=1)
spcv_plan_4 <- mlr3::rsmp("repeated_spcv_coords", folds = 4, repeats=1)
spcv_plan_5 <- mlr3::rsmp("repeated_spcv_coords", folds =6, repeats=1)
spcv_plan_6 <- mlr3::rsmp("repeated_spcv_coords", folds = 15, repeats=1)


learner <- lrn("regr.lm")

design = benchmark_grid(task, learner, list(spcv_plan_3,spcv_plan_4, spcv_plan_5,
                                            spcv_plan_6))
# print(design)
future::plan("multisession", workers = 10) # sets the number of cores to run on -  we have
bmr = mlr3::benchmark(design)

aggr = bmr$aggregate(measures = c(msr("regr.rmse"), msr("regr.mse"), msr("regr.rsq")))

gt::gt(aggr[,4:9,])


result <- bmr$aggregate(msr("regr.rmse"))
results[[as.character(folds)]] <- result

result
results_df <- data.table::rbindlist(results)
results_df
# Print the results
print(results_df)

# Plot the results to visualize the performance across different fold values
library(ggplot2)
ggplot(results_df, aes(x = iters, y = regr.rmse)) +
  geom_line() +
  geom_point() +
  xlab("Number of Folds") +
  ylab("Root Mean Squared Error (RMSE)")


# testing for other resampling strategies  --------------------------------
block_cv <- rsmp("spcv_block", folds = 3, range = 3000)




# Visualise the resampling strategy  --------------------------------------


# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df_10,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

#create the resampling strategy for the train and test set 
resample <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

# Create the autoplot with custom font and tilted x-axis labels
autoplot(resample, task = task, fold_id = 1:1) +
  theme(
    text = element_text(size = 7, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 2:2) +
  theme(
    text = element_text(size = 7,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 3:3) +
  theme(
    text = element_text(size = 7,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file="Fold_3.png", dpi = 600)

# resample_lm ------------------------------------------------------------

resample_lm <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task,
    learner = afs$learner,
    resampling = resample, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

# evaluate ----------------------------------------------------------------

metric_scores <- resample_lm$aggregate(measure = c(
  mlr3::msr("regr.bias"),
  mlr3::msr("regr.rmse"),
  mlr3::msr("regr.rsq"),
  mlr3::msr("regr.mae")))

metric_scores

# visualise ---------------------------------------------------------------

resample_lm$prediction() %>%
  ggplot() +
  aes(x = response, y = truth) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
    breaks = c(0, 5000, 20000, 50000)
  ) +
  geom_abline(slope = 1) +
  theme_light() +
  labs(x = "Predicted Canopy Height (m)", y = "Observed Canopy Height (m)")




