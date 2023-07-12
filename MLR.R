
# load in packages --------------------------------------------------------
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3filters)
library(mlr3fselect)
library(mlr3learners)
library(mlr3mbo)
library(tidyverse)
library(dplyr)
library(terra)
library(ggpmisc)
library(lightgbm)
library(mlr3viz)
library(GGally)
library(gt)
library(blockCV)

# set seed ----------------------------------------------------------------

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))


# generate a task --------------------------------------------------------


# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  
  )
)

library(mlr3)
library(mlr3learners)
library(mlr3spatiotempcv)

# Define your model (replace 'regr.lm' with the appropriate learner)
learner <- lrn("regr.lm")

# Create a spatiotemporal task using mlr3spatiotempcv (replace with your appropriate data and settings)
task <- TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df,  # Replace 'df' with your appropriate data
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  # Replace 'cube' with your appropriate spatial data object
  )
)

# Define a range of fold values to test
fold_values <- c(2, 3, 4, 5, 6, 7, 10)

# Perform spatiotemporal cross-validation with different fold values
for (fold in fold_values) {
  # Define a spatiotemporal resampling strategy with the current fold value
  resampling <- mlr3::rsmp("spcv_coords", fold)
  
  # Perform spatiotemporal cross-validation and compute performance measures
  res <- resample(task, learner, resampling)
  
  # Extract and print the average performance measure
  performance <- mean(res$aggregate(msr("regr.rsq")))
  
  cat("Number of Folds:", fold, "Performance:", performance, "\n")
}


# Define the learner and resampling plan

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)
measure <- msr("regr.rsq")

# Define the regr.lm learner
learner <- lrn("regr.lm")

#createautofselector 
afs=auto_fselector( 
  fselector=fs("random_search"), 
  learner=learner, 
  resampling=resample, 
  measure=measure, 
  term_evals=10)
  #subset_size = 0.3)) 

#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})

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
  mlr3::msr("regr.mse")))


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