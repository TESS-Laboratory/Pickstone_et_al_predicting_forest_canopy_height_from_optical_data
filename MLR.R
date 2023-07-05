
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
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))

# find optimal number of clusters -----------------------------------------
# Exclude x and y from the dataframe
data <- df[, !(colnames(df) %in% c("x", "y"))]

# Define the range of cluster numbers to evaluate
k_values <- 2:7

# Perform k-means clustering for each k value and calculate total within-cluster sum of squares (wss)
wss <- sapply(k_values, function(k) {
  kmeans(data, centers = k)$tot.withinss
})

# Plot the elbow curve
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares (wss)")

df

# generate a task ---------------------------------------------------------
task = mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM", 
  backend= df,
  target = "CHM",
  coordinate_names= c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs= terra::crs(kat_planet))
)


# define a resampling plan ------------------------------------------------
# Define the spcv plans
# Define the spcv plans
spcv_plan1 <- mlr3::rsmp("cv", folds = 10)
spcv_plan2 <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)
spcv_plan3 <- mlr3::rsmp("spcv_coords", folds = 3)
spcv_plan4 <- mlr3::rsmp("spcv_disc", folds = 3, radius = 50)
spcv_plan4 <- mlr3::rsmp("spcv_block", folds = 3, range = 100)




# Create an empty list to store the benchmark results
bmr_list <- list()

# Iterate over each spcv plan
for (spcv_plan in list(spcv_plan1, spcv_plan2, spcv_plan3, spcv_plan4, spcv_plan5)) {
  # Create the benchmark grid with the spcv plan and regr.lm learner
  design <- benchmark_grid(task, lrn("regr.lm"), spcv_plan)
  
  # Run the benchmark
  bmr <- benchmark(design)
  
  # Store the benchmark result in the list
  bmr_list[[length(bmr_list) + 1]] <- bmr
}

# Aggregate the results for each spcv plan
aggr_list <- lapply(bmr_list, function(bmr) bmr$aggregate(measures = c(msr("regr.rmse"), msr("regr.mse"), msr("regr.rsq"))))

# Display the results using the gt package for each spcv plan
for (i in seq_along(aggr_list)) {
  aggr <- aggr_list[[i]]
  cat("Results for SPCV Plan", i, ":\n")
  gt(aggr[, 4:9])
  cat("\n")
}


# complete multiple linear regression -------------------------------------

# Define the task
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df,
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(kat_planet))
)

# Define the learner

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

#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})


library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(progressr)

# Define the task
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df,
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(kat_planet))
)

# Define the learner
learner <- lrn("regr.lm")

# Define the resampling strategy
resampling <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

# Define the measure
measure <- msr("regr.rsq")

# Create an autofeature selector
afs <- auto_fselector(
  fselector = fs("random_search"),
  learner = learner,
  resampling = resampling,
  measure = measure,
  term_evals = 10,
  subset_size = 0.3  # Set the desired subset size to 30%
)

# Optimize feature subset and fit the final model
future::plan("multisession", workers = 10)
with_progress({
  afs$train(task)
})


# ressample_lm ------------------------------------------------------------

resample_lm <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task,
    learner = afs$learner,
    resampling = resample, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

# visualise the resamples -------------------------------------------------


# evaluate ----------------------------------------------------------------

resample_lm$aggregate(measure = c(
  mlr3::msr("regr.bias"),
  mlr3::msr("regr.rmse"),
  mlr3::msr("regr.rsq"),
  mlr3::msr("regr.mse")))


# visualise ---------------------------------------------------------------

resample_lm$prediction() |> 
  ggplot() +
  aes(x=response, y=truth)+
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(trans=scales::yj_trans(0.1), option="G", direction=-1) +
  geom_abline(slope=1) +
  theme_light()


# train the model ---------------------------------------------------------

at$learner$train(task)

