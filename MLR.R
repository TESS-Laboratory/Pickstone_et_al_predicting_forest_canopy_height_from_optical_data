
# load in packages --------------------------------------------------------
library(mlr3verse)
library(mlr3spatiotempcv)
library(terra)
library(tidyverse)
library(dplyr)
library(ggpmisc)
library(lightgbm)


# set seed ----------------------------------------------------------------

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
df <- read_csv(file.path(data_path, "df.csv"))


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
spcv_plan <- mlr3::rsmp("repeated_spcv_coords", folds = 4, repeats = 5)
#autoplot(spcv_plan, task = task, fold_id = 1:3)


# define some learners ----------------------------------------------------

filt.lrn <- mlr3::lrn("regr.lightgbm")

.lrn <- mlr3::lrn("regr.lightgbm", predict_type = "response", boosting = "gbdt", 
                  learning_rate = to_tune(lower = 0.01, upper = 0.1, logscale = TRUE), 
                  max_bin= to_tune(lower = 100, upper= 1500), 
                  num_leaves = to_tune(lower = 10, upper = 100))


# set up a pipeline learner -----------------------------------------------
.lrn.filt <- 
  po("subsample", frac = 0.6) %>>%
  po("filter", filter = flt("importance", learner= filt.lrn), filter.frac=to_tune(0.1, 0.3))%>>%
  mlr3::lrn("regr.lm")  #simple linear model
  #.lrn

plot(.lrn.filt)

combined.lrnr <- as_learner(.lrn.filt)


# tune the model ----------------------------------------------------------

at <- auto_tuner(
  method = tnr("random_search"),
  learner = combined.lrnr,
  resampling = spcv_plan,
  measure = mlr3::msr(c("regr.rsq")),
  term_evals = 10
)

future::plan("multisession", workers = 10)
progressr::with_progress(expr= {
  at$train(task)
})


# do spatial resampling ---------------------------------------------------
resample_lgbm <- progressor::with_progressor(expr {
  mlr3::resample(
    task = task, 
    learner = at$learner, 
    resampling = spcv_plan,
    store_models = FALSE, 
    encapsulate = "evaluate"
  )
})

