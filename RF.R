library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3spatiotempcv)
library(mlr3fselect)
library(ranger)
library(tidyverse)
library(terra)

library(rlang)

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))


# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = train_d,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)


# Define the random forest learner
learner <- lrn("regr.ranger", predict_type = "response")

#search_space = ps(
 # mtry = p_int(lower = 4, upper = 10),
  #num.trees = p_int(lower = 500, upper = 2000),
  #sample.fraction = p_dbl(lower = 0.5, upper = 0.8),
  #max.depth = p_int(lower = 20, upper = 100),
  #min.node.size = p_int(lower = 20, upper = 100)
)

measure <- msr("regr.rsq")

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 1)


# set up a pipeline learner -----------------------------------------------

lrn_filt <- 
  po("subsample", frac = 0.5) %>>%
  po("filter", filter = flt("importance", learner = learner), filter.frac = to_tune(0.1,0.3))%>>%
  learner

combined.lrnr <- as_learner(lrn_filt)

afs=auto_tuner( 
  tuner=tnr("random_search"), 
  learner=combined.lrnr, 
  resampling=resample, 
  measure=measure, 
  term_evals=10)
#subset_size = 0.3)) 


#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})
