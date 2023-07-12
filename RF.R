library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3mbo)
library(mlr3spatiotempcv)
library(mlr3fselect)
library(ranger)
library(tidyverse)
library(terra)
library(ggpmisc)
library(progressr)

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))


# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

#Define a resampling plan
resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 1)
lrn_rf <- mlr3::lrn("regr.ranger")


# define the random forest learner ----------------------------------------
lrn_tune <- mlr3::lrn("regr.ranger", 
                      mtry = to_tune(lower = 4, upper = 10),
                      num.trees = to_tune(lower = 100, upper = 1000),
                      sample.fraction = to_tune(lower = 0.5, upper = 0.8),
                      max.depth = to_tune(lower = 20, upper = 100),
                      min.node.size = to_tune(lower = 20, upper = 100))


lrnr_graph <-
  po("subsample", frac = 0.01) %>>%
  po("filter", filter =flt("importance",learner = lrn_rf), filter.frac=to_tune(0.1,0.3))%>>%
  lrn_tune

plot(lrnr_graph)

sub_samp_lrnr <- mlr3::as_learner(lrnr_graph)

afs=auto_fselector( 
  fselector=fs("random_search"), 
  learner=sub_samp_lrnr, 
  resampling=resample, 
  measure=msr("regr.rsq"), 
  term_evals=10)

# Optimize feature subset and fit final model
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})


# resample_rf ------------------------------------------------------------

resample_rf <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task,
    learner = afs$learner,
    resampling = resample, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

# evaluate ----------------------------------------------------------------

metric_scores <- resample_rf$aggregate(measure = c(
  mlr3::msr("regr.bias"),
  mlr3::msr("regr.rmse"),
  mlr3::msr("regr.rsq"),
  mlr3::msr("regr.mae")))

metric_scores
# visualise ---------------------------------------------------------------

resample_rf$prediction() %>%
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
