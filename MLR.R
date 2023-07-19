
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
library(sf)

# set seed ----------------------------------------------------------------

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))

# generate a task --------------------------------------------------------


# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df_10,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  
  )
)
df_10

# Define your model
learner <- lrn("regr.lm")

# Define the learner and resampling plan

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

measure <- msr("regr.rsq")

filter = flt("correlation")

answer <- filter$calculate(task)
answer

head(as.data.table(answer))


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
