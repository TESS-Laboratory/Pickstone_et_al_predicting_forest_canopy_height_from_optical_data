
# load in packages --------------------------------------------------------
library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3filters)
library(mlr3fselect)
library(mlr3learners)
library(mlr3mbo)
library(tidyverse)
library(terra)
library(ggpmisc)
library(mlr3viz)
library(GGally)
library(gt)
library(sf)

# set seed ----------------------------------------------------------------

set.seed(1234)



# set data path -----------------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

# load in the data files ----------------------------------------------------
cube <- rast(file.path(data_path,"S2_comb_data.tif"))
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv"))
df_PS <- read_csv(file.path(data_path, "df_PScope_10m.csv"))

# look for cross correlation ----------------------------------------------

filter = flt("find_correlation")
filter$calculate(task)
as.data.table(filter)


# generate a task --------------------------------------------------------


# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df_PS,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  
  )
)

# Define your model

learner <- lrn("regr.lm")

# Define the learner and resampling plan

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

measure <- msr("regr.rsq")


graph = po("filter", filter = flt("correlation")) %>>%
  mlr3::lrn("regr.lm")

plot(graph)

sub_samp_lrnr <- mlr3::as_learner(graph)

at=auto_tuner( 
  tuner=tnr("random_search"), 
  learner=sub_samp_lrnr, 
  resampling=resample, 
  measure=measure, 
  term_evals=10)

afs = auto_fselector(
  fselector = fs("random_search"),
  learner = learner,
  resampling = resample,
  measure = measure,
  term_evals = 10)


#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  model <- at$train(task)
})

model
# resample_lm ------------------------------------------------------------

resample_lm <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task,
    learner = at$learner,
    resampling = resample, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

dt <- as.data.table(resample_lm$prediction()) 
mlr3measures::rsq(dt$truth, dt$response)

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
  ) +
  geom_abline(slope = 1) +
  theme_light() +
  labs(x = "Predicted Canopy Height (m)", y = "Observed Canopy Height (m)")
