
# load in packages --------------------------------------------------------
library(mlr3verse)
library(mlr3mbo)
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
library(terra)

# set seed ----------------------------------------------------------------

set.seed(5442)

# set data path -----------------------------------------------------------
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

# generate a task --------------------------------------------------------

# Create a spatiotemporal task using mlr3spatiotempcv
task_lm <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_s2,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, #excludes coordinate values within the features
    crs = terra::crs(s2.cube)  
  )
)

# Define the learner and resampling plan
resample_lm <- mlr3::rsmp("repeated_spcv_coords", folds = 11, repeats = 2)
learner_lm <- lrn("regr.lm")

#set up a pipeline  of find_correlation, so it filters through a correlation 
#matrix
lrnr_graph <-
  po("filter", filter = flt("find_correlation"), 
     filter.frac = to_tune(0.3,0.8))%>>%
  learner_lm
#turn the pipeline into a learner
sub_samp_lrnr <- mlr3::as_learner(lrnr_graph)

#tune the model using random search and 10 evalulations 
at <- auto_tuner(
  tuner = tnr("mbo"), #Bayesian optimization
  learner = sub_samp_lrnr,
  resampling = resample_lm,
  measure=msr("regr.rmse"), 
  term_evals = 10)

#optimise feature subset and fit final model 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  at$train(task_lm)
})

# Complete the resampling plan --------------------------------------------

resample_lm <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task_lm,
    learner = at$learner,
    resampling = resample_lm, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})


#evaluate using micro averaging 
dt <- as.data.table(resample_lm$prediction()) 

# Calculate performance metrics: 
rsq_value <- mlr3measures::rsq(dt$truth, dt$response)
mae_value <- mlr3measures::mae(dt$truth, dt$response)
rmse_value <- mlr3measures::rmse(dt$truth, dt$response)

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R-squared (rsq):", rsq_value, "\n")

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
  labs(x = "Predicted Canopy Height (m)", y = "LiDAR Canopy Height (m)")


# train the model  --------------------------------------------------------
at$learner$train(task_lm)

# predict the full model and plot --------------------------------------------------

p <- terra::predict(cube, at$learner, na.rm = TRUE)

plot(p)
