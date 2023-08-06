library(mlr3)
library(mlr3tuning)
library(mlr3verse)
library(mlr3learners)
library(mlr3mbo)
library(mlr3spatiotempcv)
library(mlr3fselect)
library(ranger)
library(tidyverse)
library(terra)
library(ggpmisc)
library(progressr)
library(mlr3pipelines)
library(mlr3tuningspaces)

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

# load in the data files ----------------------------------------------------
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))

df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 


# Define your regression task with spatial-temporal components
task_rf <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM.PS",
  backend = df_comb, #change this depending on the task you are running
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE, #excludes x and y coordinates from 
    crs = terra::crs(comb_cube)
  )
)

#define a resampling plan
resampling_rf <- mlr3::rsmp("repeated_spcv_coords", folds = 11, repeats=2)

#create learner with hyperparameter tuning in place
ranger_tune_lrnr <- lrn("regr.ranger", predict_type = "response", importance = "impurity", 
                        mtry = to_tune(lower = 1, upper = ceiling(sqrt(task_rf$ncol - 1))),
                        sample.fraction = to_tune(lower = 0.5, upper = 1.0),
                        min.node.size = to_tune(c(1,3,5,10)),
                        replace = to_tune(c(TRUE, FALSE)))
                        
#create base learner for the importance 
rf_lrn <- mlr3::lrn("regr.ranger", predict_type = "response", importance = "impurity")

lrnr_graph <-
  po("subsample", frac = 0.001) %>>%
  po("filter", filter = flt("importance", learner = rf_lrn), 
     filter.frac = to_tune(0.3,0.8))%>>%
  ranger_tune_lrnr

plot(lrnr_graph)

combined_learner <- as_learner(lrnr_graph)

at <- auto_tuner(
  tuner = tnr("mbo"), #Bayesian optimization tuner
  learner = combined_learner,
  resampling = resampling_rf,
  measure=msr("regr.rmse"), 
  term_evals = 10)

# Optimize feature subset and fit final model
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  at$train(task_rf)
})


# resample_rf ------------------------------------------------------------

tr <- at$tuning_result

tr <- unlist(tr$learner_param_vals, recursive = FALSE)




learner_rf_update <- lrn("regr.ranger", predict_type = "response", importance = "impurity", 
                                             mtry = tr$regr.ranger.mtry,
                                             sample.fraction = tr$regr.ranger.sample.fraction,
                                             min.node.size = as.numeric(tr$regr.ranger.min.node.size),
                                             replace = tr$regr.ranger.replace)


lrnr_graph <-
  po("subsample", frac = 0.2) %>>%
  po("filter", filter = flt("importance", learner = rf_lrn), 
     filter.frac = tr$importance.filter.frac) %>>%
  learner_rf_update

plot(lrnr_graph)

combined_learner <- as_learner(lrnr_graph)

resample_rf <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task_rf,
    learner = combined_learner,
    resampling = resampling_rf, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

importance <- resample_rf$learner$timings

importance_table <- data.table(
  Names = names(importance),
  Numbers = importance
)
importance_table
# evaluate ----------------------------------------------------------------
dt <- as.data.table(resample_rf$prediction()) 

# Calculate predictive measures: 
rsq_value <- mlr3measures::rsq(dt$truth, dt$response)

mae_value <- mlr3measures::mae(dt$truth, dt$response)
rmse_value <- mlr3measures::rmse(dt$truth, dt$response)

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R-squared (rsq):", rsq_value, "\n")

# visualise ---------------------------------------------------------------

resample_rf$prediction() %>%
  ggplot() +
  aes(x = truth, y = response) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
    breaks = c(0, 5000, 20000, 50000)
  ) +
  geom_abline(slope = 1) +
  theme_light() +
  labs(x = "LiDAR Canopy Height (m)", y = "Predicted Canopy Height (m)")


# train the model  --------------------------------------------------------


resample_rf$learner$train(task_rf)

# predict the full model --------------------------------------------------

p <- terra::predict(s2.cube, resample_rf$learner, na.rm = TRUE)

plot(p)

