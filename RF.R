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

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

# load in the data files ----------------------------------------------------
cube <- rast(file.path(data_path,"S2_comb_data.tif"))
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

# Define your regression task with spatial-temporal components
task_rf <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM.S2",
  backend = df_s2,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)


#define a resampling plan
resample_rf <- mlr3::rsmp("repeated_spcv_coords", folds = 11, repeats=2)

# define the random forest learner ----------------------------------------
lrn_rf<- lrn("regr.ranger", importance = "impurity")

.lrn_rf <- lrn("regr.ranger", predict_type = "response",
               mtry = to_tune(lower = 4, upper = 23),
               num.trees = to_tune(lower = 500, upper = 2500),
               sample.fraction = to_tune(lower = 0.5, upper = 0.8),
               max.depth = to_tune(lower = 20, upper = 100),
               min.node.size = to_tune(lower = 20, upper = 100))

lrnr_graph <-
  po("subsample", frac = 0.2)%>>%
  po("filter", filter = flt("importance", learner = lrn_rf), 
     filter.frac = to_tune(0.1, 0.5))%>>%
  .lrn_rf

plot(lrnr_graph)

combined_learner <- mlr3::as_learner(lrnr_graph)

at <- auto_tuner(
  tuner = tnr("random_search"),
  learner = combined_learner,
  resampling = resample,
  measure=msr("regr.rsq"), 
  term_evals = 20)

# Optimize feature subset and fit final model
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  at$train(task_rf)
})


# resample_rf ------------------------------------------------------------

resample_rf <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task,
    learner = at$learner,
    resampling = resample, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

# evaluate ----------------------------------------------------------------
#evaluate
dt <- as.data.table(resample_rf$prediction()) 

# Calculate predictive measures: 
rsq_value <- mlr3measures::rsq(dt$truth, dt$response)

mae_value <- mlr3measures::mae(dt$truth, dt$response)
rmse_value <- mlr3measures::rmse(dt$truth, dt$response)

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R-squared (rsq):", rsq_value, "\n")


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


