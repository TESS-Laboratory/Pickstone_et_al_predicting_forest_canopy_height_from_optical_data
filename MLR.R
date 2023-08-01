
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
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

df_comb
# generate a task --------------------------------------------------------
# Normalize columns

names(df_s2)

# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "CHM.10m", 
  backend = df_comb,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  
  )
)


# Define the model - Multiple Linear Regression
learner <- lrn("regr.lm")

# Define the learner and resampling plan

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 11, repeats = 2)

measure <- msr("regr.rsq")

lrnr_graph <-
  po("filter", filter = flt("correlation", filter.frac = 0.5))

plot(lrnr_graph)

sub_samp_lrnr <- mlr3::as_learner(lrnr_graph)

afs <-  auto_fselector(
  fselector = fs("random_search"),
  learner = learner,
  resampling = resample,
  measure=msr("regr.rsq"), 
  term_evals = 20)


#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})

afs$fselect_result


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



#evaluate
dt <- as.data.table(resample_lm$prediction()) 

# Calculate predictive measures: 
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
  labs(x = "Predicted Canopy Height (m)", y = "Observed Canopy Height (m)")
