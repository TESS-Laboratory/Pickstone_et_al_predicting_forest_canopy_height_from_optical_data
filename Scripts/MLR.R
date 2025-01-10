##This script has been written for completing MLR on all 
##four different datasources 
##it includes feature selection using find correlation 

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
library(tidyterra)

# set seed ----------------------------------------------------------------

set.seed(5442)

# set data path -----------------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

# load in the data files ----------------------------------------------------
#load in the cube packages (spatraster)
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))

#load in the dataframes
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 

# generate a task --------------------------------------------------------
# Create a spatiotemporal task using mlr3spatiotempcv
task_lm <- TaskRegrST$new(
  id = "kat.CHM", 
  backend = df_PS.3m,  #change this to the dataframe you are interested in modelling
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, #excludes coordinate values within the features
    crs = terra::crs(PScope_3m.cube)  
  )
)

# Define the learner and resampling plan
resample_lm <- mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats = 2)

#define simple liner regression learner 
learner_lm <- lrn("regr.lm") #linear regression learner

#set up a pipeline  of find_correlation, so it filters through a correlation 
#matrix between 0.3 and 0.8 of the filters 
lrnr_graph <-
  po("filter", filter = flt("find_correlation"), 
     filter.frac = to_tune(0.3,0.8))%>>%
  learner_lm

#turn the pipeline into a learner
sub_samp_lrnr <- mlr3::as_learner(lrnr_graph)

#tune the model using random search and 10 evalulations based on RMSE
at <- auto_tuner(
  tuner = tnr("random_search"),
  learner = sub_samp_lrnr,
  resampling = resample_lm,
  measure=msr("regr.rmse"), 
  term_evals = 10)


#optimise feature subset and fit final model 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  at$train(task_lm)
})

#Extract features that were used
at$learner$state$model$find_correlation

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

#save the dataframe if needed to be used for later
write.csv(dt, file = "/raid/home/bp424/Documents/MTHM603/Data/dt_MLR_PS3.csv", row.names = FALSE)

# Calculate performance metrics (RMSE, MAE, R2): 
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
  aes(x = truth, y = response) +
  geom_bin_2d(binwidth = 0.3) +
  scale_fill_viridis_c(
    trans = scales::yj_trans(0.1),
    option = "G",
    direction = -1,
    breaks = seq(0, 3000, length.out = 3) 
  ) +
  geom_abline(slope = 1, linewidth = 0.2) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman"),  
    axis.text = element_text(size = 10),              
    axis.title = element_text(size = 10),             
    legend.text = element_text(size = 8),            
    legend.title = element_text(size = 10)            
  ) +
  labs(x = "LiDAR Canopy Height (m)", 
       y = "Predicted Canopy Height (m)", 
       fill = "Count")+ 
  scale_x_continuous(breaks = seq(0, 40, by = 10)) +  
  scale_y_continuous(breaks = seq(-10, 40, by = 10), limits = c(-10, 40))


# train the model  --------------------------------------------------------
at$learner$train(task_lm)

# predict the full model and plot --------------------------------------------------
p <- terra::predict(PScope_3m.cube, at$learner, na.rm = TRUE)

#save this as spatraster to be plotted later 
writeRaster(p, filename = "/raid/home/bp424/Documents/MTHM603/Data/PS3_MLR_predict.tif", 
            overwrite = TRUE)

(CHM.plot <- ggplot() + 
    theme_bw() +
    geom_spatraster(data = p)+
    theme(axis.text = element_text(size = 6, family = "Times New Roman"), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 4, family = "Times New Roman"), 
          legend.title = element_text(size = 6, family = "Times New Roman"), 
          axis.ticks = element_line(linewidth = 0.1))+
    scale_fill_gradientn(
      name = "Canopy Height (m)",
      colors = viridisLite::viridis(n = 100),
      na.value = 'transparent',
      limits = c(0, 55), 
      guide = guide_colorbar(barwidth = .5, barheight = 2)))
