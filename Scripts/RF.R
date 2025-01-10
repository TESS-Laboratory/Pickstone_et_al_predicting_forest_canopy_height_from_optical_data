##This script has been written for completing Random Forest on all 
##four different datasources 
##it includes feature selection and hyperparameter tuning 

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
#load in cubes for each dataframe
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))

#load in dataframes
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 


# Define your regression task with spatial-temporal components
task_rf <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM.PS3",
  backend = df_PS.3m, #change this depending on the task you are running
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE, #excludes x and y coordinates from 
    crs = terra::crs(PScope_3m.cube) #change this depending on the dataframe
  )
)

#define a resampling plan 
resampling_rf <- mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats=2)

#create learner with hyperparameter tuning in place
ranger_tune_lrnr <- lrn("regr.ranger", predict_type = "response", importance = "impurity", 
                        mtry = to_tune(lower = 1, upper = ceiling(sqrt(task_rf$ncol - 1))),
                        sample.fraction = to_tune(lower = 0.5, upper = 1.0),
                        min.node.size = to_tune(c(1,3,5,10)),
                        replace = to_tune(c(TRUE, FALSE)))
                        
#create base learner for the importance 
rf_lrn <- mlr3::lrn("regr.ranger", predict_type = "response", importance = "impurity")

#select a subsample of data to complete this on, and set a filter fraction 
#for the features to be selected. This will select between 30% and 80% of features
lrnr_graph <-
  po("subsample", frac = 0.001) %>>%
  po("filter", filter = flt("importance", learner = rf_lrn), 
     filter.frac = to_tune(0.3,0.8))%>>%
  ranger_tune_lrnr

plot(lrnr_graph)

#create learner from pipeline
combined_learner_feature <- as_learner(lrnr_graph)

#set up tuner 
at <- auto_tuner(
  tuner = tnr("mbo"), #Bayesian optimization tuner
  learner = combined_learner_feature,
  resampling = resampling_rf,
  measure=msr("regr.rmse"), 
  term_evals = 10)

# Optimize feature subset and fit final model
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  at$train(task_rf)
})

#extract the hyperparamerters that were used within the model 
at$learner$state$model

#extract the feature that were used in the model 
at$learner$state$model$importance$features

#extract the scores of the model
importance <- at$learner$state$model$importance$scores

#create a datatable of the importance score to be made into a plot
importance_table <- data.table(
  Names = names(importance),
  Numbers = importance
)

#export as csv, so it can be used later if needed 
write.csv(importance_table, file = "/raid/home/bp424/Documents/MTHM603/Data/importance_PS3_2.csv", row.names = FALSE)


# Convert 'Names' column to factor with levels in original order
importance_table$Names <- factor(
  importance_table$Names,
  levels = unique(importance_table$Names)
)

importance_table[grepl("dtm", Names), Names := "DTM"]


# Create a bar plot with the original order
ggplot(importance_table, aes(x = Numbers, y = Names)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  xlab("Importance Score") +
  ylab("Features") +
  theme(
    text = element_text(size = 4, family = "Times New Roman")) +
  scale_y_discrete(limits = rev(unique(importance_table$Names)))

#save plot
ggsave(file="feature_selection_PS3_2.png", dpi = 600)

# resample_rf ------------------------------------------------------------
#extract tuning results from previous sub sample
tr <- at$tuning_result
tr <- unlist(tr$learner_param_vals, recursive = FALSE)

#add hyperparameter tuning result to new learner
learner_rf_update <- lrn("regr.ranger", predict_type = "response", importance = "impurity", 
                                             mtry = tr$regr.ranger.mtry,
                                             sample.fraction = tr$regr.ranger.sample.fraction,
                                             min.node.size = as.numeric(tr$regr.ranger.min.node.size),
                                             replace = tr$regr.ranger.replace)


#set a fraction of the learner - this will run on 5% of the data
lrnr_graph <-
  po("subsample", frac = 0.05) %>>%
  po("filter", filter = flt("importance", learner = rf_lrn), 
     filter.frac = tr$importance.filter.frac) %>>%
  learner_rf_update

plot(lrnr_graph)

#turn pipeline into learner
combined_learner <- as_learner(lrnr_graph)

#resample the task using the resampling strategy set above (10 folds, 2 repeats)
resample_rf <- progressr::with_progress(expr ={
  mlr3::resample(
    task = task_rf,
    learner = combined_learner,
    resampling = resampling_rf, 
    store_models = FALSE,
    encapsulate = "evaluate"
  )
})

# evaluate ----------------------------------------------------------------
dt <- as.data.table(resample_rf$prediction()) 

write.csv(dt, file = "/raid/home/bp424/Documents/MTHM603/Data/dt_RF_P3.csv", row.names = FALSE)

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

combined_learner$train(task_rf)

# predict the full model --------------------------------------------------
p <- terra::predict(PScope_3m.cube, combined_learner, na.rm = TRUE)

#save the full model to be plotted later
writeRaster(p, filename = "/raid/home/bp424/Documents/MTHM603/Data/PS3_RF_predict.tif", 
            overwrite = TRUE)

#see how the new canopy height model looks 
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


