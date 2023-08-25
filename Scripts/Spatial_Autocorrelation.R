##This code is for determining the spatial autocorrelation within the study site
##It is also used to look into the resampling plan
# load in packages --------------------------------------------------------
library(geoR)
library(gstat)
library(terra)
library(sf)
library(spdep)
library(sp)
library(tidyverse)
library(mlr3)
library(mlr3learners)
library(patchwork)

set.seed(1234)
# load in required datafiles ----------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
dtm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m

#calculate the canopy height model 
CHM <- dsm - dtm
names(CHM) <- "CHM"

# Pre-process CHM  ------------------------------------------------
# Change the resolution to 100x100 meters
CHM_100 <- aggregate(CHM, fact = 100)

# Convert the CHM raster to a spatial points data frame
CHM_points <- as.data.frame(CHM_100, xy = TRUE)
coordinates(CHM_points) <- ~x + y

# Create Variogram --------------------------------------------------------
vgm1 <- variogram(log(CHM)~1, CHM_points)
lzn.fit=fit.variogram(vgm1,model=vgm(1,"Exp",3000,1))


# Plot the variogram to visualize the autocorrelation structure
file_name <- "variogram.png"
dpi <- 600

# Create the PNG device with high resolution
png(file = file_name, width = 8, height = 6, units = "in", res = dpi)

plot(vgm1,lzn.fit, 
     xlab = "Distance (m)", ylab = "Semi-variance")

dev.off()

# Check spatial correlation using Moran's I -------------------------------
spcor <- terra::autocor(CHM_100, method="moran", global=TRUE)


# check for the appropriate amount of resamples ---------------------------


# Create a task
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df_PS.10m,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(PScope_10m.cube)
  )
)

#set resampling plan with varying folds to see the change in RMSE 
spcv_plan_2 <- mlr3::rsmp("spcv_coords", folds = 2, repeats=2)
spcv_plan_3 <- mlr3::rsmp("spcv_coords", folds = 3, repeats=2)
spcv_plan_4 <- mlr3::rsmp("repeated_spcv_coords", folds = 4, repeats=2)
spcv_plan_5 <- mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats=2)
spcv_plan_6 <- mlr3::rsmp("repeated_spcv_coords", folds = 6, repeats=2)
spcv_plan_7 <- mlr3::rsmp("repeated_spcv_coords", folds = 7, repeats=2)
spcv_plan_8 <- mlr3::rsmp("repeated_spcv_coords", folds = 8, repeats=2)
spcv_plan_9 <- mlr3::rsmp("repeated_spcv_coords", folds = 9, repeats=2)
spcv_plan_10 <- mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats=2)


#define the learner
learner <- lrn("regr.lm")

design = benchmark_grid(task, learner, list(spcv_plan_2,spcv_plan_3,
                                            spcv_plan_4, spcv_plan_5,
                                            spcv_plan_6, spcv_plan_7, 
                                            spcv_plan_8, spcv_plan_9, 
                                            spcv_plan_10))



future::plan("multisession", workers = 10) # sets the number of cores to run on
bmr = mlr3::benchmark(design)

aggr = bmr$aggregate(measures = c(msr("regr.rmse")))

gt::gt(aggr[,3:7,])


result_rmse <- bmr$aggregate(msr("regr.rmse"))

results_df <- bind_rows(result_rmse)

# Print the results
print(results_df)


# Plot the results to visualize the performance across different fold values
(ggplot(results_df, aes(x = iters, y = regr.rmse)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  theme_bw()+
  theme(axis.text = element_text(family = "Times New Roman", size = 6),
       axis.title = element_text(family = "Times New Roman", size = 6))+
  xlab("Number of Folds with Two Repeats") +
  ylab("Root Mean Squared Error (RMSE) (m)") +
  scale_x_continuous(breaks = seq(1, max(results_df$iters), by = 1)))


ggsave(file="blockcv_plot.png", dpi = 600)


# Visualise the resampling strategy  --------------------------------------

#create the resampling strategy for the train and test set 
resample <- rsmp("repeated_spcv_coords", folds = 10, repeats = 2)


# Create the autoplot with custom font and tilted x-axis labels

(Fold_1 <- autoplot(resample, task = task, fold_id = 1:1) +
    theme(
      legend.position = "none",
      text = element_text(size = 7, family = "Times New Roman"),
      axis.text.x = element_text(angle = 45, hjust = 1)))


(Fold_2 <- autoplot(resample, task = task, fold_id = 2:2) +
  theme(
    legend.position = "none",
    text = element_text(size = 7, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)))

#combine using patchwork
Fold_1 + Fold_2

#save to data files
ggsave(file="resampling_plan.png", dpi = 600)

