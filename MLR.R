
# load in packages --------------------------------------------------------
library(mlr3verse)
library(mlr3spatiotempcv)
library(terra)
library(tidyverse)
library(dplyr)
library(ggpmisc)
library(cluster)
install.packages("factoextra")
library(factoextra)



# set seed ----------------------------------------------------------------

set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
df <- read_csv(file.path(data_path, "df.csv"))


# generate a task ---------------------------------------------------------

task = mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM", 
  backend= df,
  target = "CHM",
  coordinate_names= c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs= terra::crs(kat_planet))
)

# find optimal number of clusters -----------------------------------------
# Exclude x and y from the dataframe
data <- df[, !(colnames(df) %in% c("x", "y"))]

# Define the range of cluster numbers to evaluate
k_values <- 2:10

# Perform k-means clustering for each k value and calculate total within-cluster sum of squares (wss)
wss <- sapply(k_values, function(k) {
  kmeans(data, centers = k)$tot.withinss
})

# Plot the elbow curve
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares (wss)")

# Identify the optimal number of clusters based on the elbow point
elbow_point <- fviz_nbclust(data, kmeans, method = "wss")$ch

# Add a vertical line at the elbow point
abline(v = elbow_point, lty = 2)


# define a resampling plan ------------------------------------------------

spcv_plan <- mlr3::rsmp("repeated_spcv_coords", folds = 6, repeats = 1)
autoplot(spcv_plan, task = task, fold_id = 1:3)


# define some learners ----------------------------------------------------

filt.lrn <- mlr3::lrn("regr.lightgbm", predict_type = "response", boosting ="dart")

.lrn <- mlr3::lrn("regr.lightgbm", predict_type = "response", boosting = "dart", 
                  learning_rate = to_tune(lower = 0.01, upper = 0.1, logscale = TRUE), 
                  max_bin= to_tune(lower = 100, upper= 1000), 
                  num_leaves = to_tune(lower = 10, upper = 75))


# set up a pipeline learner -----------------------------------------------


