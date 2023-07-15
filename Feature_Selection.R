library(mlr3verse)
library(mlr3spatiotempcv)
library(mlr3viz)
library(data.table)
library(ggplot2)

data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
cube <- rast(file.path(data_path,"comb_cube.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))

# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df_10,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(cube)
  )
)

#create the resampling strategy for the train and test set 
resample <- rsmp("repeated_spcv_coords", folds = 3, repeats = 1)

# Create the autoplot with custom font and tilted x-axis labels
autoplot(resample, task = task, fold_id = 1:1) +
  theme(
    text = element_text(size = 4, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 2:2) +
  theme(
    text = element_text(size = 4,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

autoplot(resample, task = task, fold_id = 3:3) +
  theme(
    text = element_text(size = 4,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file="Fold_3.png", dpi = 600)


lrn_ranger = lrn("regr.ranger", importance = "impurity")

at <- auto_tuner(
  tuner= tnr("random_search"),
  learner = lrn_ranger,
  resampling = resample,
  measure = msr("regr.rsq"),
  term_evals = 10
)

future::plan("multisession", workers = 10)
at$train(task)
importance <- at$learner$importance()

importance_table <- data.table(
  Names = names(importance),
  Numbers = importance
)

# Convert 'Names' column to factor with levels in original order
importance_table$Names <- factor(
  importance_table$Names,
  levels = unique(importance_table$Names)
)

# Create a bar plot with the original order
ggplot(importance_table, aes(x = Names, y = Numbers)) +
  geom_bar(stat = "identity") +
  xlab("Names") +
  ylab("Numbers") +
  ggtitle("Histogram of Importance") +
  theme_minimal()

