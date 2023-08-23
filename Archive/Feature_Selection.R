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
resample <- rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

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

importance_table[grepl("katingan_DTM", Names), Names := "DTM"]


importance_table

# Create a bar plot with the original order
ggplot(importance_table, aes(x = Names, y = Numbers)) +
  theme_gray()+
  geom_bar(stat = "identity") +
  xlab("Features") +
  ylab("Importance Score") +
  theme(
    text = element_text(size = 7,family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file="feature_selection.png", dpi = 600)

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
    breaks = seq(0, 15000, by = 5000)
  ) +
  geom_abline(slope = 1) +
  theme_light() +
  labs(x = "Predicted Canopy Height (m)", y = "Observed Canopy Height (m)")

