library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(ranger)


set.seed(1234)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))
df <- read_csv(file.path(data_path, "final_df.csv"))


sds = apply(assay(df, 'exprs'),1,sd)


# filter out normal tissues
se_small = se[order(sds,decreasing = TRUE)[1:200],
              colData(se)$characteristics_ch1.1=='normal: no']
dat = assay(se_small, 'exprs')


# Define your regression task with spatial-temporal components
task <- mlr3spatiotempcv::TaskRegrST$new(
  id = "kat_base_CHM",
  backend = df,
  target = "CHM",
  coordinate_names = c("x", "y"),
  extra_args = list(
    coords_as_features = FALSE,
    crs = terra::crs(kat_planet)
  )
)


# Define the random forest learner
learner <- lrn("regr.randomForest")  # You can adjust the number of trees as desired

resample <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 2)

afs=auto_fselector( 
  fselector=fs("random_search"), 
  learner=learner, 
  resampling=resample, 
  measure=measure, 
  term_evals=10) 

#optimizefeaturesubsetandfitfinalmodel 
future::plan("multisession", workers = 10)
progressr::with_progress(expr = {
  afs$train(task)
})
