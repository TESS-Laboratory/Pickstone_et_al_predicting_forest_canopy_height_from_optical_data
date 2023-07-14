library(mlr3)
library(mlr3learners)
library(mlr3spatiotempcv)

df <- read_csv(file.path(data_path, "final_df.csv"))
df_10 <- read_csv(file.path(data_path, "final_df_10m.csv"))
df_20 <- read_csv(file.path(data_path, "final_df_20m.csv"))

# Create a spatiotemporal task using mlr3spatiotempcv
task <- TaskRegrST$new(
  id = "kat_base_CHM", 
  backend = df,  
  target = "CHM",
  coordinate_names = c("x", "y"), 
  extra_args = list(
    coords_as_features = FALSE, 
    crs = terra::crs(cube)  
  )
)




# ---------- DEFINTE A RESAMPLING PLAN ------------
spcv_plan <- mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats=1)

lgbm.lrn <- mlr3::lrn("regr.lightgbm")
lrn.glm <- mlr3::lrn("regr.glm")

design = benchmark_grid(task, list(lgbm.lrn, lrn.glm), spcv_plan)
# print(design)
future::plan("multisession", workers = 10) # sets the number of cores to run on -  we have
bmr = mlr3::benchmark(design)

aggr = bmr$aggregate(measures = c(msr("regr.rmse"), msr("regr.mse"), msr("regr.rsq")))

gt::gt(aggr[,4:9,])

