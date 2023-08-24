library(tidyverse)
library(terra)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#load in cubes for each dataframe
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))


#load in dataframes
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 


# create sample of df_PS.3m -----------------------------------------------
   
sample_df_PS.3m <- df_PS.3m[1:100000, ]

write.csv(sample_df_PS.3m, "sample_P3.csv", row.names = FALSE)

sample_PS3cube <- as_spatraster(sample_df_PS.3m, crs = crs(PScope_3m.cube))

writeRaster(sample_PS3cube, filename = "sample_PS3cube.tif", 
            overwrite = TRUE)

# create sample of df_PS.10m ----------------------------------------------
sample_df_PS.10m <- df_PS.10m[1:100000, ]

write.csv(sample_df_PS.10m, "sample_P10.csv", row.names = FALSE)

sample_PS10cube <- as_spatraster(sample_df_PS.10m, crs = crs(PScope_10m.cube))

writeRaster(sample_PS10cube, filename = "sample_PS10cube.tif", 
            overwrite = TRUE)

# create sample of S2 ----------------------------------------------
sample_df_S2 <- df_s2[1:100000, ]

write.csv(sample_df_S2, "sample_S2.csv", row.names = FALSE)

sample_S2cube <- as_spatraster(sample_df_S2, crs = crs(s2.cube))

writeRaster(sample_S2cube, filename = "sample_S2cube.tif", 
            overwrite = TRUE)

# create sample of combined -----------------------------------------------
sample_comb <- df_comb[1:100000, ]

write.csv(sample_comb, "sample_comb.csv", row.names = FALSE)

sample_combcube <- as_spatraster(sample_comb, crs = crs(comb_cube))

writeRaster(sample_combcube, filename = "sample_combcube.tif", 
            overwrite = TRUE)
