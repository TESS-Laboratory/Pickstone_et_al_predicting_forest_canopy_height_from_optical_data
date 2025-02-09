##This script is used for combining 10 m Sentinel-2 Data with 10 m PlanetScope
#Data - including the creation of a dataframe and spatraster 

# import packages needed for the analysis ---------------------------------
library(terra)
library(viridisLite)
library(colorspace)
library(tidyverse)
library(sf)
library(raster)
library(patchwork)


# Define Data Paths -------------------------------------------------------
#data_path <- "/Users/bri/Library/CloudStorage/OneDrive-UniversityofExeter/University/Dissertation/Data"
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#read in sentinel 2 and PlanetScope 10 m resolution data 
S2_df <- read_csv(file.path(data_path, "df_S2.10m_final.csv"))
PS_10m_df <- read_csv(file.path(data_path, "df_PScope_10m.csv"))

#rename similar columns from Planet Scope and Sentinel2 to see the difference
S2_df <- S2_df %>%
  rename_with(~paste0(., "_s2"), c(3:7, 13:14, 20:31))
print(names(S2_df))

# Remove specific columns that are not unique to S2 - except x and y
drop_columns <- c("slope", "aspect", "roughness", "TRIrmsd", "dtm", "CHM")
S2_df_update <- S2_df %>% dplyr::select(-one_of(drop_columns))

# Merge the tibbles based on "x" and "y" columns
combined_df <- merge(S2_df_update, PS_10m_df, by = c("x", "y"))%>%
  tidyr::drop_na()

print(names(combined_df))

#create a spatraster of the combined dataframe, with the same coordinates as 
#the previous datacubes 
comb_cube <- as_spatraster(combined_df, crs = crs(s2.cube))

#check this has worked by plotting the CHM
plot(comb_cube$CHM)

#save the dataframe 
write.csv(combined_df, file = "/raid/home/bp424/Documents/MTHM603/Data/combined_df.csv", row.names = FALSE)

#save the spatraster
writeRaster(comb_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/comb_cube.tif", 
            overwrite = TRUE)