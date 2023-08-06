# import packages needed for the analysis ---------------------------------
library(terra)
library(viridisLite)
library(colorspace)
library(tidyverse)
library(sf)
library(raster)
library(patchwork)

# Import Planet Labs Data -------------------------------------------------

# Define Data Paths -------------------------------------------------------
#data_path <- "/Users/bri/Library/CloudStorage/OneDrive-UniversityofExeter/University/Dissertation/Data"
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"



S2_df <- read_csv(file.path(data_path, "df_S2.10m_final.csv"))
PS_10m_df <- read_csv(file.path(data_path, "df_PScope_10m.csv"))


print(names(S2_df))
#rename similar columns from Planet Scope and Sentinel2 to see the difference
S2_df <- S2_df %>%
  rename_with(~paste0(., "_s2"), c(3:7, 13:14, 20:31))
print(names(S2_df))

# Remove specific columns that are not unique to S2 - except x and y
drop_columns <- c("slope", "aspect", "roughness", "CHM", "TRIrmsd", "dtm")
S2_df_update <- S2_df %>% dplyr::select(-one_of(drop_columns))


# Merge the tibbles based on "x" and "y" columns
combined_df <- left_join(S2_df_update, PS_10m_df, by = c("x", "y"))%>%
  tidyr::drop_na()

write.csv(combined_df, file = "/raid/home/bp424/Documents/MTHM603/Data/combined_df.csv", row.names = FALSE)

# Step 5: Convert back to spatial raster
comb_cube <- rast(cube, nlyr=ncol(combined_df))
values(comb_cube) <- as.matrix(combined_df)
names(comb_cube) <- colnames(combined_df)
print(names(comb_cube))

writeRaster(comb_cube, filename = "/raid/home/bp424/Documents/MTHM603/Data/comb_cube.tif", 
            overwrite = TRUE)
