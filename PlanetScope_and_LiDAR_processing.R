## This script is used for pre-processing the optical data and LiDAR Data
##to create the 3m and 10m resolution PlanetScope Data
##This includes calcualtion of vegetation spectral indices, topogrpahic metrics 
##and the generation of a canopy height model
##This script should be completed after the extraction of Sentinel-2 Data

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

S2_10m <- rast(file.path(data_path,"kat/kat_final_S2_2021-02-27.tif"))
kat_boundary <- st_read(file.path(data_path, "katingan_aoi.gpkg"))

st_area(kat_boundary)

# import optical planet labs data -----------------------------------------

kat_planet <- rast(file.path(data_path,"Katingan-Comp-22-median.tif"))

print(names(kat_planet))

# import the LiDAR data ---------------------------------------------------

# Read LiDAR dtm and dsm file 
dtm_LiDAR <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))


# Crop the data so they have the same extent ------------------------------
#this will also save computing time for the calculations 
kat_planet <- terra::crop(kat_planet, dtm_LiDAR)

#NDVI does not need to be scaled so do this calculation first 
kat_planet$NDVI <- (kat_planet$nir - kat_planet$red)/(kat_planet$nir +kat_planet$red)

#scale down the 8 existing bands - stored within "names"
kat_planet <- kat_planet[[(names(kat_planet))]]/10000


# Do the band math - HG examples
kat_planet$NDRE <- (kat_planet$nir - kat_planet$rededge)/(kat_planet$nir +kat_planet$rededge)
kat_planet$EVI <- 2.5 * ((kat_planet$nir) - (kat_planet$red)) /
  ((kat_planet$nir) + 6 * (kat_planet$red) - 7.5 * (kat_planet$blue) + 1)

#Calculations by BP
kat_planet$AVI <- (kat_planet$nir * (1 - kat_planet$red)*(kat_planet$nir - kat_planet$red))^(1/3)
kat_planet$RDVI <- (kat_planet$nir - kat_planet$red)/((kat_planet$nir + kat_planet$red)^0.5)
kat_planet$CIRE <- ((kat_planet$nir/kat_planet$rededge)-1)
kat_planet$GNDVI <- (kat_planet$nir - kat_planet$green)/
  (kat_planet$nir + kat_planet$green)
kat_planet$RGBVI <- (kat_planet$green^2 - kat_planet$blue * kat_planet$red)/
  (kat_planet$green^2 + kat_planet$blue * kat_planet$red)
kat_planet$BNDVI <- (kat_planet$nir - kat_planet$blue)/(kat_planet$nir + kat_planet$blue)
kat_planet$CVI <- (kat_planet$nir * kat_planet$red)/(kat_planet$green^2)
kat_planet$GBNDVI <- (kat_planet$nir - (kat_planet$green + kat_planet$blue))/
  (kat_planet$nir + (kat_planet$green + kat_planet$blue))
kat_planet$GLI <- (2 * kat_planet$green - kat_planet$red - kat_planet$blue)/
  (2 * kat_planet$green + kat_planet$red + kat_planet$blue)
kat_planet$GEMI <- ((2 *((kat_planet$nir^2.0)-(kat_planet$red^2.0)) + 1.5 * 
                       kat_planet$nir + 0.5 * kat_planet$red)/(kat_planet$nir + kat_planet$red + 0.5)) * 
  (1.0 - 0.25 * ((2.0 *((kat_planet$nir^2.0)- (kat_planet$red^2)) + 
                    1.5 * kat_planet$nir + 0.5 * kat_planet$red)/(kat_planet$nir + kat_planet$red + 0.5))) - 
  ((kat_planet$red - 0.125)/(1 - kat_planet$red))


# Calculate Canopy Height Model  ------------------------------------------
CHM <- dsm - dtm_LiDAR
names(CHM) <- "CHM"


#make sure the CHM has the same projection as the PlanetScope Data
CHM_project_3m <- project(CHM, kat_planet, method="bilinear")


# download the digital terrain model from corpenicus ---------------------
bbox <- st_bbox(kat_planet) 
bbox_wgs84 <- bbox |>
  st_as_sfc(crs=32749) |> #make sure you change this
  st_transform("EPSG:4326") |>
  st_bbox()

# extracting digital terrain model ----------------------------------------
mpc_dtm_src <- function(aoi_box,
                        collection = "cop-dem-glo-30"){
  
  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
  rstac::get_request(s_obj)
  
  it_obj <- s_obj  |>
    rstac::stac_search(collections = collection[1],
                       bbox = c(aoi_box["xmin"],aoi_box["ymin"],
                                aoi_box["xmax"],aoi_box["ymax"]))  |>
    rstac::get_request()
  
  src_list <-rstac::assets_url(it_obj)
  
  .urls <- src_list[grep(".tif$", src_list)]
  
  sapply(.urls, function(x) paste0("/vsicurl/", x), USE.NAMES =FALSE)
  
}

dtm.3m <- mpc_dtm_src(bbox_wgs84) |> # call the function to get the tile urls
  lapply(rast) |> # iterate over the tiles and load as a spatRaster object
  terra::sprc() |> # convert to a collection
  terra::merge() |> # merge the tiles - basic mosaic - nothing overlaps here so this is fine.
  project(kat_planet, method="bilinear") # project into our desired CRS and extent etc.
names(dtm.3m) <- "dtm"

# now generate a few geomorphometric layers
asp <- terra::terrain(dtm.3m, "aspect")
slp <- terrain(dtm.3m, "slope")
TRI <- terrain(dtm.3m, "TRIrmsd")
rough <- terrain(dtm.3m, "roughness")

terrain <- c(dtm.3m, asp, slp, TRI, rough)

PScope_3m <- c(kat_planet,terrain, CHM_project_3m)

#export file, so that the combined raster file can be used in other analyses 
writeRaster(PScope_3m, filename = "/raid/home/bp424/Documents/MTHM603/Data/PScope_3m.tif", 
            overwrite = TRUE)

PScope_3m_df <- as.data.frame(PScope_3m, xy=TRUE) |> 
  tidyr::drop_na()

#export data table so it can be used in other analyses 
write.csv(PScope_3m_df, file = "/raid/home/bp424/Documents/MTHM603/Data/PScope_3m_df.csv", row.names = FALSE)

# resample ----------------------------------------------------------------
#resample 3m stack to 10m stack for Planet Scope 
CHM10 <- project(CHM, S2_10m, method="bilinear")
mpc_dtm_src <- function(aoi_box,
                        collection = "cop-dem-glo-30"){
  
  s_obj <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
  rstac::get_request(s_obj)
  
  it_obj <- s_obj  |>
    rstac::stac_search(collections = collection[1],
                       bbox = c(aoi_box["xmin"],aoi_box["ymin"],
                                aoi_box["xmax"],aoi_box["ymax"]))  |>
    rstac::get_request()
  
  src_list <-rstac::assets_url(it_obj)
  
  .urls <- src_list[grep(".tif$", src_list)]
  
  sapply(.urls, function(x) paste0("/vsicurl/", x), USE.NAMES =FALSE)
  
}

dtm_10 <- mpc_dtm_src(bbox_wgs84) |> # call the function to get the tile urls
  lapply(rast) |> # iterate over the tiles and load as a spatRaster object
  terra::sprc() |> # convert to a collection
  terra::merge() |> # merge the tiles - basic mosaic - nothing overlaps here so this is fine.
  project(S2_10m, method="bilinear") # project into our desired CRS and extent etc.
names(dtm_10) <- "dtm"

# now generate a few geomorphometric layers
asp_10 <- terra::terrain(dtm_10, "aspect")
slp_10 <- terrain(dtm_10, "slope")
TRI_10 <- terrain(dtm_10, "TRIrmsd")
rough_10 <- terrain(dtm_10, "roughness")

terrain_10 <- c(dtm_10,asp_10, slp_10, TRI_10, rough_10)

kat_planet10 <- project(kat_planet, S2_10m, method="bilinear")

PScope_10m <- c(kat_planet10, terrain_10, CHM10)


#export file, so that the combined raster file can be used in other analyses 
writeRaster(PScope_10m, filename = "/raid/home/bp424/Documents/MTHM603/Data/PScope_10m.tif", 
            overwrite = TRUE)

#create datatable

df_PScope_10m <- as.data.frame(PScope_10m, xy=TRUE) %>%
  tidyr::drop_na()

write.csv(df_PScope_10m, file = "/raid/home/bp424/Documents/MTHM603/Data/df_PScope_10m.csv", row.names = FALSE)

# Create density plots of canopy heights for 1m, 3m and 10m ------------------------------------
# Convert raster to tibble
CHM_df_1m <- as.data.frame(CHM)

(CHM.plots <- ggplot() +
    geom_density(data = CHM_df_1m, aes(x = CHM, fill = "1"), alpha = 0.2) +
    geom_density(data = df_PS.3m, aes(x = CHM, fill = "3"), alpha = 0.2) +
    geom_density(data = df_PS.10m, aes(x = CHM, fill = "10"), alpha = 0.2) +
    scale_fill_manual(values = c("1" = "gray60", '3' = '#ffcf20ff', '10' = '#2f9aa0ff'), 
                      breaks = c("1", "3", "10"),
                      name = 'Resolution (m)') +
    theme_classic() +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 8),
      axis.title = element_text(family = "Times New Roman", size = 8),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.position = c(0.85, 0.85),  
      legend.justification = c(1, 1),  # Top-right corner
      legend.title = element_text(family = "Times New Roman", size = 8),  # Legend title font
      legend.text = element_text(family = "Times New Roman", size = 6),
      legend.key.size = unit(0.6, "lines"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ))

ggsave(file="CHM.hist.plots.png", dpi = 600)
