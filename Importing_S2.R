
# load in required packages -----------------------------------------------
library(rstac)
library(sf)
library(gdalcubes)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
dtm_LiDAR <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DTM.tif"))
dsm <- rast(file.path(data_path,"Original-DEMS/katingan_DEMS/katingan_DSM.tif"))
#create bounding box of the area interested in for the sen-2 timages
#transform it to epsg 4326
bbox <- st_bbox(dtm_LiDAR) 
bbox_wgs84 <- bbox |>
  st_as_sfc(crs=32749) |> #make sure you change this
  st_transform("EPSG:4326") |>
  st_bbox()

#pecify the STAC-API endpoint URL and query all available 
#images for our area and time of interest.
s = stac("https://earth-search.aws.element84.com/v0")
items = s |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(bbox_wgs84["xmin"],bbox_wgs84["ymin"],
                       bbox_wgs84["xmax"],bbox_wgs84["ymax"]), 
              datetime = "2021-01-01/2022-12-31") |>
  post_request() |> items_fetch(progress = FALSE)

length(items$features)
#in this search there are 149 features


#list the names of all bands to be included in the collection by adding the assets_names argument

assets = c("B01","B02","B03","B04","B05","B06", "B07","B08","B8A","B09","B11","SCL")
s2_collection = stac_image_collection(items$features, 
                                      asset_names = assets, property_filter =
                                        function(x) {x[["eo:cloud_cover"]] < 20})
s2_collection

#set the extent of sentinel 2 collection, and then create a composite of those images
e <- extent(s2_collection) 

gdalcubes_options(parallel = parallel::detectCores()-4)

#view the image - this is without any cloud masking
v = cube_view(srs="EPSG:32749", dx=10, dy=10, dt="P1D", 
              aggregation="median", resampling = "bilinear",
              extent=
                list(t0 = "2021-02-27", t1 = "2021-02-27",
                     left=bbox["xmin"], right=bbox["xmax"],
                     top=bbox["ymax"], bottom=bbox["ymin"]))

#start cloud masking
S2.mask <- gdalcubes::image_mask(
  "SCL",
  values = c(
    0, # NO_DATA
    1, # SATURATED_OR_DEFECTIVE
    3, # CLOUD_SHADOWS
    8, # CLOUD_MEDIUM_PROBABILITY
    9, # CLOUD_HIGH_PROBABILITY
    10 # THIN_CIRRUS
  )
)


#download the collection, and compute NDVI as well
s2_cube <- raster_cube(s2_collection, v, mask = S2.mask) |> 
  apply_pixel("(B08-B04)/(B08+B04)", "NDVI", names="NDVI", keep_bands=TRUE) 

#view the image after cloud masking 
raster_cube(s2_collection, v, mask = S2.mask) %>%
  select_bands(c("B02","B03","B04")) %>%
  reduce_time(c("median(B02)", "median(B03)", "median(B04)")) %>%
  plot(rgb = 3:1, zlim=c(0,1800)) %>% system.time()

directory <- "/raid/home/bp424/Documents/MTHM603/Data/kat"

#convert to a raster
s2_cube_10m <-
  write_tif(s2_cube,  dir = directory, prefix = "kat_final_S2_") %>% 
  rast()

# Rescale the imagery but not NDVI and SCL
s2_cube_10m[[setdiff(names(s2_cube_10m), c("NDVI", "SCL"))]] <- 
  s2_cube_10m[[setdiff(names(s2_cube_10m), c("NDVI", "SCL"))]]/10000 

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

dtm_S2.10m <- mpc_dtm_src(bbox_wgs84) |> # call the function to get the tile urls
  lapply(rast) |> # iterate over the tiles and load as a spatRaster object
  terra::sprc() |> # convert to a collection
  terra::merge() |> # merge the tiles - basic mosaic - nothing overlaps here so this is fine.
  project(s2_cube_10m) # project into our desired CRS and extent etc.
names(dtm_S2.10m) <- "dtm"

# now generate a few geomorphometric layers
asp_S2.10m <- terra::terrain(dtm_S2.10m, "aspect")
slp_S2.10m <- terrain(dtm_S2.10m, "slope")
TRI_S2.10m <- terrain(dtm_S2.10m, "TRIrmsd")
rough_S2.10m <- terrain(dtm_S2.10m, "roughness")

terrain_s2_10m <- c(dtm_S2.10m,asp_S2.10m,slp_S2.10m, TRI_S2.10m, rough_S2.10m)

# calculate CHM to be included in the table -------------------------------

# Calculate Canopy Height Model  ------------------------------------------
CHM <- dsm - dtm_LiDAR
names(CHM) <- "CHM"


#make sure the CHM has the same projection as the S2 Data
CHM_project_10m <- project(CHM, s2_cube_10m, method="bilinear")

S2.10m <- c(s2_cube_10m, terrain_s2_10m, CHM_project_10m)
print(names(S2.10m))

drop_bands <- c("SCL") # we don't want to model with pixel masks so drop it.
S2.10m <- S2.10m[[setdiff(names(S2.10m), drop_bands)]]

#rename the bands so that they match the PlanetScope names 

print(names(S2.10m))
names(S2.10m[[1]]) <- "coastal_blue"
names(S2.10m[[2]]) <- "blue"
names(S2.10m[[3]]) <- "green"
names(S2.10m[[4]]) <- "red"
names(S2.10m[[5]]) <- "rededge"
names(S2.10m[[6]]) <- "vnir_6"
names(S2.10m[[7]]) <- "vnir_7"
names(S2.10m[[8]]) <- "vnir_8"
names(S2.10m[[9]]) <- "SWIR_9"
names(S2.10m[[10]]) <- "SWIR_11"
names(S2.10m[[11]]) <- "nir"


# do the band math --------------------------------------------------------

# Do the band math - HG examples
S2.10m$NDRE <- (S2.10m$nir - S2.10m$rededge)/(S2.10m$nir +S2.10m$rededge)
S2.10m$EVI <- 2.5 * ((S2.10m$nir) - (S2.10m$red)) /
  ((S2.10m$nir) + 6 * (S2.10m$red) - 7.5 * (S2.10m$blue) + 1)

#Calculations by BP
S2.10m$AVI <- (S2.10m$nir * (1 - S2.10m$red)*(S2.10m$nir - S2.10m$red))^(1/3)
S2.10m$RDVI <- (S2.10m$nir - S2.10m$red)/((S2.10m$nir + S2.10m$red)^0.5)
S2.10m$CIRE <- ((S2.10m$nir/S2.10m$rededge)-1)
S2.10m$GNDVI <- (S2.10m$nir - S2.10m$green)/
  (S2.10m$nir + S2.10m$green)
S2.10m$RGBVI <- (S2.10m$green^2 - S2.10m$blue * S2.10m$red)/
  (S2.10m$green^2 + S2.10m$blue * S2.10m$red)
S2.10m$BNDVI <- (S2.10m$nir - S2.10m$blue)/(S2.10m$nir + S2.10m$blue)
S2.10m$CVI <- (S2.10m$nir * S2.10m$red)/(S2.10m$green^2)
S2.10m$GBNDVI <- (S2.10m$nir - (S2.10m$green + S2.10m$blue))/
  (S2.10m$nir + (S2.10m$green + S2.10m$blue))
S2.10m$GLI <- (2 * S2.10m$green - S2.10m$red - S2.10m$blue)/
  (2 * S2.10m$green + S2.10m$red + S2.10m$blue)
S2.10m$GEMI <- ((2 *((S2.10m$nir^2.0)-(S2.10m$red^2.0)) + 1.5 * 
                   S2.10m$nir + 0.5 * S2.10m$red)/(S2.10m$nir + S2.10m$red + 0.5)) * 
  (1.0 - 0.25 * ((2.0 *((S2.10m$nir^2.0)- (S2.10m$red^2)) + 
                    1.5 * S2.10m$nir + 0.5 * S2.10m$red)/(S2.10m$nir + S2.10m$red + 0.5))) - 
  ((S2.10m$red - 0.125)/(1 - S2.10m$red))


#export as a tiff file: 
print(names(S2.10m))
writeRaster(S2.10m, filename = "/raid/home/bp424/Documents/MTHM603/Data/S2_comb_data.tif")

# this will be our dataframe for the ML section
S2.10m_df <- as.data.frame(S2.10m, xy=TRUE) |> 
  tidyr::drop_na()

S2.10m_df <- S2.10m_df %>%
  relocate(CHM, .after = last_col())

write.csv(S2.10m_df, file = "/raid/home/bp424/Documents/MTHM603/Data/df_S2.10m_final.csv", row.names = FALSE)

