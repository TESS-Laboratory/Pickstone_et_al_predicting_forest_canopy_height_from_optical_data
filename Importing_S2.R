
# load in required packages -----------------------------------------------
library(rstac)
library(sf)
library(gdalcubes)

# load in the datafile ----------------------------------------------------
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"
lidar_cube <- rast(file.path(data_path,"lidar_cube.tif"))

#create bounding box of the area interested in for the sen-2 timages
#transform it to epsg 4326
lidar_cube
bbox <- st_bbox(lidar_cube) 
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
              datetime = "2019-01-01/2023-06-30") |>
  post_request() |> items_fetch(progress = FALSE)

length(items$features)
#in this search there are 73 features


#list the names of all bands to be included in the collection by adding the assets_names argument

assets = c("B01","B02","B03","B04","B05","B06", "B07","B08","B8A","B09","B11","SCL")
s2_collection = stac_image_collection(items$features, 
                                      asset_names = assets, property_filter =
                                        function(x) {x[["eo:cloud_cover"]] < 10})
s2_collection

#set the extent of sentinel 2 collection, and then create a composite of those images
e <- extent(s2_collection) 
e 


gdalcubes_options(parallel = parallel::detectCores()-4)

#view the image - this is without any cloud masking
v = cube_view(srs="EPSG:32749", dx=30, dy=30, dt="P1D", 
              aggregation="median", resampling = "bilinear",
              extent=
                list(t0 = "2022-08-26", t1 = "2022-08-26",
                     left=bbox["xmin"], right=bbox["xmax"],
                     top=bbox["ymax"], bottom=bbox["ymin"]))
v

#start cloud masking

S2.mask = image_mask("SCL", values=c(3,8,9)) # clouds and cloud shadows


#download the collection, and compute NDVI as well
cube <- raster_cube(s2_collection, v, mask = S2.mask) |> 
  apply_pixel("(B08-B04)/(B08+B04)", "NDVI", names="NDVI", keep_bands=TRUE) 

cube
directory <-  "/raid/home/bp424/Documents/MTHM603/Data/Data_out"

#convert to a raster
s2_cube <-
  write_tif(cube,  dir = directory, prefix = "kat_example_") %>% 
  rast()

s2_cube

# Rescale the imagery but not NDVI

s2_cube[[setdiff(names(s2_cube), c("NDVI", "SCL"))]] <- 
  s2_cube[[setdiff(names(s2_cube), c("NDVI", "SCL"))]]/10000 

s2_cube

#view the image before cloud masking
raster_cube(s2_collection, v) |>
  select_bands(c("B02","B03","B04")) |>
  reduce_time(c("median(B02)", "median(B03)", "median(B04)")) |>
  plot(rgb = 3:1, zlim = c(0,2500))

#view the image after cloud masking 
raster_cube(s2_collection, v, mask = S2.mask) %>%
  select_bands(c("B02","B03","B04")) %>%
  reduce_time(c("median(B02)", "median(B03)", "median(B04)")) %>%
  plot(rgb = 3:1, zlim=c(0,1800)) %>% system.time()

