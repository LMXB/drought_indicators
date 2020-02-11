library(raster)
library(rgdal)

source("/home/zhoylman/drought_indicators/snodas/R/get_snodas.R")
source("/home/zhoylman/drought_indicators/zoran/R/fdates.R")

UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp")

dates = c(as.Date(Sys.Date()):as.Date(Sys.Date()-7)) %>%
  as.Date(., origin = "1970-01-01")

get_snowdas(dates)

files = list.files("/home/zhoylman/drought_indicators/snodas/data/processed/snow_depth/", full.names = T)

time = files %>%
  fdates() %>%
  as.Date(., format = "%Y%m%d")

process_raster = function(date){
  raster_temp = raster(files[which(time == date)]) %>%
    crop(., extent(UMRB)) %>%
    mask(., UMRB) /1000 * 39.3701
  return(raster_temp)
}

today = process_raster(Sys.Date())

delta_1 = process_raster(Sys.Date()) - process_raster(Sys.Date()-1)
delta_3 = process_raster(Sys.Date()) - process_raster(Sys.Date()-3)
delta_7 = process_raster(Sys.Date()) - process_raster(Sys.Date()-7)

writeRaster(today, filename = paste0("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/current_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_1, filename = paste0("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_1_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_3, filename = paste0("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_3_depth_in.tif"), overwrite=TRUE)
writeRaster(delta_7, filename = paste0("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_7_depth_in.tif"),overwrite=TRUE)

do.call(file.remove, list(list.files("/home/zhoylman/drought_indicators/snodas/data/processed/swe", full.names = TRUE)))
do.call(file.remove, list(list.files("/home/zhoylman/drought_indicators/snodas/data/processed/snow_depth", full.names = TRUE)))
