library(raster)
library(tidyverse)

source('/home/zhoylman/drought_indicators/zoran/R/fdates.R')

sites = read_csv('/home/zhoylman/drought_indicators_data/holden_data/site_locations.csv')

topofire_sm = list.files("/mnt/ScratchDrive/data/holden_hoylman_shared/topofire_soilmoisture/resample_4km/", full.names = T) %>%
  lapply(., raster)

topofire_sm = stack(topofire_sm)

time = list.files("/mnt/ScratchDrive/data/holden_hoylman_shared/topofire_soilmoisture/resample_4km/", full.names = T) %>%
  fdates()

spatial_sites = SpatialPoints(data.frame(x = sites$longitude, y = sites$latitude), proj4string = crs(topofire_sm[[1]]))

extraction = t(data.frame(raster::extract(x = topofire_sm, y = spatial_sites))) %>%
  as.data.frame()

extraction$date = time

station_name = stringr::str_replace(sites$station_key, " ", ".") %>%
  stringr::str_replace(., " #", "..")

colnames(extraction) = c(station_name, "date")

write.csv(extraction, "/home/zhoylman/drought_indicators_data/holden_data/topofire_soilmoisture_4km.csv")
