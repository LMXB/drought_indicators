library(raster)
library(tidyverse)

source('/home/zhoylman/drought_indicators/zoran/R/fdates.R')

sites = read_csv('/home/zhoylman/drought_indicators_data/holden_data/site_locations.csv')

deficit = list.files("/mnt/ScratchDrive/data/Hoylman/deficit/deficit_4km/", full.names = T) %>%
  lapply(., raster)

deficit = stack(deficit)

time = list.files("/mnt/ScratchDrive/data/Hoylman/deficit/deficit_4km/", full.names = T) %>%
  fdates()

spatial_sites = SpatialPoints(data.frame(x = sites$longitude, y = sites$latitude), proj4string = crs(deficit[[1]]))

extraction = t(data.frame(raster::extract(x = deficit, y = spatial_sites)))

extraction_df = extraction

write.csv(extraction_df, "/home/zhoylman/drought_indicators_data/holden_data/test.csv")

extraction_df = read_csv("/home/zhoylman/drought_indicators_data/holden_data/test.csv")
extraction_df$date = time
station_name = stringr::str_replace(sites$station_key, " ", ".") %>%
  stringr::str_replace(., " #", "..")

extraction_df[,1] = NULL

colnames(extraction_df) = c(station_name, "date")

write.csv(extraction_df, "/home/zhoylman/drought_indicators_data/holden_data/def_mm_4km.csv")
