library(raster)
library(tidyverse)

source('/home/zhoylman/drought_indicators/zoran/R/fdates.R')

sites = read_csv('/home/zhoylman/drought_indicators_data/holden_data/site_locations.csv')

load("/home/zhoylman/drought_indicators_data/cpc_soil_moisture/cpc_soil_moisture_brick.RData")

time = cpc_soil_moisture %>%
  names() %>%
  substr(., 2, 9) %>%
  as.Date(., format = "%Y%m%d")

spatial_sites = SpatialPoints(data.frame(x = sites$longitude, y = sites$latitude), proj4string = crs(cpc_soil_moisture[[1]]))

extraction = t(data.frame(raster::extract(x = cpc_soil_moisture, y = spatial_sites))) %>%
  as.data.frame()

extraction$date = time

station_name = stringr::str_replace(sites$station_key, " ", ".") %>%
  stringr::str_replace(., " #", "..")

colnames(extraction) = c(station_name, "date")

write.csv(extraction, "/home/zhoylman/drought_indicators_data/cpc_soil_moisture/cpc_soil_moisture_extraction.csv")
