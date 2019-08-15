library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

# query lat long data from sql
# for Zoran run source ~/.bashrc in terminal 
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/get_mesonet_station_info.R")

#source spei function: arguments = Lat, long, timescale
source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")

#import mesonet soil moisture data to filter stations with soil moisture data. 
load("/home/zhoylman/drought_indicators_data/mesonet/mesonet_soil_moisture.RData")
mesonet_soil_moisture = mesonet_soil_moisture[order(mesonet_soil_moisture$station_key, mesonet_soil_moisture$datetime),]

#find site names with valid soil moisture data
valid_stations = unique(mesonet_soil_moisture$station_key)

#filter stations info
station_data = station_data[(station_data$station_key %in% valid_stations),]

#define timescales
time_scales = c(seq(5,360,5))

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

tic()

out = foreach(s = 1:length(station_data$latitude)) %dopar% {
  source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")
  tryCatch({
    time = spei_point(station_data$latitude[s],station_data$longitude[s],time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

mesonet_spei = out

save(mesonet_spei, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_spei.RData")
