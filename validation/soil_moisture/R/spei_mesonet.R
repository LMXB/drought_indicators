library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

# query lat long data from sql
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/get_mesonet_station_info.R")

#source spei function: arguments = Lat, long, timescale
source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")

#define timescales
time_scales = c(seq(10,360,10))

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

tic()

out = foreach(s = 1:length(station_data$latitude)) %dopar% {
  source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")
  for(i in 1:length(time_scales)){
    time[[i]] = spei_point(station_data$latitude[s],station_data$longitude[s],time_scales[i])
  } 
  site[[s]] = time
}

toc()

stopCluster(cl)

mesonet_spei = out

save(mesonet_spei, file = "/home/zhoylman/drought_indicators_data/mesonet_spei/mesonet_spei.RData")
