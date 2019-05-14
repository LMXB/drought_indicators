library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#load soil moisture from snotel 
load("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")

#source spei function arguments = Lat, long, timescale
source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")

#define timescales
time_scales = c(seq(10,360,10))

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

tic()

out = foreach(s = 1:length(snotel$lat)) %dopar% {
  source("/home/zhoylman/drought_indicators/spei_app/R/spei_point.R")
  for(i in 1:length(time_scales)){
    time[[i]] = spei_point(snotel$lat[s],snotel$lon[s],time_scales[i])
  } 
  site[[s]] = time
}

toc()

stopCluster(cl)

snotel_spei = out

save(snotel_spei, file = "/home/zhoylman/drought_indicators_data/snotel_spei/snotel_spei.RData")
