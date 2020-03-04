library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

#define input shp files
snotel = read.csv("~/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")
def_data = read.csv("~/drought_indicators_data/holden_data/def_mm_4km.csv")
station_data = read.csv("~/drought_indicators_data/mesonet/station_data.csv")

#mesonet parse
mesonet_index = which(colnames(def_data) %in% station_data$station_key)
mesonet_def = def_data[,mesonet_index]
mesonet_def$time = as.Date(as.character(def_data$date), format = "%Y%m%d")
mesonet_loop_index = which(colnames(mesonet_def) %in% station_data$station_key)

#snotel parse
`%notin%` <- Negate(`%in%`)
start_index = which(colnames(def_data) %notin% station_data$station_key)
snotel_index = start_index[2:(length(start_index)-1)]
snotel_def = def_data[,snotel_index]
snotel_def$time = as.Date(as.character(def_data$date), format = "%Y%m%d")
snotel_loop_index = which(colnames(snotel_def) %in% colnames(def_data[,snotel_index]))

#define timescales
time_scales = c(seq(5,730,5))

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

out = list()

tic()

out = foreach(s = mesonet_loop_index) %dopar% {
  source("~/drought_indicators/drought_index_functions/R/sedi_point.R")
  tryCatch({
    time = sedi_point(x = mesonet_def[,s],time = mesonet_def$time,time_scale = time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

mesonet_sedi = out

save(mesonet_sedi, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_sedi.RData")


# snotel calc

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

out = list()

tic()

out = foreach(s = snotel_loop_index) %dopar% {
  source("~/drought_indicators/drought_index_functions/R/sedi_point.R")
  tryCatch({
    time = sedi_point(x = snotel_def[,s],time = snotel_def$time,time_scale = time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

snotel_sedi = out

save(snotel_sedi, file = "/home/zhoylman/drought_indicators_data/snotel/snotel_sedi.RData")
