library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

#define input shp files
snotel = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")

#source eddi function arguments = Lat, long, timescale
source("/home/zhoylman/drought_indicators/eddi_app/R/eddi_point.R")

#define timescales
time_scales = c(seq(5,730,5))

time = list()
site = list()
time_export = list()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

out = list()

tic()

out = foreach(s = 1:length(snotel$site_num_id)) %dopar% {
  source("/home/zhoylman/drought_indicators/eddi_app/R/eddi_point.R")
  tryCatch({
    time = eddi_point(snotel$latitude[s],snotel$longitude[s],time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

snotel_eddi = out

save(snotel_eddi, file = "/home/zhoylman/drought_indicators_data/snotel/snotel_eddi.RData")
