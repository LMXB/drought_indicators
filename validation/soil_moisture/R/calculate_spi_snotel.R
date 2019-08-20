library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

#define input shp files
snotel = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")

#source spi function arguments = Lat, long, timescale
source("/home/zhoylman/drought_indicators/spi_app/R/spi_point.R")

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
  source("/home/zhoylman/drought_indicators/spi_app/R/spi_point.R")
  tryCatch({
    time = spi_point(snotel$latitude[s],snotel$longitude[s],time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

snotel_spi = out

save(snotel_spi, file = "/home/zhoylman/drought_indicators_data/snotel/snotel_spi.RData")
