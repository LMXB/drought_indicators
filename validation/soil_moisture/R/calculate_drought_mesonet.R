library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

# query lat long data from sql
# for Zoran run source ~/.bashrc in terminal 
station_data = read.csv("/home/zhoylman/drought_indicators_data/mesonet/station_data_clean.csv")

#define timescales
time_scales = c(seq(5,730,5))

time = list()
site = list()
time_export = list()


################## spei #################

cl = makeCluster(20)
registerDoParallel(cl)

tic()

out_spei = foreach(s = 1:length(station_data$latitude)) %dopar% {
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

mesonet_spei = out_spei


################## spi #################

cl = makeCluster(20)
registerDoParallel(cl)

tic()

out_spi = foreach(s = 1:length(station_data$latitude)) %dopar% {
  source("/home/zhoylman/drought_indicators/spi_app/R/spi_point.R")
  tryCatch({
    time = spi_point(station_data$latitude[s],station_data$longitude[s],time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

mesonet_spi = out_spi

################## eddi #################

cl = makeCluster(20)
registerDoParallel(cl)

tic()

out_eddi = foreach(s = 1:length(station_data$latitude)) %dopar% {
  source("/home/zhoylman/drought_indicators/eddi_app/R/eddi_point.R")
  tryCatch({
    time = eddi_point(station_data$latitude[s],station_data$longitude[s],time_scales)
  }, error = function(e){
    return(NA)
  })
  site[[s]] = time
}

toc()

stopCluster(cl)

mesonet_eddi = out_eddi


save(mesonet_spi, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_spi.RData")
save(mesonet_spei, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_spei.RData")
save(mesonet_eddi, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_eddi.RData")
