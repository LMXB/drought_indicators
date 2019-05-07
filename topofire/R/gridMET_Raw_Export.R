rm(list = ls())

library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)

write.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_pet_raw/"

gridMET = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", var = "daily_mean_reference_evapotranspiration_grass")
crs(gridMET) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

time = as.Date(as.numeric(substring(names(gridMET),2)), origin = "1900-01-01")

time_char = as.character(time)
time_char_short = gsub("[^0-9.]", "", time_char) 

tic()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

foreach(i=1:length(time)) %dopar% {
  library(raster)
  writeRaster(gridMET[[i]], paste0(write.dir,"gridMET_", as.character(time_char_short[i]),"_pet.tif"), format = "GTiff", overwrite = T)
}

stopCluster(cl)

toc()
