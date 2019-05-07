library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(stringr)
library(foreach)
library(doParallel)

setwd("/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km/")

WGS_84_CONUS = raster("/home/zhoylman/drought_indicators/zoran/master_warp_grid/master_grid_clipped_2.5km.tif")

#Precip NetCDF
gridMET = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var = "precipitation_amount")

#PET NetCDF
#gridMET = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", #var = "daily_mean_reference_evapotranspiration_grass")

time = as.Date(as.numeric(substring(names(gridMET),2)), origin = "1900-01-01")

# reproject one MACA geotiff to match the NWS precip grids. 

work.dir <- "/home/zhoylman/drought_indicators/zoran/R/"
write.dir = "/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km/"

source(paste0(work.dir, "ProjectRaster_function.R"))

time_char = as.character(time)
time_char_short = gsub("[^0-9.]", "", time_char) 

cl = makeCluster(30)
registerDoParallel(cl)

#sum and mask precip in parellel
projected_raster = foreach(i=1:length(time)) %dopar% {
  library(raster)
  source(paste0(work.dir, "ProjectRaster_function.R"))
  
  input.file <- gridMET[[i]]
  target.file <- WGS_84_CONUS
  out.file <- paste0(write.dir, "gridMET_",time_char_short[i],"_precip_2.5km_warp.tif")
  
  gdalProjRaster(input.file, target.file, out.file)
  
}

#stop parellel cluster
stopCluster(cl)

