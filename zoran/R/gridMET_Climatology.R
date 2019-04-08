library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(tictoc)
library(foreach)
library(doParallel)

setwd("/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/")

WGS_84 = raster("/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/target_grids/APCP24_20190314_conus_WGS84.tif")
WGS_84_CONUS = raster("/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/target_grids/master_grid_clipped_2.5km.tif")

gridMET = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var = "precipitation_amount")

writeRaster(gridMET[[14000]], "/mnt/ScratchDrive/data/Hoylman/R/gridMET_14000.tif", format = "GTiff")

time = as.Date(as.numeric(substring(names(gridMET),2)), origin = "1900-01-01")


# reproject one MACA geotiff to match the NWS precip grids. 

work.dir <- "/mnt/ScratchDrive/data/Hoylman/R/"
write.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_precip_2.5km_warp_CONUS/"

source(paste0(work.dir, "ProjectRaster_function.R"))

tic()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#sum and mask precip in parellel
projected_raster = foreach(i=1:length(time)) %dopar% {
  library(raster)
  source(paste0(work.dir, "ProjectRaster_function.R"))
  
  input.file <- gridMET[[i]]
  target.file <- WGS_84_CONUS
  out.file <- paste0(write.dir, time[i],"_precip_2.5km_warp_clip.tif")
  
  gdalProjRaster(input.file, target.file, out.file)
  
}

#stop parellel cluster
stopCluster(cl)

toc()

test = raster(paste0(write.dir, time[length(time)],"_precip_2.5km_warp_clip.tif"))
names(test)
plot(test)
