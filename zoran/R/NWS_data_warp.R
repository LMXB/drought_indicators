library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(tictoc)
library(stringr)
library(foreach)
library(doParallel)

#import master grid to warp match
WGS_84_CONUS = raster("/home/zhoylman/drought_indicators/zoran/master_warp_grid/master_grid_clipped_2.5km.tif")

#define directories
git.dir = '/home/zhoylman/drought_indicators/zoran/R/'
NWS.dir = '/mnt/ScratchDrive/data/Hoylman/NWS_data/NWS_historical/'
work.dir <- "/home/zhoylman/drought_indicators/zoran/R/"
write.dir = "/mnt/ScratchDrive/data/Hoylman/NWS_data/NWS_warp/"

#import file paths
NWS = list.files(NWS.dir, pattern = ".tif$", full.names = T)

#import funtion to calculate time
source(paste0(git.dir, "fdates.R"))

#compute time
time = fdates(NWS)
time_char = as.character(time)

#warp in parallel
tic()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

projected_raster = foreach(i=1:length(time)) %dopar% {
  library(raster)
  source(paste0(work.dir, "ProjectRaster_function.R"))
  
  input.file <- raster(NWS[i])
  target.file <- WGS_84_CONUS
  out.file <- paste0(write.dir, "nws_",time_char[i],"_precip_2.5km_warp.tif")
  
  gdalProjRaster(input.file, target.file, out.file)
  
}

#stop parellel cluster
stopCluster(cl)

toc()

