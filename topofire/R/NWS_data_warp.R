library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(tictoc)
library(stringr)
library(foreach)
library(doParallel)

#import master grid to warp match
WGS_84_CONUS = raster("/mnt/ScratchDrive/data/Hoylman/SPI/NWS_SPI_Test/nws_current_spi_30.tif")

#define directories
git.dir = '/home/zhoylman/drought_indicators/zoran/R/'
NWS.dir = '/mnt/ScratchDrive/data/Hoylman/SPI/Raw_gridMET_SPI_test/'
work.dir <- "/home/zhoylman/drought_indicators/zoran/R/"
write.dir = "/mnt/ScratchDrive/data/Hoylman/SPI/Raw_gridMET_SPI_test/2.5km_warp/"

#import file paths
NWS = list.files(NWS.dir, pattern = ".tif$", full.names = T)
NWS_short = list.files(NWS.dir, pattern = ".tif$", full.names = F)

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
  #NAvalue(input.file) <- -9999
  #convert to mm
  #input.file = input.file * 25.4
  
  target.file <- WGS_84_CONUS
  out.file <- paste0(write.dir, "warp_2.5km_",NWS_short[i])
  
  gdalProjRaster(input.file, target.file, out.file)
  
}

#stop parellel cluster
stopCluster(cl)

toc()

