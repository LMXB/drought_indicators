rm(list = ls())

library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(tictoc)
library(stringr)
library(foreach)
library(doParallel)

#import master grid to warp match
master = raster("/mnt/ScratchDrive/data/Hoylman/SPI/NWS_SPI_Test/nws_current_spi_30.tif")

#define directories
input.dir = '/mnt/ScratchDrive/data/Hoylman/SPI/Raw_gridMET_SPI_test/'
write.dir = "/mnt/ScratchDrive/data/Hoylman/SPI/Raw_gridMET_SPI_test/2.5km_warp/"

git.dir = '/home/zhoylman/drought_indicators/zoran/R/'
work.dir <- "/home/zhoylman/drought_indicators/zoran/R/"

#import file paths
input = list.files(input.dir, pattern = ".tif$", full.names = T)
input_short = list.files(input.dir, pattern = ".tif$", full.names = F)

#import funtion to calculate time
source(paste0(git.dir, "fdates.R"))

#compute time
time = fdates(input)
time_char = as.character(time)

#warp in parallel
tic()

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

projected_raster = foreach(i=1:length(time)) %dopar% {
  library(raster)
  source(paste0(work.dir, "ProjectRaster_function.R"))
  
  input.file <- raster(input[i])
  target.file <- master
  out.file <- paste0(write.dir, "warp_2.5km_",input_short[i])
  
  gdalProjRaster(input.file, target.file, out.file)
  
}

#stop parellel cluster
stopCluster(cl)

toc()

