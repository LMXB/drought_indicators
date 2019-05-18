#ingest forecast grid, and reporject
library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(foreach)
library(doParallel)

#set climatology dir and git dirs
git.dir = "/home/zhoylman/drought_indicators/topofire/R/"
climatology.dir = "/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km/"
forecast.dir = "/mnt/DataDrive1/data/WPC_QPF_7day/conus/"
write.forecast.dir = "/mnt/DataDrive2/data/drought_indices/forecast/precip/"

source(paste0(git.dir, "fdates.R"))

#remove old forecast grids
file.remove(list.files(write.forecast.dir, full.names = T))

#import master grid (needed for gridmet update function)
master_grid = raster("/home/zhoylman/drought_indicators/topofire/master_warp_grid/master_grid_clipped_2.5km.tif")

#import forecast and climatology file names
forecast.files = list.files(forecast.dir, pattern = c("APCP" , ".tif$"), full.names = T)
climatology.files = list.files(climatology.dir, pattern = ".tif$", full.names = T)

#compute time
forecast.dates = fdates(forecast.files)
climatology.dates = fdates(climatology.files)

#select forecasts that are not represented by maca data
valid.forecast.files = forecast.files[(which(forecast.dates == climatology.dates[length(climatology.dates)])+1):length(forecast.dates)]
valid.forecast.dates = forecast.dates[(which(forecast.dates == climatology.dates[length(climatology.dates)])+1):length(forecast.dates)]

#only going to have 8 files to write each day
cl = makeCluster(8)
registerDoParallel(cl)
    
#sum and mask precip in parellel
projected_raster = foreach(i=1:length(valid.forecast.files)) %dopar% {
	library(raster)
	source(paste0(git.dir, "ProjectRaster_function.R"))
			      
	input.file <- raster(valid.forecast.files[i])
	target.file <- master_grid
	out.file <- paste0(write.forecast.dir, "gridMET_",valid.forecast.dates[i],"_precip_2.5km_warp.tif")
	gdalProjRaster(input.file, target.file, out.file)	      
}
    
#stop parellel cluster
stopCluster(cl)
