## LOAD THE REQUIRED LIBRARYS
library(raster)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(parallel)

#define directories
work.dir = "/mnt/DataDrive2/data/drought_indices/spi/"
git.dir = '/home/zhoylman/drought_indicators/topofire/R/'
climatology.dir = "/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km"
cpp.dir = "/home/zhoylman/drought_indicators/topofire/cpp/"

write.dir = paste0(work.dir,"current/")
archive.dir = paste0(work.dir,"archive/")
dir.create(write.dir)
dir.create(archive.dir)

source(paste0(git.dir, "gamma_fit.R"))
source(paste0(git.dir, "fdates.R"))
source(paste0(git.dir, "spi_fun.R"))

files = list.files(climatology.dir, pattern = ".tif$", full.names = T)

#compute time from files
time_fdates = fdates(files)
time = data.frame(datetime = as.Date(fdates(files), format = "%Y%m%d"))
time$day = strftime(time$datetime,"%m-%d")

#designate time scale
time_scale = c(30, 60, 90, 180, 360)

for(t in 1:length(time_scale)){
  dir.create(paste0(work.dir, "tmp_dir/"))
  tmp.dir <- paste0(work.dir, "tmp_dir/", time_scale[t], "_days/")
  dir.create(tmp.dir)
  
  #calcualte run time
  tic()
  
  #compute time breaks (indexes)
  first_date_breaks = which(time$day == time$day[length(time$datetime)])
  second_date_breaks = first_date_breaks-(time_scale[t]-1)
  
  #if there are negative indexes remove last year (incomplete data range)
  #change this to remove all indexes from both vectors that are negative
  if(!all(second_date_breaks < 0)){
    pos_index = which(second_date_breaks > 0)
    first_date_breaks = first_date_breaks[c(pos_index)]
    second_date_breaks = second_date_breaks[c(pos_index)]
  }
  
  #create slice vectors and group by vectors
  for(j in 1:length(first_date_breaks)){
    if(j == 1){
      slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
      group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
    }
    else{
      slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
      group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
    }
  }
  
  ########################################
  ###   C++ METOHD FOR SUMMING GRIDS  ####
  ########################################
  
  cl = makeCluster(20)
  registerDoParallel(cl)
  
  #summed_precip_vec = foreach(i=unique(group_by_vec)) %dopar% {
  run = foreach(i=unique(group_by_vec)) %dopar% {
    flist = files[slice_vec[group_by_vec == i]]
    datetime_char = time$datetime[slice_vec[group_by_vec == i]]
    
    txt.filename <- paste0(tmp.dir, "do_sum_group_", datetime_char[1], "_",
                           datetime_char[length(datetime_char)],".txt")
    
    write.table(flist, file=txt.filename, quote=F, row.names=F, col.names=F, append=F)
    out.file <- paste0(tmp.dir, "sum_raster_", datetime_char[1], "_",
                       datetime_char[length(datetime_char)], ".tif")
    
    # call C++ sum program here
    # aruments are: 1. text file which lists geotiffs; 2. name of the output file; 3. NoData value 
   system(paste0("/opt/drought_anomaly/drought_anomaly_sum ", txt.filename, " ", out.file, " ", -9999  ))
  }
  
  #import summed rasters
  summed_rasters = list.files(tmp.dir, pattern = ".tif$", full.names = T)
  summed_raster_stack = stack(summed_rasters)
  
  #reformat data
  summed_precip_vec = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    values(summed_raster_stack[[i]])
  }
  integrated_precip = structure(summed_precip_vec, row.names = c(NA, -length(summed_precip_vec[[1]])), class = "data.frame")
  
  #calculate SPI
  clusterExport(cl, c("gamma_fit", "spi_fun"))
  spi_values = parApply(cl,integrated_precip, 1, FUN = spi_fun)
  stopCluster(cl)
  
  #create spatial template for current spi values
  current_spi = summed_raster_stack[[1]]
  
  #allocate curent spi values to spatial template
  values(current_spi) = spi_values
  
  #write out archive raster
  writeRaster(current_spi, paste0(archive.dir,"spi_",time_fdates[length(time_fdates)],"_", 
                                   as.character(time_scale[t]),"_day" ,".tif"), format = "GTiff", overwrite = T)
  
  #write out raster as "current"
  writeRaster(current_spi, paste0(write.dir,"current_spi_", 
                                   as.character(time_scale[t]),"_day" ,".tif"), format = "GTiff", overwrite = T)
  
  toc()
  do.call(file.remove, list(list.files(tmp.dir, full.names = T)))
  
  print(paste0(as.character(time_scale[t])," day SPI calcualtion complete."))
}
