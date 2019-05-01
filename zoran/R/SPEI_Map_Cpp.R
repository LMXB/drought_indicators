rm(list = ls())

## LOAD THE REQUIRED LIBRARYS
library(raster)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(parallel)

#define directories
work.dir = "/mnt/ScratchDrive/data/Hoylman/SPEI/"
git.dir = '/home/zhoylman/drought_indicators/zoran/R/'
precip.climatology.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_precip_raw"
pet.climatology.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_pet_raw"

write.dir = paste0(work.dir,"Raw_gridMET_SPEI_test/")
dir.create(write.dir)

#fits a gamma distrbution to a vector
#returns the shape and rate parameters
source(paste0(git.dir, "fdates.R"))
source(paste0(git.dir, "spei_fun.R"))

precip.files = list.files(precip.climatology.dir, pattern = ".tif$", full.names = T)
pet.files = list.files(pet.climatology.dir, pattern = ".tif$", full.names = T)

#compute time from files
time_fdates = fdates(precip.files)
time = data.frame(datetime = as.Date(fdates(precip.files), format = "%Y%m%d"))
time$day = strftime(time$datetime,"%m-%d")

#designate time scale
time_scale = c(30, 60, 90, 180, 360)

for(t in 1:length(time_scale)){
  dir.create(paste0(work.dir, "tmp_dir_precip/"))
  tmp.dir.precip <- paste0(work.dir, "tmp_dir_precip/", time_scale[t], "_days/")
  dir.create(tmp.dir.precip)
  
  dir.create(paste0(work.dir, "tmp_dir_pet/"))
  tmp.dir.pet <- paste0(work.dir, "tmp_dir_pet/", time_scale[t], "_days/")
  dir.create(tmp.dir.pet)
  
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
  
  cl = makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  #sum precip grids
  run = foreach(i=unique(group_by_vec)) %dopar% {
    flist = precip.files[slice_vec[group_by_vec == i]]
    datetime_char = time$datetime[slice_vec[group_by_vec == i]]
    
    txt.filename <- paste0(tmp.dir.precip, "do_sum_group_", datetime_char[1], "_",
                           datetime_char[length(datetime_char)],".txt")
    
    write.table(flist, file=txt.filename, quote=F, row.names=F, col.names=F, append=F)
    out.file <- paste0(tmp.dir.precip, "sum_raster_", datetime_char[1], "_",
                       datetime_char[length(datetime_char)], ".tif")
    
    # call C++ sum program here
    # aruments are: 1. text file which lists geotiffs; 2. name of the output file; 3. NoData value 
    system(paste0("/home/zhoylman/drought_indicators/zoran/cpp/drought_anomaly_sum ", txt.filename, " ", out.file, " ", -9999  ))
  }
  
  #sum pet grids
  run = foreach(i=unique(group_by_vec)) %dopar% {
    flist = pet.files[slice_vec[group_by_vec == i]]
    datetime_char = time$datetime[slice_vec[group_by_vec == i]]
    
    txt.filename <- paste0(tmp.dir.pet, "do_sum_group_", datetime_char[1], "_",
                           datetime_char[length(datetime_char)],".txt")
    
    write.table(flist, file=txt.filename, quote=F, row.names=F, col.names=F, append=F)
    out.file <- paste0(tmp.dir.pet, "sum_raster_", datetime_char[1], "_",
                       datetime_char[length(datetime_char)], ".tif")
    
    # call C++ sum program here
    # aruments are: 1. text file which lists geotiffs; 2. name of the output file; 3. NoData value 
    system(paste0("/home/zhoylman/drought_indicators/zoran/cpp/drought_anomaly_sum ", txt.filename, " ", out.file, " ", -9999  ))
  }
  
  #import summed rasters
  summed_precip_rasters = list.files(tmp.dir.precip, pattern = ".tif$", full.names = T)
  summed_precip_raster_stack = stack(summed_precip_rasters)
  
  #import summed rasters
  summed_pet_rasters = list.files(tmp.dir.pet, pattern = ".tif$", full.names = T)
  summed_pet_raster_stack = stack(summed_pet_rasters)
  
  #reformat data and take differnce
  summed_diff_vec = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = summed_precip_raster_stack[[i]] - summed_pet_raster_stack[[i]]
    values(temp)
  }
  integrated_diff = structure(summed_diff_vec, row.names = c(NA, -length(summed_diff_vec[[1]])), class = "data.frame")
  
  #calculate SPEI
  clusterExport(cl, c("spei_fun"))
  clusterCall(cl, function() {lapply(c("lmomco"), library, character.only = TRUE)})
  spei_values = parApply(cl,integrated_diff, 1, FUN = spei_fun)
  stopCluster(cl)
  
  #create spatial template for current spi values
  current_spei = summed_precip_raster_stack[[1]]
  
  #allocate curent spi values to spatial template
  values(current_spei) = spei_values
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("darkred","red", "white", "blue", "darkblue"))
  
  #plot map
  plot(current_spei, col = color_ramp(100), zlim = c(-3.5,3.5),
       main = paste0("Current ", as.character(time_scale[t]), " Day SPEI"))
  
  #write out raster
  writeRaster(current_spei, paste0(write.dir,"spei_",time_fdates[length(time_fdates)],"_", 
                                   as.character(time_scale[t]),"_day" ,".tif"), format = "GTiff", overwrite = T)
  
  toc()
  do.call(file.remove, list(list.files(tmp.dir.precip, full.names = T)))
  do.call(file.remove, list(list.files(tmp.dir.pet, full.names = T)))
}
