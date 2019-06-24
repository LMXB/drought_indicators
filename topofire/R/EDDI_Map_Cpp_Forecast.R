## LOAD THE REQUIRED LIBRARYS
library(raster)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(parallel)
library(dplyr)
library(stringr)

#define directories
work.dir = "/mnt/DataDrive2/data/drought_indices/eddi/"
git.dir = '/home/zhoylman/drought_indicators/topofire/R/'
cpp.dir = "/home/zhoylman/drought_indicators/topofire/cpp/"

precip.climatology.dir = "/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km"
precip.forecast.dir = "/mnt/DataDrive2/data/drought_indices/forecast/precip"

pet.climatology.dir = "/mnt/DataDrive2/data/drought_indices/historical_2p5km/PET"
pet.current.dir = "/mnt/DataDrive2/data/airtemp_realtime/water_balance/2pt5km" #also the forecast dir

#create dirs for writing
write.dir = paste0(work.dir,"forecast/")
archive.dir = paste0(work.dir,"forecast_archive/")
dir.create(write.dir)
dir.create(archive.dir)

#fits a gamma distrbution to a vector
#returns the shape and rate parameters
source(paste0(git.dir, "fdates.R"))
source(paste0(git.dir, "eddi_fun.R"))

#parse precip
precip.climatology.files = list.files(precip.climatology.dir, pattern = ".tif$", full.names = T)
precip.forecast.files = list.files(precip.forecast.dir, pattern = ".tif$", full.names = T)
precip.files = c(precip.climatology.files, precip.forecast.files)

#parse pet
pet.files.historical = list.files(pet.climatology.dir, pattern = ".tif$", full.names = T)
pet.files.current = list.files(pet.current.dir, pattern = glob2rx("*et0*.tif$*"), full.names = T)
pet.files.current = pet.files.current[str_detect(pet.files.current,paste(c(seq(2019,2100,1)),collapse = '|'))] #will run till 2100
pet.files.forecast = list.files(pet.current.dir, pattern = glob2rx("*et0*forecast*.tif$*"), full.names = T)

#change file names of forecast grids to play nice with fdates
pet.time.current = as.Date(fdates(pet.files.current), format = "%Y%m%d")
forecast.dates = gsub("[^[:digit:].]", "",  pet.time.current[length(pet.time.current)] + c(1:7))

#delete old forecasts
do.call(file.remove, list(list.files("/mnt/DataDrive2/data/drought_indices/pet_forecast_fdates/",full.names = T)))

#copy pet forecast to temp folder and rename for fdates
file.copy(pet.files.forecast, paste0("/mnt/DataDrive2/data/drought_indices/pet_forecast_fdates/pet_forecast_",forecast.dates,".tif"))
pet.files.forecast = list.files("/mnt/DataDrive2/data/drought_indices/pet_forecast_fdates",full.names = T)

#combine pet
pet.files = c(pet.files.historical, pet.files.current, pet.files.forecast)

#compute time from files
pet.time = fdates(pet.files)
precip.time = fdates(precip.files)

#match time series
pet.files.match = pet.files[pet.time %in% precip.time]
precip.files.match = precip.files[precip.time %in% pet.time]

#compute new time from files
pet.time = fdates(pet.files.match)
precip.time = fdates(precip.files.match)
time_fdates = fdates(precip.files.match)
time = data.frame(datetime = as.Date(fdates(pet.files.match), format = "%Y%m%d"))
time$day = strftime(time$datetime,"%m-%d")

#designate time scale
time_scale = c(30, 60, 90, 180, 360)

for(t in 1:length(time_scale)){
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
  
  cl = makeCluster(20)
  registerDoParallel(cl)
  
  #sum pet grids
  run = foreach(i=unique(group_by_vec)) %dopar% {
    flist = pet.files.match[slice_vec[group_by_vec == i]]
    datetime_char = time$datetime[slice_vec[group_by_vec == i]]
    
    txt.filename <- paste0(tmp.dir.pet, "do_sum_group_", datetime_char[1], "_",
                           datetime_char[length(datetime_char)],".txt")
    
    write.table(flist, file=txt.filename, quote=F, row.names=F, col.names=F, append=F)
    out.file <- paste0(tmp.dir.pet, "sum_raster_", datetime_char[1], "_",
                       datetime_char[length(datetime_char)], ".tif")
    
    # call C++ sum program here
    # aruments are: 1. text file which lists geotiffs; 2. name of the output file; 3. NoData value 
    system(paste0("/opt/drought_anomaly/drought_anomaly_sum ", txt.filename, " ", out.file, " ", -9999  ))
  }
  
  #import summed rasters
  summed_pet_rasters = list.files(tmp.dir.pet, pattern = ".tif$", full.names = T)
  summed_pet_raster_stack = stack(summed_pet_rasters)
  
  #reformat data
  summed_pet_vec = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    values(summed_pet_raster_stack[[i]])
  }
  integrated_pet = structure(summed_pet_vec, row.names = c(NA, -length(summed_pet_vec[[1]])), class = "data.frame")
  
  #calculate EDDI
  clusterExport(cl, c("git.dir"))
  clusterCall(cl, function() {source(paste0(git.dir,"eddi_fun.R"))})
  eddi_values = parApply(cl,integrated_pet, 1, FUN = eddi_fun)
  stopCluster(cl)
  
  #create spatial template for current spi values
  current_eddi = summed_pet_raster_stack[[1]]
  
  #allocate curent spi values to spatial template
  values(current_eddi) = eddi_values
  
  #write out archive raster
  writeRaster(current_eddi, paste0(archive.dir,"forecast_eddi_",time_fdates[length(time_fdates)],"_", 
                                   as.character(time_scale[t]),"_day" ,".tif"), format = "GTiff", overwrite = T)
  
  #write out raster as "current"
  writeRaster(current_eddi, paste0(write.dir,"forecast_eddi_", 
                                   as.character(time_scale[t]),"_day" ,".tif"), format = "GTiff", overwrite = T)
  toc()
  
  #clean up all temp data
  do.call(file.remove, list(list.files(tmp.dir.pet, full.names = T)))  

  print(paste0(as.character(time_scale[t])," day EDDI calcualtion complete."))
}
