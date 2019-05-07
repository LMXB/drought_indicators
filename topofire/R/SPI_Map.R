rm(list = ls())

## LOAD THE REQUIRED LIBRARYS
library(ncdf4)
library(dplyr)
library(zoo)
library(rowr)
library(gridExtra)
library(raster)
library(MASS)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(parallel)
library(spdplyr)

#define directories
climatology.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_precip_2.5km_warp_CONUS/"
work.dir = "/mnt/ScratchDrive/data/Hoylman/SPI/"
write.dir = "/mnt/ScratchDrive/data/Hoylman/SPI/SPI_Map_Output/"

#fits a gamma distrbution to a vector
#returns the shape and rate parameters
source(paste0(work.dir, "gamma_fit.R"))

#spi function
spi_fun = function(x) { 
  fit.gamma = gamma_fit(x)
  fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
  standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
  return(standard_norm[length(standard_norm)]) 
}

load(paste0(climatology.dir,"gridMET_climatology.RData"))

#designate time scale
time_scale = c(364)#30,60,90,180,360)

for(t in 1:length(time_scale)){
  #calcualte run time
  tic()
  
  #compute time from file names
  time = data.frame(datetime = as.Date(substr(unlist(lapply(gridMET_climatology,names)),start = 2, stop = 11), format = "%Y.%m.%d"))
  time$day = strftime(time$datetime,"%m-%d")
  
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
  ### RASTER METOHD FOR SUMMING GRIDS ####
  ########################################
  
  #start cluster for parellel computing
  #sum and mask precip in parellel
  # raster_precip_clipped = foreach(i=unique(group_by_vec)) %dopar% {
  #   library(raster)
  #   temp = stack(gridMET_climatology[slice_vec[group_by_vec == i]])
  #   temp = sum(temp)
  # }
  
  
# lets vectorize the data and then use dplyr to do the summing (dplyr uses C++)
  cl = makeCluster(detectCores()-10)
  registerDoParallel(cl)
  
  ########################################
  ###  DPLYR METOHD FOR SUMMING GRIDS ####
  ########################################
  
  #summed_precip_vec = foreach(i=unique(group_by_vec)) %dopar% {
  summed_precip_vec = foreach(i=1) %dopar% {
    library(raster)
    library(dplyr)
    library(bigmemory)
    temp = stack(gridMET_climatology[slice_vec[group_by_vec == i]])
    temp_vec = as.data.frame(values(temp))
    sum_vec = temp_vec %>%
      mutate(sumVar = rowSums(.[1:ncol(temp_vec)])) %>%
      .$sumVar %>% data.frame()
    
    #clean up memory
    rm(temp_vec)
    gc()
    
    #return the summed vec at the end of the foreach
    sum_vec
  }

  stopCluster(cl)

  #convert large list to df for easy handling
  integrated_precip = structure(summed_precip_vec, row.names = c(NA, -length(summed_precip_vec[[1]]$.)), class = "data.frame")
  colnames(integrated_precip) = paste0("group_",unique(group_by_vec))
  
  cl = makeCluster(detectCores()-1)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  #36 seconds in parallel
  spi_values = parApply(cl,integrated_precip, 1, FUN = spi_fun)
  stopCluster(cl)
  
  #create spatial template for current spi values
  current_spi = gridMET_climatology[[1]]
  
  #allocate curent spi values to spatial template
  values(current_spi) = spi_values
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("red", "white", "blue"))
  
  #plot map
  plot(current_spi, col = color_ramp(11), zlim = c(-3.5,3.5),
       main = paste0("Current ", as.character(time_scale[t]), " Day SPI"))
  
  #write out raster
  writeRaster(current_spi, paste0(write.dir,"current_spi_", as.character(time_scale[t]),".tif"), format = "GTiff", overwrite = T)
  
  toc()
}

