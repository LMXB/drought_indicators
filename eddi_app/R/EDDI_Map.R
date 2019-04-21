#rm(list = ls())

## LOAD THE REQUIRED LIBRARYS
library(ncdf4) # Downlaoded from https://github.com/pmjherman/r-ncdf4-build-opendap-windows (only for windows)
library(lubridate)
library(dplyr)
library(zoo)
library(rowr)
library(raster)
library(MASS)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(glogis)
library(PearsonDS)
library(gsl)
library(lmomco)

git.dir = '/home/zhoylman/drought_indicators/eddi_app/R/'

#load in gamma fitting function
source(paste0(git.dir, "eddi_fun.R"))

raster_pet = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", var = "daily_mean_reference_evapotranspiration_grass")
#proj4string(raster_pet) = CRS("+init=EPSG:4326")

#designate time scale
time_scale = c(30,60,90,180,300)

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/montana_outline.kml")

#clip precip and PET grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_pet_spatial_clip = crop(raster_pet, extent(UMRB))

for(t in 1:length(time_scale)){
  
  #calcualte time
  tic()
  time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_pet_spatial_clip),2)), origin="1900-01-01"))
  time$day = strftime(time$datetime,"%m-%d")
  
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
  
  #start cluster for parellel computing
  cl = makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  #sum and mask PET in parellel
  raster_pet_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_pet_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }
  
  integrated_pet = data.frame(matrix(nrow = length(values(raster_pet_clipped[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_pet[,i] = values(raster_pet_clipped[[i]])
  }
  
  clusterCall(cl, function() {source("/home/zhoylman/drought_indicators/eddi_app/R/eddi_fun.R")})
  current_eddi = parApply(cl,integrated_pet, 1, FUN = eddi_fun)
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current eddi values
  eddi_map = raster_pet_clipped[[1]]
  
  #allocate curent spi values to spatial template and add metadata of last time stamp used
  values(eddi_map) = current_eddi
  metadata(eddi_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(rev(c("red", "white", "blue")))
  
  #plot map
  plot(eddi_map, col = color_ramp(100), zlim = c(-2.5,2.5), 
       main = paste0("Current ", as.character(time_scale[t]), " Day EDDI"))
  plot(montana, add = T)
  
  #define path to export and wrtie GEOtiff
  path_file = paste("/home/zhoylman/drought_indicators/eddi_app/maps/current_eddi/current_eddi_",
                    as.character(time_scale[t]),".tif", sep = "")
  writeRaster(eddi_map, path_file, format = "GTiff", overwrite = T)
  
  ############################################
  ################ SHP FILES #################
  ############################################
  
  # Extract raster values for each HUC 
  r.vals <- extract(eddi_map, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export and add metadata about last timestamp used
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  
  #assign watershed aggregate values to shps, define path to export and export
  watersheds_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("/home/zhoylman/drought_indicators/eddi_app/shp/current_eddi/", sep = "")
  layer_name = paste("current_eddi_watershed_",as.character(time_scale[t]), sep = "")
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  #extract raster values for each county
  r.vals <- extract(eddi_map, county)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export
  county_export = county
  
  #assign county aggregate values to shps, define path to export and export
  county_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("/home/zhoylman/drought_indicators/eddi_app/shp/current_eddi/", sep = "")
  layer_name = paste("current_eddi_county_",as.character(time_scale[t]), sep = "")
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  toc()
}


