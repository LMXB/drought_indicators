#rm(list = ls())

## LOAD THE REQUIRED LIBRARYS
library(ncdf4) # if running on windows, need opendap ncdf4 build https://github.com/pmjherman/r-ncdf4-build-opendap-windows
library(lubridate)
library(dplyr)
library(zoo)
library(plyr)
library(rowr)
library(precintcon)
library(gridExtra)
library(raster)
library(MASS)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(rgeos)
library(stringr)

git.dir = '/home/zhoylman/drought_indicators/precipation/R/'

## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)
proj4string(raster_precip) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/watersheds_umrb.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/county_umrb.shp")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))

time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])

#designate time scale
time_scale = c(15,30,60,90,180,365, water_year, year_to_date)

for(t in 1:length(time_scale)){
  #calcualte time
  tic()
  
  #compute indexes for time breaks
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
  
  #sum and mask precip in parellel
  raster_precip_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }

  #calucalte time integrated precip sum
  integrated_precip = data.frame(matrix(nrow = length(values(raster_precip_clipped[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_precip[,i] = values(raster_precip_clipped[[i]])
  }
  #integrated_precip[integrated_precip == 0] = NA
  
  anomaly = function(x){
    x_mean = mean(x, na.rm = T)
    anomaly = ((x[length(x)])/x_mean)*100
    return(anomaly)
  }
  
  current_anomaly = parApply(cl,integrated_precip, 1, FUN = anomaly)
  
  #stop parellel cluster
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current anomaly values
  anomaly_map = raster_precip_clipped[[1]]

  #allocate curent anomaly values to spatial template
  values(anomaly_map) = current_anomaly
  
  #add metadata for most current time stamp
  metadata(anomaly_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("darkred","red", "white", "blue", "darkblue"))
  
  #plot map
  plot(anomaly_map, col = color_ramp(100), zlim = c(0,200),
       main = paste0("Current ", as.character(time_scale[t]), " Day anomaly"))

  #define path for map export
  path_file = paste("/home/zhoylman/drought_indicators/precipitation/maps/current_anomaly_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      path_file = paste("/home/zhoylman/drought_indicators/precipitation/maps/current_anomaly_",
                        "water_year",".tif", sep = "")
    }
    if(t == (length(time_scale))){
      path_file = paste("/home/zhoylman/drought_indicators/precipitation/maps/current_anomaly_",
                        "year_to_date",".tif", sep = "")
    }
  }
  
  #write GeoTiff
  writeRaster(anomaly_map, path_file, format = "GTiff", overwrite = T)
  
  ############################################
  ################ SHP FILES #################
  ############################################
  
  # Extract raster values for each HUC 
  r.vals <- extract(anomaly_map, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  
  r.median = nullToNA(r.median)
  
  #create export shp file, define most current time and define aggregate values
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  watersheds_export$average = as.vector(unlist(r.median))
  
  #define path to export folder and export
  path_file_watershed = paste("/home/zhoylman/drought_indicators/precipitation/shp/", sep = "")
  layer_name = paste("current_anomaly_watershed_",as.character(time_scale[t]), sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_anomaly_watershed_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_anomaly_watershed_year_to_date", sep = "")
    }
  }
  
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  # Extract raster values for each county 
  r.vals <- extract(anomaly_map, county)
  
  # Use list apply to calculate median for each county
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  r.median = nullToNA(r.median)
  #create export shp file and define aggregate values
  county_export = county
  county_export$average = as.vector(unlist(r.median))
  
  #define path to export folder and export
  path_file_watershed = paste("/home/zhoylman/drought_indicators/precipitation/shp/", sep = "")
  layer_name = paste("current_anomaly_county_",as.character(time_scale[t]), sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_anomaly_county_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_anomaly_county_year_to_date", sep = "")
    }
  }
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)

  #compute run time
  toc()
}
