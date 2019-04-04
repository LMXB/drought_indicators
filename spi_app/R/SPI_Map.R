rm(list = ls())

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

#load in gamma fitting function
source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")

## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)
proj4string(raster_precip) = CRS("+init=EPSG:4326")

#designate time scale
time_scale = c(30,60,90,180,300)

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/montana_outline.kml")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))

for(t in 1:length(time_scale)){
  #calcualte time
  tic()
  time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
  time$day = strftime(time$datetime,"%m-%d")
  
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
  integrated_precip[integrated_precip == 0] = NA
  
  #define spi function
  spi_fun <- function(x) { 
    fit.gamma = gamma_fit(x)
    fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
    standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
    return(standard_norm[length(standard_norm)]) 
  }
  
  #calcualte current spi in parellel
  clusterExport(cl, "gamma_fit")
  current_spi = parApply(cl,integrated_precip, 1, FUN = spi_fun)
  
  #stop parellel cluster
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current spi values
  spi_map = raster_precip_clipped[[1]]

  #allocate curent spi values to spatial template
  values(spi_map) = current_spi
  
  #add metadata for most current time stamp
  metadata(spi_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("red", "white", "blue"))
  
  #plot map
  plot(spi_map, col = color_ramp(11), zlim = c(-3.5,3.5),
       main = paste0("Current ", as.character(time_scale[t]), " Day SPI"))
  plot(montana, add = T)
  
  #define path for map export
  path_file = paste("/home/zhoylman/drought_indicators/spi_app/maps/current_spi/current_spi_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  #write GeoTiff
  writeRaster(spi_map, path_file, format = "GTiff", overwrite = T)
  
  ############################################
  ################ SHP FILES #################
  ############################################
  
  # Extract raster values for each HUC 
  r.vals <- extract(spi_map, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create export shp file, define most current time and define aggregate values
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  watersheds_export$average = as.vector(unlist(r.median))
  
  #define path to export folder and export
  path_file_watershed = paste("/home/zhoylman/drought_indicators/spi_app/shp/current_spi/", sep = "")
  layer_name = paste("current_spi_watershed_",as.character(time_scale[t]), sep = "")
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  # Extract raster values for each county 
  r.vals <- extract(spi_map, county)
  
  # Use list apply to calculate median for each county
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create export shp file and define aggregate values
  county_export = county
  county_export$average = as.vector(unlist(r.median))
  
  #define path to export folder and export
  path_file_watershed = paste("/home/zhoylman/drought_indicators/spi_app/shp/current_spi/", sep = "")
  layer_name = paste("current_spi_county_",as.character(time_scale[t]), sep = "")
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)

  #compute run time
  toc()
}
