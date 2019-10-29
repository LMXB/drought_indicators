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
  
  percentile_inverse = function(x){
    tryCatch({
      temp_cdf = ecdf(x) 
      cdf = temp_cdf(x)
      return(cdf[length(cdf)]*100)
    }, error = function(e) {
      return(NA)
    })
  }
  
  raw_amount = function(x){
    return(x[length(x)]/25.4)
  }
  
  current_anomaly = parApply(cl,integrated_precip, 1, FUN = anomaly)
  current_percentile = parApply(cl,integrated_precip, 1, FUN = percentile_inverse)
  current_raw = parApply(cl,integrated_precip, 1, FUN = raw_amount)
  
  #stop parellel cluster
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current anomaly values
  anomaly_map = raster_precip_clipped[[1]]
  percentile_map = raster_precip_clipped[[1]]
  raw_map = raster_precip_clipped[[1]]

  #allocate curent anomaly values to spatial template
  values(anomaly_map) = current_anomaly
  values(percentile_map) = current_percentile
  values(raw_map) = current_raw
  
  #add metadata for most current time stamp
  metadata(anomaly_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  metadata(percentile_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  metadata(raw_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("darkred","red", "white", "blue", "darkblue"))
  
  #plot map
  plot(anomaly_map, col = color_ramp(100), zlim = c(0,200),
       main = paste0("Current ", as.character(time_scale[t]), " Day anomaly"))

  vars = c("current_anomaly_", "current_percentile_", "current_raw_")
  
  path_file = list()
  
  for(v in 1:length(vars)){
    #define path for map export
    path_file[[v]] = paste("/home/zhoylman/drought_indicators/precipitation/maps/",vars[v],
                      as.character(time_scale[t]),".tif", sep = "")
    
    # wateryear and year to date file name
    if(t > (length(time_scale)-2)){
      if(t == (length(time_scale)-1)){
        path_file[[v]] = paste("/home/zhoylman/drought_indicators/precipitation/maps/",vars[v],
                          "water_year",".tif", sep = "")
      }
      if(t == (length(time_scale))){
        path_file[[v]] = paste("/home/zhoylman/drought_indicators/precipitation/maps/",vars[v],
                          "year_to_date",".tif", sep = "")
      }
    }
  }
  
  #write GeoTiff
  maps = list(anomaly_map, percentile_map, raw_map)
  for(i in 1:3){
    writeRaster(maps[[i]], path_file[[i]], format = "GTiff", overwrite = T)
  }
  
  ############################################
  ################ SHP FILES #################
  ############################################
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  
  extract_shp_data = function(map, shp){
    # Extract raster values for each shp 
    shp.vals = extract(map, shp)
    # Use list apply to calculate median for each polygon
    r.median = lapply(shp.vals, FUN=median, na.rm=TRUE) %>%
      nullToNA()
    return(as.vector(unlist(r.median)))
  }
  
  #create export shp file, define most current time and define aggregate values
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  watersheds_export$anomaly = extract_shp_data(anomaly_map, watersheds)
  watersheds_export$percentile = extract_shp_data(percentile_map, watersheds)
  watersheds_export$raw = extract_shp_data(raw_map, watersheds)
  
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
  county_export = county
  county_export$anomaly = extract_shp_data(anomaly_map, county)
  county_export$percentile = extract_shp_data(percentile_map, county)
  county_export$raw = extract_shp_data(raw_map, county)
  
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
