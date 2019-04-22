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

git.dir = '/home/zhoylman/drought_indicators/spi_app/R/'

#load in gamma fitting function
source(paste0(git.dir,"gamma_fit.R"))
source(paste0(git.dir, "spi_fun.R"))

## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)
#proj4string(raster_precip) = CRS("+init=EPSG:4326")

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/montana_outline.kml")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))

time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])

#designate time scale
time_scale = c(30,60,90,180,365, water_year, year_to_date)

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
  color_ramp = colorRampPalette(c("darkred","red", "white", "blue", "darkblue"))
  
  #plot map
  plot(spi_map, col = color_ramp(100), zlim = c(-3.5,3.5),
       main = paste0("Current ", as.character(time_scale[t]), " Day SPI"))
  plot(montana, add = T)
  
  #define path for map export
  path_file = paste("/home/zhoylman/drought_indicators/spi_app/maps/current_spi/current_spi_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      path_file = paste("/home/zhoylman/drought_indicators/spi_app/maps/current_spi/current_spi_",
                        "water_year",".tif", sep = "")
    }
    if(t == (length(time_scale))){
      path_file = paste("/home/zhoylman/drought_indicators/spi_app/maps/current_spi/current_spi_",
                        "year_to_date",".tif", sep = "")
    }
  }
  
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
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_spi_watershed_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_spi_watershed_year_to_date", sep = "")
    }
  }
  
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
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_spi_county_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_spi_county_year_to_date", sep = "")
    }
  }
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)

  #compute run time
  toc()
}

#download current USDM
#Function to extract date of USDM
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\d*")
}

#download USDM shp file
dlshape=function(shploc) {
  temp=tempfile()
  download.file(shploc, temp)
  files = unzip(temp)
  index = grep(".shp$", files)
  name = files[index]
  date_usdm = numextract(name)
  usdm = rgdal::readOGR(files[index])
  unlink(temp)
  usdm_crop = raster::intersect(usdm,UMRB)
  export = list()
  export[[1]] = usdm_crop
  export[[2]] = date_usdm
  return(export)
}

#Download
usdm = dlshape(shploc = "https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip")

#Export
path_file_usdm = paste("/home/zhoylman/drought_indicators/USDM_current/", sep = "")
rgdal::writeOGR(obj=usdm[[1]], dsn=path_file_usdm, layer = "current_usdm", driver="ESRI Shapefile", overwrite_layer = T)

#w
write.csv(usdm[[2]], paste0(path_file_usdm, "usdm_time.csv"))
