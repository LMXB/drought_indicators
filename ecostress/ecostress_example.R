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

git.dir = '/home/zhoylman/drought_indicators/spi_app/R/'
write.dir = '/home/zhoylman/drought_indicators/ecostress/spi/'

#load in gamma fitting function
source(paste0(git.dir,"gamma_fit.R"))
source(paste0(git.dir, "spi_fun.R"))

## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)
#proj4string(raster_precip) = CRS("+init=EPSG:4326")

#designate time scale
time_scale = c(30,60,90,180,360)

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/montana_outline.kml")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))

#compute time
time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

for(t in 1:length(time_scale)){
  #calcualte time
  tic()
  
  #compute indexes for time breaks
  target_day = which(time$datetime == "2018-08-29")
  
  first_date_breaks = which(time$day == time$day[target_day])
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
  clusterExport(cl, c("gamma_fit", "spi_fun"))
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
  plot(spi_map, col = color_ramp(100), zlim = c(-4.5,4.5),
       main = paste0("Current ", as.character(time_scale[t]), " Day SPI"))
  plot(montana, add = T)
  
  #define path for map export
  path_file = paste0(write.dir,"spi_",as.character(time_scale[t]),"_day_",
                     time$day[second_date_breaks[1]],"_" ,time$day[first_date_breaks[1]],".tif", sep = "")
  
  #write GeoTiff
  writeRaster(spi_map, path_file, format = "GTiff", overwrite = T)
  
  ############################################
  ################ SHP FILES #################
  ############################################
  
  # # Extract raster values for each HUC 
  # r.vals <- extract(spi_map, watersheds)
  # 
  # # Use list apply to calculate median for each polygon
  # r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  # 
  # #create export shp file, define most current time and define aggregate values
  # watersheds_export = watersheds
  # watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  # watersheds_export$average = as.vector(unlist(r.median))
  # 
  # #define path to export folder and export
  # path_file_watershed = paste("/home/zhoylman/drought_indicators/spi_app/shp/current_spi/", sep = "")
  # layer_name = paste("current_spi_watershed_",as.character(time_scale[t]), sep = "")
  # rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  # 
  # # Extract raster values for each county 
  # r.vals <- extract(spi_map, county)
  # 
  # # Use list apply to calculate median for each county
  # r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  # 
  # #create export shp file and define aggregate values
  # county_export = county
  # county_export$average = as.vector(unlist(r.median))
  # 
  # #define path to export folder and export
  # path_file_watershed = paste("/home/zhoylman/drought_indicators/spi_app/shp/current_spi/", sep = "")
  # layer_name = paste("current_spi_county_",as.character(time_scale[t]), sep = "")
  # rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  # 
  #compute run time
  toc()
}

spi_files = list.files(write.dir)
spi_maps_path = list()
for(i in 1:length(spi_files)){
  spi_maps_path[[i]] = paste0(write.dir,spi_files[i])
}
spi_maps = lapply(spi_maps_path,raster)

order = c(2,4,5,1,3)

par(mfrow = c(3,2))
png(paste0('/home/zhoylman/drought_indicators/ecostress/',"spi_fig_08-01-2018.png"), width = 500, height = 800)

for(i in 1:5){
  plot(spi_maps[[order[i]]], col = color_ramp(100), zlim = c(-3.5,3.5),
       main = paste0(as.character(time_scale[i]), " Day SPI"))
  plot(montana, add = T)
}

dev.off()

rasterplot = function(raster,name){
  raster_points = as.data.frame(rasterToPoints(raster))
  colnames(raster_points) = c("x","y","SPI")
  
  pal = c("darkred","red","white","blue","darkblue")
  
  plot = ggplot(data = raster_points, aes(x = x, y = y))+
    geom_raster(aes(fill = SPI))+
    geom_path(data = fortify(montana), aes(x = long, y = lat, group = group))+
    geom_path(data = fortify(UMRB), aes(x = long, y = lat, group = group))+
    xlab("Longitude")+
    ylab("Latitude")+
    scale_fill_gradientn(colours=c("darkred", "red", "white", "blue", "darkblue"), limits = c(-3.5,3.5))+
    ggtitle(name)+
    theme_bw()
  
  return(plot)
  
}

plot1 = rasterplot(spi_maps[[4]],"August 1st, 2018 | 60 day SPI")
plot2 = rasterplot(spi_maps[[5]],"August 1st, 2018 | 90 day SPI")
plot3 = rasterplot(spi_maps[[1]],"August 1st, 2018 | 180 day SPI")

img <- image_read(system.file("img", "/home/zhoylman/Downloads/drought_201808_map.jpg", package="png"))
g <- rasterGrob(img, interpolate=TRUE)

full = ggpubr::ggarrange(plot1,plot1,plot1,plot2,ncol =2, nrow=2)

ggsave(paste0("/home/zhoylman/drought_indicators/ecostress/", "spi.png"), plot = full, width = 9, height = 6, units = "in", dpi = 600)
