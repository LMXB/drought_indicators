rm(list = ls())

## LOAD THE REQUIRED LIBRARYS
library(ncdf4) # Downlaoded from https://github.com/pmjherman/r-ncdf4-build-opendap-windows
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

#load in gamma fitting function
source("D:\\Git_Repo\\drought_indicators\\functions\\gamma_fit.R")


## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)

#designate time scale
time_scale = 30

#import montana outline for clipping  
montana = rgdal::readOGR("D:\\Git_Repo\\drought_indicators\\shp_kml\\UMRB_Outline_Conus.shp")
montana = rgdal::readOGR("C:\\Users\\zhoyl\\Documents\\Git_Repo\\drought_indicators\\shp_kml\\montana_outline.kml")

watersheds = rgdal::readOGR("D:\\Git_Repo\\drought_indicators\\shp_kml\\UMRB_Clipped_HUC8.shp")
tic()

#clip precip grids to the extent of montana, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(montana))

#calcualte time
time = data.frame(datetime = as.POSIXct(as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01")))
time$day = yday(time$datetime)

first_date_breaks = which(time$day == time$day[length(time$datetime)])
second_date_breaks = first_date_breaks-(time_scale-1)

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
  #temp = crop(temp, extent(montana))
  mask(temp, montana)
}

#stop parellel cluster
stopCluster(cl)

#calucalte time integrated precip sum
integrated_precip = data.frame(matrix(nrow = length(values(raster_precip_clipped[[1]])), ncol = length(unique(group_by_vec))))
for(i in 1:length(unique(group_by_vec))){
  integrated_precip[,i] = values(raster_precip_clipped[[i]])
}

integrated_precip[integrated_precip == 0] = NA

#spi function
spi_fun <- function(x) { 
  fit.gamma = gamma_fit(x)
  fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
  standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
  return(standard_norm[length(standard_norm)]) 
}

#calcualte current spi
current_spi = apply(integrated_precip, 1, spi_fun)

#create spatial template for current spi values
spi_map = raster_precip_clipped[[1]]

#allocate curent spi values to spatial template
values(spi_map) = current_spi

#compute color ramp for visualization
color_ramp = colorRampPalette(c("red", "white", "blue"))

#plot map
plot(spi_map, col = color_ramp(100), zlim = c(-3.5,3.5))

writeRaster(spi_map, "D:\\temp\\current_spi.tif", format = "GTiff", overwrite = T)

toc()

#calulcate watershed averages

#start cluster for parellel computing
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#sum and mask precip in parellel
watershed_values = foreach(i=1:length(watersheds$HUC8)) %dopar% {
  library(raster)
  median(values(mask(spi_map, (watersheds[watersheds$HUC8[i], ]))), na.rm = T)
}

watersheds$average = as.vector(unlist(watershed_values))
setwd("D:\\temp")
maptools::writeSpatialShape(watersheds, "D:\\temp\\current_spi_watershed")


#stop parellel cluster
stopCluster(cl)

# watershed_values[watershed_values == "NaN"] = NA
# watershed_values = as.vector(unlist(watershed_values))
# 
# watershed_ones = spi_map/spi_map
# 
# raster.list = list()
# 
# for(i in 1:length(watershed_values)){
#   raster.list[[i]] = mask(watershed_ones, (watersheds[watersheds$HUC8[i], ])) * watershed_values[i]
# }
# 
# raster.list$fun <- max
# 
# mos <- do.call(mosaic, raster.list)
# 
# plot(mos, col = color_ramp(100), zlim = c(-3.5,3.5))
# plot(watersheds, bg="transparent", add=TRUE)
# 
# writeRaster(mos, "D:\\temp\\current_spi_watersheds.tif", format = "GTiff", overwrite = T)
# 
# plot(watersheds,col=(c("red", "white", "blue"))[watersheds$mean])
