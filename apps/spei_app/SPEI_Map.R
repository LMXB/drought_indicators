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
library(rgdal)
library(glogis)

#load in gamma fitting function
#source("D:\\Git_Repo\\drought_indicators\\functions\\gamma_fit.R")

## DEFINE OUR VARIABLE NAME 
raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= "precipitation_amount")
proj4string(raster_precip) = CRS("+init=EPSG:4326")

raster_pet = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", var = "daily_mean_reference_evapotranspiration_grass")
proj4string(raster_pet) = CRS("+init=EPSG:4326")

#designate time scale
time_scale = c(30)#,60,90,180,300)

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("D:\\Git_Repo\\drought_indicators\\shp_kml\\UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("Y:\\Projects\\MCO_Drought_Indicators\\shp\\raw\\UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("Y:\\Projects\\MCO_Drought_Indicators\\shp\\raw\\UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("D:\\Git_Repo\\drought_indicators\\shp_kml\\montana_outline.kml")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
tic()
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))
toc()

tic()
raster_pet_spatial_clip = crop(raster_pet, extent(UMRB))
toc()

for(t in 1:length(time_scale)){
  
  
  #calcualte time
  tic()
  time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
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
  
  #sum and mask precip in parellel
  raster_precip_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }
  
  #sum and mask precip in parellel
  raster_pet_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_pet_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }
  
  raster_p_pet_diff = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = raster_precip_clipped[[i]] - raster_pet_clipped[[i]]
  }
  
  #stop parellel cluster
  stopCluster(cl)
  # 
  # data_2017 = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == 39]]]) %>%
  #   mask(montana) %>%
  #   crop(extent(montana))
  # 
  # data_2018 = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == 40]]]) %>%
  #   mask(montana) %>%
  #   crop(extent(montana))
  # 
  # data_2019 = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == 41]]]) %>%
  #   mask(montana) %>%
  #   crop(extent(montana))
  # 
  # writeRaster(data_2017, "D:\\gridMET_Comparison\\gridMET_2017-02-01_2017-03-01_pr_sum_OPeNDAP.tif", format = "GTiff", overwrite = T)
  # writeRaster(data_2018, "D:\\gridMET_Comparison\\gridMET_2018-02-01_2018-03-01_pr_sum_OPeNDAP.tif", format = "GTiff", overwrite = T)
  # writeRaster(data_2019, "D:\\gridMET_Comparison\\gridMET_2019-02-01_2019-03-01_pr_sum_OPeNDAP.tif", format = "GTiff", overwrite = T)
  
  # 
  #calucalte time integrated precip sum
  
  ######################################################
  ## add in structure function here to eliminate loop ##
  ######################################################
  
  integrated_diff = data.frame(matrix(nrow = length(values(raster_p_pet_diff[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_diff[,i] = values(raster_p_pet_diff[[i]])
  }
  
  integrated_diff[integrated_diff == 0] = NA
  
  #spi function
  spei_fun <- function(x) {
    tryCatch(
      {
        x = as.numeric(x)
        fit.loglogistic = glogisfit(x)
        fit.cdf = pglogis(x,location = fit.loglogistic$parameters['location'], scale = fit.loglogistic$parameters['scale'], 
                          shape = fit.loglogistic$parameters['shape'], lower.tail = TRUE, log.p = FALSE)
        standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
        return(standard_norm[length(standard_norm)])
      },
      error=function(cond) {
        return(NA)
      })
  }
  
  cl = makeCluster(detectCores()-1)
  registerDoParallel(cl)
  clusterEvalQ(cl, library(glogis))
  tic()
  current_spei = parApply(cl,integrated_diff, 1, FUN = spei_fun)
  toc()
  stopCluster(cl)
  
  
  #calcualte current spi
  #current_spei = apply(integrated_diff, 1, spei_fun)
  
  #create spatial template for current spi values
  spei_map = raster_p_pet_diff[[1]]
  
  #allocate curent spi values to spatial template
  values(spei_map) = current_spei
  
  metadata(spei_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("red", "white", "blue"))
  
  #plot map
  
  plot(spei_map, col = color_ramp(11), zlim = c(-3.5,3.5))
  addLines(montana)
  
  path_file = paste("D:\\Git_Repo\\drought_indicators\\apps\\spei_app\\current_spei_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  writeRaster(spei_map, path_file, format = "GTiff", overwrite = T)
  
  #calulcate watershed averages
  
  #start cluster for parellel computing
  # cl = makeCluster(detectCores()-1)
  # registerDoParallel(cl)
  # 
  # #sum and mask precip in parellel
  # watershed_values = foreach(i=1:length(watersheds$HUC8)) %dopar% {
  #   library(raster)
  #   median(values(mask(spi_map, (watersheds[watersheds$HUC8[i], ]))), na.rm = T)
  # }
  #   #stop parellel cluster
  # stopCluster(cl)
  # 
  # 
  # 
  # Extract raster values for each HUC 
  r.vals <- extract(spei_map, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  
  watersheds_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("D:\\Git_Repo\\drought_indicators\\apps\\spei_app", sep = "")
  layer_name = paste("current_spei_watershed_",as.character(time_scale[t]), sep = "")
  
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  #extract raster values for each county
  r.vals <- extract(spei_map, county)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  
  county_export = county
  
  county_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("D:\\Git_Repo\\drought_indicators\\apps\\spei_app", sep = "")
  layer_name = paste("current_spi_county_",as.character(time_scale[t]), sep = "")
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  toc()
}
