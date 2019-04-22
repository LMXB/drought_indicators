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


raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= "precipitation_amount")
#proj4string(raster_precip) = CRS("+init=EPSG:4326")

raster_pet = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc", var = "daily_mean_reference_evapotranspiration_grass")
#proj4string(raster_pet) = CRS("+init=EPSG:4326")

#import UMRB outline for clipping and watershed for aggregating
UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Outline_Conus.shp")
watersheds = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_HUC8_Simple.shp")
county = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/UMRB_Clipped_County_Simple.shp")
montana = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/montana_outline.kml")

#clip precip and PET grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(UMRB))
raster_pet_spatial_clip = crop(raster_pet, extent(UMRB))

time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

water_year = (length(time$day) - which(time$day == "10-01")[length(which(time$day == "10-01"))])
year_to_date = (length(time$day) - which(time$day == "01-01")[length(which(time$day == "01-01"))])

#designate time scale
time_scale = c(30,60,90,180,365, water_year, year_to_date)

for(t in 1:length(time_scale)){
  
  #calcualte time
  tic()
  
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
  
  #sum and mask PET in parellel
  raster_pet_clipped = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = sum(raster_pet_spatial_clip[[slice_vec[group_by_vec == i]]])
    mask(temp, UMRB)
  }
  
  #Compute Difference Grids
  raster_p_pet_diff = foreach(i=unique(group_by_vec)) %dopar% {
    library(raster)
    temp = raster_precip_clipped[[i]] - raster_pet_clipped[[i]]
  }
  
  integrated_diff = data.frame(matrix(nrow = length(values(raster_p_pet_diff[[1]])), ncol = length(unique(group_by_vec))))
  for(i in 1:length(unique(group_by_vec))){
    integrated_diff[,i] = values(raster_p_pet_diff[[i]])
  }
  
  spei_fun <- function(x) {
    #first try log logistic
    tryCatch(
      {
        x = as.numeric(x)
        #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
        pwm = pwm.ub(x)
        #Probability-Weighted Moments to L-moments
        lmoments_x = pwm2lmom(pwm)
        #fit generalized logistic
        fit.parglo = parglo(lmoments_x)
        #compute probabilistic cdf 
        fit.cdf = cdfglo(x, fit.parglo)
        #compute standard normal equivelant
        standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
        return(standard_norm[length(standard_norm)])
      },
      #else return NA
      error=function(cond) {
        return(NA)
      })
  }
  
  #compute SPEI
  Packages <- c("glogis", "PearsonDS", "gsl", "lmomco")
  
  clusterCall(cl, function() {lapply(c("glogis", "PearsonDS", "gsl", "lmomco"), library, character.only = TRUE)})
  current_spei = parApply(cl,integrated_diff, 1, FUN = spei_fun)
  stopCluster(cl)
  
  ############################################
  ############## RASTER FILE #################
  ############################################
  
  #create spatial template for current spei values
  spei_map = raster_p_pet_diff[[1]]
  
  #allocate curent spi values to spatial template and add metadata of last time stamp used
  values(spei_map) = current_spei
  metadata(spei_map) = list(substr(time$datetime[length(time$datetime)],1,10))
  
  #compute color ramp for visualization
  color_ramp = colorRampPalette(c("red", "white", "blue"))
  
  #plot map
  plot(spei_map, col = color_ramp(100), zlim = c(-3.5,3.5), 
       main = paste0("Current ", as.character(time_scale[t]), " Day SPEI"))
  plot(montana, add = T)
  
  #define path to export and wrtie GEOtiff
  path_file = paste("/home/zhoylman/drought_indicators/spei_app/maps/current_spei/current_spei_",
                    as.character(time_scale[t]),".tif", sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      path_file = paste("/home/zhoylman/drought_indicators/spei_app/maps/current_spei/current_spei_",
                        "water_year",".tif", sep = "")
    }
    if(t == (length(time_scale))){
      path_file = paste("/home/zhoylman/drought_indicators/spei_app/maps/current_spei/current_spei_",
                        "year_to_date",".tif", sep = "")
    }
  }
  
  writeRaster(spei_map, path_file, format = "GTiff", overwrite = T)

  ############################################
  ################ SHP FILES #################
  ############################################
  
  # Extract raster values for each HUC 
  r.vals <- extract(spei_map, watersheds)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export and add metadata about last timestamp used
  watersheds_export = watersheds
  watersheds_export$current_time = substr(time$datetime[length(time$datetime)],1,10)
  
  #assign watershed aggregate values to shps, define path to export and export
  watersheds_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("/home/zhoylman/drought_indicators/spei_app/shp/current_spei/", sep = "")
  layer_name = paste("current_spei_watershed_",as.character(time_scale[t]), sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_spei_watershed_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_spei_watershed_year_to_date", sep = "")
    }
  }
  
  rgdal::writeOGR(obj=watersheds_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  #extract raster values for each county
  r.vals <- extract(spei_map, county)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  #create shp file for export
  county_export = county
  
  #assign county aggregate values to shps, define path to export and export
  county_export$average = as.vector(unlist(r.median))
  path_file_watershed = paste("/home/zhoylman/drought_indicators/spei_app/shp/current_spei/", sep = "")
  layer_name = paste("current_spei_county_",as.character(time_scale[t]), sep = "")
  
  # wateryear and year to date file name
  if(t > (length(time_scale)-2)){
    if(t == (length(time_scale)-1)){
      layer_name = paste("current_spei_county_water_year", sep = "")
    }
    if(t == (length(time_scale))){
      layer_name = paste("current_spei_county_year_to_date", sep = "")
    }
  }
  
  rgdal::writeOGR(obj=county_export, dsn=path_file_watershed, layer = layer_name, driver="ESRI Shapefile", overwrite_layer = T)
  
  toc()
}


