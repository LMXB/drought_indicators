library(ncdf4)
library(raster)
library(RCurl)
library(tidyverse)
library(magrittr)
library(raster)
library(foreach)
library(doParallel)
library(parallel)
library(stringr)

url = "ftp://ftp.cpc.ncep.noaa.gov/GIS/USDM_Products/soil/total/daily/"

files = read.table(url) %>%
  dplyr::select(V9) %$%
  V9

file_location_vec = paste0(url, files)
#file_location = file_location_vec[166]

get_cpc =function(file_location) {
  tryCatch({
    # if the file is a zipped file then extract
    if(stringr::str_detect(file_location, ".zip")){
      temp=tempfile()
      download.file(file_location, temp)
      files = unzip(temp)
      # if the file is simply another zipped file
      if(length(files) == 1){
        repeat {
          files = unzip(files)
          # check for success
          if (length(files) > 1) break
        }
      }
      index = grep(".tif$", files)
      name = files[index]
      name_short = stringr::str_extract(name, "(\\d)+")
      #turn into raster and rename
      data = raster::raster(name)
      unlink(temp)
    }
    # if it is just a tif then read in the tif and extract name (date)
    if(stringr::str_detect(file_location, ".tif")){
      data = raster::raster(file_location)
      name = names(data)
      name_short = stringr::str_extract(name, "(\\d)+")
    }
    #if the data isnt in wgs84, reproject to wgs84
    if(raster::projection(data, asText=TRUE) != "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      reproject = raster::projectRaster(data, crs = sp::CRS("+init=epsg:4326"), res = 1)
    }
    if(raster::projection(data, asText=TRUE) == "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
      reproject = data
    }
    names(reproject) = name_short
    return(reproject)
  }, error = function(e) {
    return(NULL)
  })
}

#get the most current dataset to make a mask
template = get_cpc(file_location_vec[length(file_location_vec)])

cl = makeCluster(20)
registerDoParallel(cl)

cpc_list = foreach(i = 1:length(file_location_vec)) %dopar% {
  library(dplyr)
  temp = get_cpc(file_location_vec[i]) 
  if(!is.null(temp)){
    temp[temp < 0] = NA
    raster::resample(temp, template, "ngb")
  }
}

null_index = which(vapply(cpc_list, is.null, TRUE))

cpc_list[c(null_index)] = NULL

brick = brick(cpc_list)

cpc_soil_moisture = brick

writeRaster(cpc_soil_moisture, "/home/zhoylman/drought_indicators_data/cpc_soil_moisture/cpc_soil_moisture_brick.tif")

stopCluster(cl)
