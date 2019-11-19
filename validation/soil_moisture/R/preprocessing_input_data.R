library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)
library(stringr)

#load snotel soil moisture data  (drought metric data loaded below)
station_data = read.csv("~/drought_indicators_data/mesonet/station_data_clean.csv")

#load mesonet data
load("/home/zhoylman/drought_indicators_data/mesonet/mesonet_soil_moisture.RData")

#re organize data
mesonet_soil_moisture = mesonet_soil_moisture[order(mesonet_soil_moisture$station_key, mesonet_soil_moisture$datetime),]

#find site names with valid soil moisture data
valid_stations = unique(station_data$station_key)

# clean up station data
station_data$X = NULL
station_data$network = "Mesonet"

#filter stations info
mesonet_soil_moisture = mesonet_soil_moisture[(mesonet_soil_moisture$station_key %in% valid_stations),]

# import and clean snotel moisture
snotel = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")

snotel_cropped = snotel %>%
  dplyr::select(site_name, latitude, longitude) %>%
  rename(station_key = site_name)

snotel_cropped$network = "NRCS"

master_list = rbind(station_data,snotel_cropped)

#write.csv(master_list,"/home/zhoylman/drought_indicators/validation/soil_moisture/site_locations.csv", row.names=FALSE)

# restucture mesonet soil moisture data to be consitant with list format of NRCS
# get mesonet depths and remove the surface probe (soilvwc00)
mesonet_depths = sort(unique(mesonet_soil_moisture$element))
data_reorganized = list()
data = list()

for(m in 1:length(station_data$station_key)){
  for(i in 1:length(mesonet_depths)){
    tryCatch({
      data[[i]] = mesonet_soil_moisture %>%
        dplyr::filter(station_key == station_data$station_key[m] & element == mesonet_depths[i])%>%
        dplyr::select(datetime,value)%>%
        dplyr::rename(Date = datetime)
      colnames(data[[i]]) = c("Date", as.character(mesonet_depths[i]))
    },error = function(e){
      return(NA)
    })
  }
  data_join = dplyr::full_join(data[[1]], data[[2]], by = "Date")%>%
    dplyr::full_join(.,data[[3]], by = "Date")%>%
    dplyr::full_join(.,data[[4]], by = "Date")%>%
    dplyr::full_join(.,data[[5]], by = "Date")
  data_reorganized[[m]] = data_join
}

mesonet_soil_moisture_list = data_reorganized

# restructure snotel data so that each dataframe in the list has the same number of collumns (depths)
# add collumns of NA for depths that do not have data. 
load("/home/zhoylman/drought_indicators_data/snotel/snotel_soil_moisture_unfrozen.RData")
all_cols_snotel = unique(unlist(lapply(snotel_soil_moisture, colnames)))

for(i in 1: length(snotel_soil_moisture)){
  #find missing collumns
  missing = all_cols_snotel[!(all_cols_snotel %in% colnames(snotel_soil_moisture[[i]]))]
  
  #if there are missing collumns do this (add collumns and assign names)
  if(!(rlang::is_empty(missing))){ 
    for(l in 1:length(missing)){
      snotel_soil_moisture[[i]]$missing = NA
      data.table::setnames(snotel_soil_moisture[[i]], "missing" , as.character(missing[l]))
    }
  }
  
  #reorginize column order
  data.table::setcolorder(snotel_soil_moisture[[i]], all_cols_snotel)
}

save(snotel_soil_moisture, file = "/home/zhoylman/drought_indicators_data/preprocessed_soil_moisture/snotel_soil_moisture.Rdata")
save(mesonet_soil_moisture_list, file = "/home/zhoylman/drought_indicators_data/preprocessed_soil_moisture/mesonet_soil_moisture_list.Rdata")
