library(RNRCS) 
library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(lubridate)
library(dplyr)
library(sf)
library(ggplot2)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#hit the NRCS server for current data
tic()
current = list()
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

current = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(RNRCS)
  library(lubridate)
  #rule for start date depending on if date is between Oct - Dec use current year data,
  #else (Jan - Sept), start date is the previous year (water year)
  if(month(as.Date(Sys.time()))>=10){
    start = paste0(year(as.Date(Sys.time())),"-10-01")
  } else {
    start = paste0((year(as.Date(Sys.time()))-1),"-10-01")
  }
  
  tryCatch({
    #pull in data
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = start,
                  DayEnd = as.Date(Sys.time()))
  }, error = function(e){
    return(NA)
  }
  )
}

extract_columns <- function(data, collumn_name) {
  extracted_data <- data %>%
    select_(.dots = collumn_name)
  return(extracted_data)
}

clusterExport(cl, "extract_columns")

current_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  collumn_name = c("Snow.Water.Equivalent..in..Start.of.Day.Values", 
                   "Precipitation.Accumulation..in..Start.of.Day.Values", "Date")
  tryCatch({
    extract_columns(current[[i]], collumn_name)
  }, error = function(e){
    return(NA)
  }
  )
}

stopCluster(cl)

toc()

#clean up data
for(i in 1:length(snotel$site_num)){
  if(length(current_select[[i]])>1){
    colnames(current_select[[i]]) = c("SWE", "Precip","Date")
    current_select[[i]]$yday = yday(current_select[[i]]$Date)
    current_select[[i]]$Date = as.POSIXct(current_select[[i]]$Date, format = "%Y-%m-%d")
    current_select[[i]]$SWE = current_select[[i]]$SWE*25.4
    current_select[[i]]$Precip = current_select[[i]]$Precip*25.4
  }
}

#load in climatology data
load("/home/zhoylman/drought_indicators/snotel/climatology/snotel_climatology.RData")

climatology_WY = climatology

#compute index and sequences for water year
for(i in 1:length(snotel$site_num)){
  if(length(climatology_WY[[i]]$yday) == 366){
    climatology_WY[[i]] = climatology_WY[[i]] %>%
      dplyr::mutate(WY = c(seq(91,366,1), seq(1,90,1)))
  }
  if(length(climatology_WY[[i]]$yday) == 365){
    climatology_WY[[i]] = climatology_WY[[i]] %>%
      dplyr::mutate(WY = c(seq(91,365,1), seq(1,90,1)))
  }
}

plot_snotel = function(current_data){
  plot = ggplot(data = current_data, aes(x = Date, y = SWE))+
    geom_line(color = "darkblue")+
    ylab("SWE & Precipitaiton (mm)")+
    xlim(c(current_select[[1]]$Date[1], 
         as.POSIXct(paste0(year(current_select[[1]]$Date[1])+1,"-10-01"), format = "%Y-%m-%d")))+
  theme_bw(base_size = 16)
  return(plot)
}

plot_snotel(current_select[[1]])
