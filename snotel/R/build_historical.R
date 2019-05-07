library(RNRCS) 
library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#hit the NRCS server for historical
tic()
current = list()
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

historical = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(RNRCS)
  tryCatch({
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = "1981-10-01",
                  DayEnd = "2011-10-01")
  }, error = function(e){
    return(NA)
  }
  )
}

toc()

extract_columns <- function(data, collumn_name) {
  extracted_data <- data %>%
    select_(.dots = collumn_name)
  return(extracted_data)
}

clusterExport(cl, "extract_columns")

historical_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  collumn_name = c("Snow.Water.Equivalent..in..Start.of.Day.Values", "Precipitation.Accumulation..in..Start.of.Day.Values",
                   "Date")
  tryCatch({
    extract_columns(historical[[i]], collumn_name)
  }, error = function(e){
    return(NA)
  }
  )
}

stopCluster(cl)

#reformat, add metadata
for(i in 1:length(historical_select)){
  if(length(historical_select[[i]]) == 3){
    colnames(historical_select[[i]]) = c("SWE", "Precip", "Date")
    historical_select[[i]]$Date = as.Date(historical_select[[i]]$Date)
    historical_select[[i]]$yday = yday(historical_select[[i]]$Date)
  }
}

#create daily climatology for each station
climatology = list()

for(i in 1:length(snotel$site_num)){
tryCatch({
  temp_clim = historical_select[[i]]%>%
    dplyr::group_by(yday)%>%
    dplyr::summarize(median_swe = median(SWE, na.rm = T),
                     swe_quantiles_005 = quantile(SWE, probs = c(0.05), na.rm = T),
                     swe_quantiles_025 = quantile(SWE, probs = c(0.25), na.rm = T),
                     swe_quantiles_075 = quantile(SWE, probs = c(0.75), na.rm = T),
                     swe_quantiles_095 = quantile(SWE, probs = c(0.95), na.rm = T),
                     median_precip = median(Precip, na.rm = T),
                     precip_quantiles_005 = quantile(Precip, probs = c(0.05), na.rm = T),
                     precip_quantiles_025 = quantile(Precip, probs = c(0.25), na.rm = T),
                     precip_quantiles_075 = quantile(Precip, probs = c(0.75), na.rm = T),
                     precip_quantiles_095 = quantile(Precip, probs = c(0.95), na.rm = T))
    
  climatology[[i]] = temp_clim
},
  error = function(e){
    return(NA)
    
  })
}

#add names metadata
names(climatology) = snotel$site_name

save(climatology, file = "/home/zhoylman/drought_indicators/snotel/climatology/snotel_climatology.RData")
