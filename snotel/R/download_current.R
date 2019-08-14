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
  tryCatch({
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = as.Date(Sys.time()),
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
  tryCatch({
    extract_columns(current[[i]], c("Snow.Water.Equivalent..in..Start.of.Day.Values", "Precipitation.Accumulation..in..Start.of.Day.Values"))
  }, error = function(e){
    return(NA)
  }
  )
}

# 
# historical = foreach(i = 1:length(snotel$site_num)) %dopar%{
#   library(RNRCS)
#   tryCatch({
#     grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = "1981-10-01",
#                   DayEnd = "2011-10-01")
#   }, error = function(e){
#     return(NA)
#   }
#   )
# }
# 
# 
# historical_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
#   library(dplyr)
#   collumn_name = c("Snow.Water.Equivalent..in..Start.of.Day.Values", "Precipitation.Accumulation..in..Start.of.Day.Values",
#                    "Date")
#   tryCatch({
#     extract_columns(historical[[i]], collumn_name)
#   }, error = function(e){
#     return(NA)
#   }
#   )
# }
# 
# 
# for(i in 1:length(snotel$site_name)){
#   if(length(historical_select[[i]]) == 3){
#     historical_select[[i]]$yday = yday(historical_select[[i]]$Date)
#   }
# }
stopCluster(cl)

toc()

#clean up data and turn into data.frame
current_select_df = data.frame(matrix(nrow = length(current_select),
                                      ncol = 2))
for(i in 1:length(current_select)){
  current_select_df[i,1:2] = current_select[[i]]
}

rownames(current_select_df) = snotel$site_name
colnames(current_select_df) = c("SWE", "Precip")


#calcualte yday percentiles for cdf
# for(i in 1:length(snotel$site_name)){
#   if(length(historical_select[[i]])==3){
#     historical_select[[i]] = historical_select[[i]]%>%
#       filter(yday == yday(as.Date(Sys.time())))
#     colnames(historical_select[[i]]) = c("SWE", "Date", "yday")
#   }
# }


#load in climatology data
load("/home/zhoylman/drought_indicators/snotel/climatology/snotel_climatology.RData")

#get current yday for lookup
current_yday = yday(as.Date(Sys.time()))

#compute percent of average
daily_lookup = data.frame(matrix(nrow=length(snotel$site_num), ncol = 2))
colnames(daily_lookup) = c("daily_mean_swe", "daily_mean_precip")

for(i in 1:length(snotel$site_num)){
  tryCatch({
    daily_lookup[i,1:2] = climatology[[i]] %>%
    filter(yday == current_yday) %>%
    select(median_swe, median_precip)
  }, error = function(e){
    return(c(NA,NA))
  }
  )
  
}

#merge
daily_lookup$current_swe = current_select_df$SWE
daily_lookup$current_precip = current_select_df$Precip


#compute pervent average
daily_lookup$percent_swe = (as.numeric(daily_lookup$current_swe)/as.numeric(daily_lookup$daily_mean_swe))*100
daily_lookup$percent_precip = (as.numeric(daily_lookup$current_precip)/as.numeric(daily_lookup$daily_mean_precip))*100

daily_lookup[daily_lookup == Inf] = NA
daily_lookup[daily_lookup == "NaN"] = NA

#add station id
daily_lookup$site_name = as.character(snotel$site_name)

#flatten list for export
daily_lookup = data.frame(daily_lookup)

#Write daily table
save(daily_lookup, file = "/home/zhoylman/drought_indicators/snotel/climatology/current_precent_SWE.RData")
