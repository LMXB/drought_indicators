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
    extract_columns(current[[i]], "Snow.Water.Equivalent..in..Start.of.Day.Values")
  }, error = function(e){
    return(NA)
  }
  )
}

stopCluster(cl)

toc()

#clean up data and turn into data.frame
current_select_df = t(as.data.frame(current_select))
rownames(current_select_df) = snotel$site_name
colnames(current_select_df) = "SWE"

#load in climatology data
load("/home/zhoylman/drought_indicators/snotel/climatology/snotel_climatology.RData")

#get current yday for lookup
current_yday = yday(as.Date(Sys.time()))

#compute percent of average
daily_lookup = data.frame(matrix(nrow=length(snotel$site_num), ncol = 1))
colnames(daily_lookup) = "daily_mean"

for(i in 1:length(snotel$site_num)){
  tryCatch({
    daily_lookup$daily_mean[i] = climatology[[i]] %>%
    filter(yday == current_yday) %>%
    select(median_swe)
  }, error = function(e){
    return(NA)
  }
  )
  
}

#merge
daily_lookup$current = current_select_df

#compute pervent average
daily_lookup$percent = (as.numeric(daily_lookup$current)/as.numeric(daily_lookup$daily_mean))*100

#add station id
daily_lookup$site_name = as.character(snotel$site_name)

#flatten list for export
daily_lookup = data.frame(daily_lookup)

#Write daily table
save(daily_lookup, file = "/home/zhoylman/drought_indicators/snotel/climatology/current_precent_SWE.RData")
