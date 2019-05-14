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
  collumn_name = c("Date","Soil.Moisture.Percent..2in..pct..Start.of.Day.Values", "Soil.Moisture.Percent..8in..pct..Start.of.Day.Values",
                   "Soil.Moisture.Percent..20in..pct..Start.of.Day.Values", "Snow.Water.Equivalent..in..Start.of.Day.Values")
  tryCatch({
    extract_columns(historical[[i]], collumn_name)
  }, error = function(e){
    return(NA)
  }
  )
}

stopCluster(cl)

#rename
snotel_soil_moisture = historical_select

save(snotel_soil_moisture, file = "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")
