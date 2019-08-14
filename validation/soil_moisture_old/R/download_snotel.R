library(RNRCS) 
library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)
library(mcor)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))


#hit the NRCS server for historical
tic()
current = list()
cl = makeCluster(detectCores()-2)
registerDoParallel(cl)
# 
# nrcs_sites = grabNRCS.meta(ntwrks=c("ALL"))
# 
# for(i in 1:length(nrcs_sites)){
#   if(i == 1){
#     nrcs_sites_df = as.data.frame(nrcs_sites[[1]]) 
#   }
#   else{
#     nrcs_sites_df = rbind(nrcs_sites_df, as.data.frame(nrcs_sites[[i]]) )
#   }
# }
# 
# nrcs_sites_df = nrcs_sites_df %>%
#   dplyr::filter(state == c("MT", "ID", "WY", "SD"))%>%
#   dplyr::filter(ntwk == c("SCAN", "SNTL", "SNTLT", "SNOW"))
# 
# usgs_sites = as.data.frame(grabNRCS.meta(ntwrks=c("USGS")))
# 
# #NEW
# historical_nrcs = foreach(i = 1:length(nrcs_sites_df$site_id)) %dopar%{
#   library(RNRCS)
#   tryCatch({
#     grabNRCS.data(network = nrcs_sites_df$ntwk[i], nrcs_sites_df$site_id, timescale = "daily", DayBgn = "1900-10-01",
#                   DayEnd = "2100-10-01")
#   }, error = function(e){
#     return(NA)
#   }
#   )
# }

#OLD
historical = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(RNRCS)
  tryCatch({
    grabNRCS.data("SNTL", as.numeric(snotel$site_num[i]), timescale = "daily", DayBgn = "1900-10-01",
                  DayEnd = "2100-10-01")
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
                   "Soil.Moisture.Percent..20in..pct..Start.of.Day.Values")
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
