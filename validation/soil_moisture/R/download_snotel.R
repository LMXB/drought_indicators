library(RNRCS) 
library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)
library(stringr)

#function to extract numeric ID from snotel/scan/sntlt meta
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#define input station meta data
snotel = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")
snotel$site_num_id = numextract(snotel$site_id)


#hit the NRCS server for historical
tic()
current = list()
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#NEW
historical_nrcs = foreach(i = 1:length(snotel$site_id)) %dopar%{
  library(RNRCS)
  tryCatch({
    grabNRCS.data(network = snotel$ntwk[i], snotel$site_num_id[i], timescale = "daily", DayBgn = "1900-10-01",
                  DayEnd = "2100-10-01")
  }, error = function(e){
    return(NA)
  }
  )
}

toc()

historical_select = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  library(data.table)
  tryCatch({
    historical_nrcs[[i]][colnames(historical_nrcs[[i]]) %like% "Date" | 
                         colnames(historical_nrcs[[i]]) %like% "Moisture"]
    }, error = function(e){
    return(NA)
  }
  )
}

historical_unfrozen = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  library(data.table)
  tryCatch({
    historical_nrcs[[i]][colnames(historical_nrcs[[i]]) %like% "Soil.Temperature"] %>%
      mutate(min_temp = purrr::pmap(., min)) %>%
      select(min_temp)
      
  }, error = function(e){
    return(NA) 
  }
  )
}


historical_select_unfrozen = foreach(i = 1:length(snotel$site_num)) %dopar%{
  library(dplyr)
  library(data.table)
  tryCatch({
    historical_select[[i]][historical_unfrozen[[i]]$min_temp > 32,]
  }, error = function(e){
    return(NA) 
  }
  )
}


stopCluster(cl)

good_data_index = which(lengths(historical_select_unfrozen) > 1)

snotel_soil_moisture = historical_select_unfrozen[c(good_data_index)]

save(snotel_soil_moisture, file = "/home/zhoylman/drought_indicators_data/snotel/snotel_soil_moisture_unfrozen.RData")
#write.csv(snotel_clipped, "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")
  