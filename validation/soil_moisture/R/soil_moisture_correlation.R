library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)
library(stringr)
library(tictoc)

#load network site data
mesonet_site_data = read.csv("~/drought_indicators_data/mesonet/station_data_clean.csv")
snotel_site_data = read.csv("~/drought_indicators_data/snotel/nrcs_soil_moisture.csv")

#load functions
source("./spi_app/R/gamma_fit.R")
source("./validation/soil_moisture/R/gamma_standard_fun.R")
source("./validation/soil_moisture/R/cross_cor.R")
source("./validation/soil_moisture/R/moving_cross_cor.R")
source("./validation/soil_moisture/R/drv_cor.R")

#load preprocessed soil moisture data 
load("~/drought_indicators_data/preprocessed_soil_moisture/mesonet_soil_moisture_list.Rdata")
load("~/drought_indicators_data/preprocessed_soil_moisture/snotel_soil_moisture.Rdata")

#trouble shooting set up for functions
# site = 28
# drought_index = mesonet_spi[[site]]
# soil_moisture = mesonet_soil_moisture_list[[site]]
# cross_cor(drought_index,soil_moisture)
# moving_cross_cor(drought_index,soil_moisture)
# drv_cor(drought_index,soil_moisture)

# 
# site = 1
# drought_index = snotel_spi[[site]]
# soil_moisture = snotel_soil_moisture[[site]]
# cross_cor(drought_index,soil_moisture)
# moving_cross_cor(drought_index,soil_moisture)
# drv_cor(drought_index,soil_moisture)


############################# Run Validation Analysis #######################################

################################ SNOTEL correlation #########################################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 1:length(drought_metrics)){
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/snotel/snotel_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "cross_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("snotel_", drought_metrics[i])))
  
  # Run correlation analysis
  temp = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
    library(dplyr)
    cross_cor(drought_data[[site]], snotel_soil_moisture[[site]])
  }
  stopCluster(cl)
  
  assign(paste0("correlation_matrix_snotel_", drought_metrics[i]), temp)
  
  objectName = paste0("correlation_matrix_snotel_", drought_metrics[i])
  
  save(list = paste0("correlation_matrix_snotel_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/correlation_matrix_",
                     drought_metrics[i],"_unfrozen.RData"))
  gc()
  rm(temp, drought_data);gc()
}

##################### Mesonet correlation ##############################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 1:length(drought_metrics)){
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/mesonet/mesonet_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "cross_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("mesonet_", drought_metrics[i])))
  
  # Run correlation analysis
  temp = foreach(site = 1:length(mesonet_soil_moisture_list)) %dopar% {
      library(dplyr)
      cross_cor(drought_data[[site]], mesonet_soil_moisture_list[[site]])
    }
  stopCluster(cl)
  
  assign(paste0("correlation_matrix_mesonet_", drought_metrics[i]), temp)
  
  objectName = paste0("correlation_matrix_mesonet_", drought_metrics[i])
  
  save(list = paste0("correlation_matrix_mesonet_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/correlation_matrix_mesonet_unfrozen_",
       drought_metrics[i],".RData"))
  gc()
  rm(temp, drought_data);gc()
}


########################## monthly correlations ###########################

######################### SNOTEL Monthly correlation ######################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 2:length(drought_metrics)){
  tic()
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/snotel/snotel_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "cross_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("snotel_", drought_metrics[i])))
  
  # Run correlation analysis output = [[site]][[timescale]]
  temp = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
    library(dplyr)
    moving_cross_cor(drought_data[[site]], snotel_soil_moisture[[site]])
  }
  stopCluster(cl)
  
  assign(paste0("monthly_correlation_matrix_snotel_", drought_metrics[i]), temp)
  
  objectName = paste0("monthly_correlation_matrix_snotel_", drought_metrics[i])
  
  save(list = paste0("monthly_correlation_matrix_snotel_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/monthly_correlation_matrix_",
                     drought_metrics[i],"_unfrozen.RData"))
  gc()
  rm(temp, drought_data);gc()
  toc()
  print(Sys.time())
}


######################### Mesonet Monthly correlation ######################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 2:length(drought_metrics)){
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/mesonet/mesonet_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "cross_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("mesonet_", drought_metrics[i])))
  
  # Run correlation analysis output = [[site]][[timescale]]
  temp = foreach(site = 1:length(mesonet_soil_moisture_list)) %dopar% {
    library(dplyr)
    moving_cross_cor(drought_data[[site]], mesonet_soil_moisture_list[[site]])
  }
  stopCluster(cl)
  
  assign(paste0("monthly_correlation_matrix_mesonet_", drought_metrics[i]), temp)
  
  objectName = paste0("monthly_correlation_matrix_mesonet_", drought_metrics[i])
  
  save(list = paste0("monthly_correlation_matrix_mesonet_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/monthly_correlation_matrix_mesonet_",
                     drought_metrics[i],"_unfrozen.RData"))
  gc()
  rm(temp, drought_data);gc()
  print(Sys.time())
}


########################## Wetting / Drying Correlations #################
######################### SNOTEL wet_dry correlation ######################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 1:length(drought_metrics)){
  tic()
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/snotel/snotel_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "drv_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("snotel_", drought_metrics[i])))
  
  # Run correlation analysis output = [[site]][[timescale]]
  temp = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
    library(dplyr)
    drv_cor(drought_data[[site]], snotel_soil_moisture[[site]])
  }
  stopCluster(cl)
  
  assign(paste0("wet_dry_correlation_matrix_snotel_", drought_metrics[i]), temp)
  
  objectName = paste0("wet_dry_correlation_matrix_snotel_", drought_metrics[i])
  
  save(list = paste0("wet_dry_correlation_matrix_snotel_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/wet_dry_correlation_matrix_",
                     drought_metrics[i],"_unfrozen.RData"))
  gc()
  rm(temp, drought_data);gc()
  toc()
  print(Sys.time())
}


######################### Mesonet wet_dry correlation ######################

drought_metrics = c("spi", "spei", "eddi", "sedi")
for(i in 1:length(drought_metrics)){
  #define drought metric path
  drought_metric_path = paste0("/home/zhoylman/drought_indicators_data/mesonet/mesonet_",
                               drought_metrics[i],".RData")
  #load data
  load(drought_metric_path)
  
  #start cluster
  cl = makeCluster(20)
  registerDoParallel(cl)
  clusterExport(cl, "gamma_fit")
  clusterExport(cl, "gamma_standard_fun")
  clusterExport(cl, "drv_cor")
  
  #dynamically reassin data name
  assign("drought_data", get(paste0("mesonet_", drought_metrics[i])))
  
  # Run correlation analysis output = [[site]][[timescale]]
  temp = foreach(site = 1:length(mesonet_soil_moisture_list)) %dopar% {
    library(dplyr)
    drv_cor(drought_data[[site]], mesonet_soil_moisture_list[[site]])
  }
  stopCluster(cl)
  
  assign(paste0("wet_dry_correlation_matrix_mesonet_", drought_metrics[i]), temp)
  
  objectName = paste0("wet_dry_correlation_matrix_mesonet_", drought_metrics[i])
  
  save(list = paste0("wet_dry_correlation_matrix_mesonet_", drought_metrics[i]), 
       file = paste0("/home/zhoylman/drought_indicators_data/correlation_matrix/wet_dry_correlation_matrix_mesonet_",
                     drought_metrics[i],"_unfrozen.RData"))
  gc()
  rm(temp, drought_data);gc()
  print(Sys.time())
}
