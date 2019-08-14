library(RNRCS) 
library(tictoc)
library(foreach)
library(doParallel)
library(parallel)
library(dplyr)
library(data.table)
library(sf)
library(mcor)

nrcs_sites = grabNRCS.meta(ntwrks=c("ALL"))

for(i in 1:length(nrcs_sites)){
  if(i == 1){
    nrcs_sites_df = as.data.frame(nrcs_sites[[1]])
  }
  else{
    nrcs_sites_df = rbind(nrcs_sites_df, as.data.frame(nrcs_sites[[i]]) )
  }
}

nrcs_sites_df = nrcs_sites_df %>%
  dplyr::filter(ntwk == c("SCAN", "SNTL", "SNTLT"))

write.csv(nrcs_sites_df, "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_meta.csv")
