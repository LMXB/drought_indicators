#Regional Anlysis of NPP~DEF Sensativity (Author: Zach Hoylman)
rm(list = ls())
gc()
library(ggplot2)
library(raster)
library(SDMTools)
library(graphics)
library(maptools)
library(grDevices)
library(dplyr)
library(statar)
library(rgdal)
library(parallel)
library(knitr)
library(pryr)
library(tictoc)
library(stars)
library(data.table) 
tic()
#Set Directory
setwd("D:\\Hoylman_Temp\\export")

#Set Temp Directory
rasterOptions(tmpdir='J:\\RStudio_Temp')

#import rasters
mean_def = read_stars("Deficit_30m_Masked.tif")
mean_def.p = c(mean_def$Deficit_30m_Masked.tif)
rm(mean_def); gc()

Standardized_Slope = read_stars("Standardized_Slope_30m_Masked.tif")
Standardized_Slope.p = c(Standardized_Slope$Standardized_Slope_30m_Masked.tif)
rm(Standardized_Slope); gc()

#calcualte deficit quantile index
index = xtile(mean_def.p, 100)
toc()
gc()
#save(index, file = "index.RData")

regional_summary <- 1:100 %>%
  magrittr::set_names(.,paste0(.,"%")) %>%
  purrr::map(function(i){
    tic()
    gc()
    index_bin = Standardized_Slope.p[index == i]
    export.ls = list(median = median(index_bin, na.rm = TRUE),
                     quantile.ls = quantile(index_bin, c(0.05, 0.95), na.rm = TRUE))
    gc()
    time = toc()
    cat((time$toc - time$tic), "seconds ", "run #", i, "\n")
    return(append(export.ls, list(median_def = median(mean_def.p[index == i], na.rm = T))))
  })

save(regional_summary, file = "regional_summary.Rdata")

#clear index; no longer needed
rm(index);gc()

TPI = read_stars("TPI_30m_1000m_Radius_Masked.tif")
TPI.p = c(TPI$TPI_30m_1000m_Radius_Masked.tif)
rm(TPI); gc()

#set Break Points
#Class 1 = <-80, class 2 = -80>-70, etc
TPI_breaks = c(-Inf, -80, -70, -60, -50, -40, -30, -20, -10, 0)

#Calculate TPI bins
TPI_Bin = .bincode(TPI.p, breaks = TPI_breaks)

#for(j in 1:(length(TPI_breaks)-1)){
for(j in 2){
  cat("TPI Class #", j, "start", "\n")
  rm(TPI_index_bin_def, TPI_index_bin_slope, index);gc()
  #crop deficit based on TPI class
  TPI_index_bin_def = mean_def.p[TPI_Bin == j]
  #crop the Standardized Slope based on TPI class
  TPI_index_bin_slope = Standardized_Slope.p[TPI_Bin == j]
  #calculate ntile breaks
  index = xtile(TPI_index_bin_def, 100)

TPI_class_Summary[[j]] <- 1:100 %>%
  magrittr::set_names(.,paste0(.,"%")) %>%
  purrr::map(function(i){
    tic()
    gc()
    #extract Slope data for each def class
    index_bin = TPI_index_bin_slope[index == i]
    #calculate Stats
    export.ls = list(median = median(index_bin, na.rm = TRUE),
                     quantile.ls = quantile(index_bin, c(0.05, 0.95), na.rm = TRUE))
    gc()
    time = toc()
    cat((time$toc - time$tic)/60, "minutes ", "run #", i, "\n")
    return(append(export.ls, list(median_def = median(TPI_index_bin_def[index == i], na.rm = T))))
  })
  cat("TPI Class #", j, "done", "\n", "\n")
}

save(TPI_class_Summary, file = "TPI_class_Summary.Rdata")




#calculate median

median_def = aggregate(mean_def.p ~ index, data = NULL, mean)
median_slope = aggregate(Standardized_Slope.p ~ index, data = NULL, mean)
upper_q_slope = aggregate(Standardized_Slope.p ~ index, data = NULL, FUN = function(x) quantile(x,0.95))
lower_q_slope = aggregate(Standardized_Slope.p ~ index, data = NULL, FUN = function(x) quantile(x,0.05))

data = data.frame(x = median_def$mean_def.p, y = median_slope$Standardized_Slope.p,
                  upper = upper_q_slope$Standardized_Slope.p, lower = lower_q_slope$Standardized_Slope.p)

toc()


#############################################################################################################

export_list = list(regional_summary, neg_0_10_summary, neg_10_20_summary, neg_20_30_summary, neg_30_40_summary,
                   neg_40_50_summary, neg_50_60_summary, neg_60_70_summary, neg_70_80_summary, neg_80_summary)

csv_names = c("Regional_250m.csv", "Regional_250m_Neg_0-10.csv", "Regional_250m_Neg_10-20.csv",
              "Regional_250m_Neg_20-30.csv", "Regional_250m_Neg_30-40.csv", "Regional_250m_Neg_40-50.csv",
              "Regional_250m_Neg_50-60.csv", "Regional_250m_Neg_60-70.csv", "Regional_250m_Neg_70-80.csv",
              "Regional_250m_Neg_-80--.csv")


#Save it
setwd('D:\\EcoHydro\\Manuscript_3\\test')
for(i in 1:length(csv_names)){
  write.csv(export_list[[i]],csv_names[i])
}

