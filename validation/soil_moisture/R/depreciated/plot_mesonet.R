library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)
library(stringr)
library(ggplot2)

#load data  
load("/home/zhoylman/drought_indicators_data/snotel_spei/snotel_spei.RData")
load("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")
load("/home/zhoylman/drought_indicators_data/mesonet_spei/mesonet_spei.RData")
load("/home/zhoylman/drought_indicators_data/mesonet_spei/mesonet_soil_moisture.RData")

#load functions
source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/gamma_standard_fun.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/cross_cor.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/moving_cross_cor.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/get_mesonet_station_info.R")

snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

# restucture mesonet soil moisture data to be consitant with list format of NRCS
# get mesonet depths and remove the surface probe (soilvwc00)
mesonet_depths = sort(unique(mesonet_soil_moisture$element))
data_reorganized = list()
data = list()

for(m in 1:length(station_data$station_key)){
  for(i in 1:length(mesonet_depths)){
    tryCatch({
      data[[i]] = mesonet_soil_moisture %>%
        dplyr::filter(station_key == station_data$station_key[m] & element == mesonet_depths[i])%>%
        dplyr::select(datetime,value)%>%
        dplyr::rename(Date = datetime)
      colnames(data[[i]]) = c("Date", as.character(mesonet_depths[i]))
    },error = function(e){
      return(NA)
    })
  }
  data_join = dplyr::full_join(data[[1]], data[[2]], by = "Date")%>%
    dplyr::full_join(.,data[[3]], by = "Date")%>%
    dplyr::full_join(.,data[[4]], by = "Date")%>%
    dplyr::full_join(.,data[[5]], by = "Date")
  data_reorganized[[m]] = data_join
}

mesonet_soil_moisture_list = data_reorganized

plot_sm_mesonet = function(data, station_name){
  tryCatch({
    plot = ggplot(data = data)+
      geom_line(aes(x = Date, soilwc00, color = "0 in"))+
      geom_line(aes(x = Date, soilwc04, color = "4 in"))+
      geom_line(aes(x = Date, soilwc08, color = "8 in"))+
      geom_line(aes(x = Date, soilwc20, color = "20 in"))+
      geom_line(aes(x = Date, soilwc36, color = "36 in"))+
      scale_color_manual(values = c("0 in" = "red","4 in" =  "orange","8 in" =  "green",
                                    "20 in" = "blue","36 in" =  "purple"),
                         breaks = c("0 in","4 in","8 in","20 in","36 in"),
                         name = "Probe Depth")+
      theme_bw(base_size = 16)+
      xlab("Date")+
      ylab("Soil Moisture (%)")+
      ggtitle(station_name)
    
    print(plot)
    
    return(plot)
  }, error = function(e){
    plot1 = ggplot(data = NULL)+
      geom_text(data = NULL, aes(x = 1, y = 1, label = "Sorry there was an error"), size = 16)+
      theme_bw(base_size = 16)+
      ggtitle(station_name)
    
    return(plot1)
  })
}
for(i in 1:length(mesonet_soil_moisture_list)){
  plot_sm_mesonet(mesonet_soil_moisture_list[[i]], station_data$station_key[i])
  ggsave(paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/mesonet_soil_moisture/", i,".png"),
         width = 11, height = 8, units = "in", dpi = 300)
  print(i)
}
