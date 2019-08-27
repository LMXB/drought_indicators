library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)
library(stringr)

#load data  
load("/home/zhoylman/drought_indicators_data/snotel/snotel_spei.RData")
load("/home/zhoylman/drought_indicators_data/snotel/snotel_soil_moisture.RData")
load("/home/zhoylman/drought_indicators_data/mesonet/mesonet_spei.RData")
load("/home/zhoylman/drought_indicators_data/mesonet/mesonet_soil_moisture.RData")

#load functions
source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/gamma_standard_fun.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/cross_cor.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/moving_cross_cor.R")

#re organize data
mesonet_soil_moisture = mesonet_soil_moisture[order(mesonet_soil_moisture$station_key, mesonet_soil_moisture$datetime),]

#find site names with valid soil moisture data
valid_stations = unique(mesonet_soil_moisture$station_key)

#filter stations info
station_data = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/mesonet_data/mesonet_station_data.csv")
station_data = station_data[(station_data$station_key %in% valid_stations),]
station_data$X = NULL
station_data$network = "Mesonet"

snotel = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")

snotel_cropped = snotel %>%
  dplyr::select(site_name, latitude, longitude) %>%
  rename(station_key = site_name)

snotel_cropped$network = "NRCS"

master_list = rbind(station_data,snotel_cropped)

write.csv(master_list,"/home/zhoylman/drought_indicators/validation/soil_moisture/site_locations.csv", row.names=FALSE)

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

# restructure snotel data so that each dataframe in the list has the same number of collumns (depths)
# add collumns of NA for depths that do not have data. 
all_cols_snotel = unique(unlist(lapply(snotel_soil_moisture, colnames)))

for(i in 1: length(snotel_soil_moisture)){
  #find missing collumns
  missing = all_cols_snotel[!(all_cols_snotel %in% colnames(snotel_soil_moisture[[i]]))]
  
  #if there are missing collumns do this (add collumns and assign names)
  if(!(rlang::is_empty(missing))){ 
    for(l in 1:length(missing)){
      snotel_soil_moisture[[i]]$missing = NA
      data.table::setnames(snotel_soil_moisture[[i]], "missing" , as.character(missing[l]))
    }
  }
  
  #reorginize column order
  data.table::setcolorder(snotel_soil_moisture[[i]], all_cols_snotel)
}

#trouble shooting set up for functions
# site = 1
# spei = mesonet_spei[[site]]
# soil_moisture = mesonet_soil_moisture_list[[site]]
# cross_cor(spei,soil_moisture)
# moving_cross_cor(spei,soil_moisture)
# 
site = 10
spei = snotel_spei[[site]]
soil_moisture = snotel_soil_moisture[[site]]
cross_cor(spei,soil_moisture)
moving_cross_cor(spei,soil_moisture)


##################################################################################
cl = makeCluster(20)
registerDoParallel(cl)
clusterExport(cl, "gamma_fit")
clusterExport(cl, "gamma_standard_fun")

tic()
correlation_matrix = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
  library(dplyr)
  cross_cor(snotel_spei[[site]], snotel_soil_moisture[[site]])
}

toc()
# stop cluster inbetween datasets to clear memory
stopCluster(cl)


cl = makeCluster(20)
registerDoParallel(cl)
clusterExport(cl, "gamma_fit")
clusterExport(cl, "gamma_standard_fun")

correlation_matrix_mesonet = foreach(site = 1:length(mesonet_soil_moisture_list)) %dopar% {
  library(dplyr)
  cross_cor(mesonet_spei[[site]], mesonet_soil_moisture_list[[site]])
}

toc()
stopCluster(cl)

##################################################################################
# moving window #
# cl = makeCluster(detectCores()-1)
# registerDoParallel(cl)
# clusterExport(cl, "gamma_fit")
# clusterExport(cl, "gamma_standard_fun")
# tic()
# moving_correlation_matrix = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
#   library(dplyr)
#   moving_cross_cor(snotel_spei[[site]], snotel_soil_moisture[[site]])
# }
# toc()
# 
# 
# stopCluster(cl)

find_best = function(x){
  times = c(seq(5,730,5))
  best_2in = times[which(x[1,]==max(x[1,], na.rm = T))]
  best_4in = times[which(x[2,]==max(x[2,], na.rm = T))]
  best_8in = times[which(x[3,]==max(x[3,], na.rm = T))]
  best_20in = times[which(x[4,]==max(x[4,], na.rm = T))]
  best_40in = times[which(x[5,]==max(x[5,], na.rm = T))]
  best_mean = times[which(x[6,]==max(x[6,], na.rm = T))]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_cor = function(x){
  times = c(seq(5,730,5))
  best_2in = x[which(x[1,]==max(x[1,], na.rm = T))][1,1]
  best_4in = x[which(x[2,]==max(x[2,], na.rm = T))][2,1]
  best_8in = x[which(x[3,]==max(x[3,], na.rm = T))][3,1]
  best_20in = x[which(x[4,]==max(x[4,], na.rm = T))][4,1]
  best_40in = x[which(x[5,]==max(x[5,], na.rm = T))][5,1]
  best_mean = x[which(x[6,]==max(x[6,], na.rm = T))][6,1]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_mesonet = function(x){
  times = c(seq(5,730,5))
  best_0in = times[which(x[1,]==max(x[1,], na.rm = T))]
  best_4in = times[which(x[2,]==max(x[2,], na.rm = T))]
  best_8in = times[which(x[3,]==max(x[3,], na.rm = T))]
  best_20in = times[which(x[4,]==max(x[4,], na.rm = T))]
  best_36in = times[which(x[5,]==max(x[5,], na.rm = T))]
  best_mean = times[which(x[6,]==max(x[6,], na.rm = T))]
  
  if(length(best_0in)==0){best_0in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_36in)==0){best_36in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
  return(best_times)
}

find_best_mesonet_cor = function(x){
  times = c(seq(5,730,5))
  best_0in = x[which(x[1,]==max(x[1,], na.rm = T))][1,1]
  best_4in = x[which(x[2,]==max(x[2,], na.rm = T))][2,1]
  best_8in = x[which(x[3,]==max(x[3,], na.rm = T))][3,1]
  best_20in = x[which(x[4,]==max(x[4,], na.rm = T))][4,1]
  best_36in = x[which(x[5,]==max(x[5,], na.rm = T))][5,1]
  best_mean = x[which(x[6,]==max(x[6,], na.rm = T))][6,1]
  
  if(length(best_0in)==0){best_0in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_36in)==0){best_36in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
  return(best_times)
}

best_times_matrix = data.frame(matrix(nrow = length(snotel_soil_moisture), ncol = 6))
colnames(best_times_matrix) = c("2in","4in", "8in", "20in", "40in", "mean")

best_cor_matrix = data.frame(matrix(nrow = length(snotel_soil_moisture), ncol = 6))
colnames(best_cor_matrix) = c("2in","4in", "8in", "20in", "40in", "mean")

best_times_matrix_mesonet = data.frame(matrix(nrow = length(station_data$station_key), ncol = 6))
colnames(best_times_matrix_mesonet) = c("0in", "4in", "8in", "20in", "36in", "mean")

best_cor_matrix_mesonet = data.frame(matrix(nrow = length(station_data$station_key), ncol = 6))
colnames(best_cor_matrix_mesonet) = c("0in", "4in", "8in", "20in", "36in", "mean")


for(i in 1:length(snotel_soil_moisture)){
  tryCatch({
    best_times_matrix[i,] = find_best(correlation_matrix[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

for(i in 1:length(snotel_soil_moisture)){
  tryCatch({
    best_cor_matrix[i,] = find_best_cor(correlation_matrix[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

for(i in 1:length(station_data$station_key)){
  tryCatch({
    best_times_matrix_mesonet[i,] = find_best_mesonet(correlation_matrix_mesonet[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

for(i in 1:length(station_data$station_key)){
  tryCatch({
    best_cor_matrix_mesonet[i,] = find_best_mesonet_cor(correlation_matrix_mesonet[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}


density_snotel = list()
density_mesonet = list()


for(i in 1:6){
  density_snotel[[i]] = data.frame(x = density(best_times_matrix[,i], na.rm = T)[[1]],
                                    y = density(best_times_matrix[,i], na.rm = T)[[2]])
}

for(i in 1:6){
  density_mesonet[[i]] = data.frame(x = density(best_times_matrix_mesonet[,i], na.rm = T)[[1]],
                                    y = density(best_times_matrix_mesonet[,i], na.rm = T)[[2]])
}


library(ggplot2)
color_names = c("2 in", "4 in" ,"8 in", "20 in" ,"40 in", "Mean")

snotel_summary = ggplot()+
  geom_line(data = density_snotel[[1]], aes(x = x, y = y, color = color_names[1]))+
  geom_line(data = density_snotel[[2]], aes(x = x, y = y, color = color_names[2]))+
  geom_line(data = density_snotel[[3]], aes(x = x, y = y, color = color_names[3]))+
  geom_line(data = density_snotel[[4]], aes(x = x, y = y, color = color_names[4]))+
  geom_line(data = density_snotel[[5]], aes(x = x, y = y, color = color_names[5]))+
  geom_line(data = density_snotel[[6]], aes(x = x, y = y, color = color_names[6]))+
  
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("2 in" =  "yellow", "4 in" = "orange",
                                "8 in" =  "green", "20 in" = "blue", 
                                "40 in" =  "purple", "Mean" = "black"),
                     breaks = c("2 in", "4 in","8 in","20 in","40 in", "Mean"),
                     name = "Probe Depth")+
  xlim(0,730)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.74),
        plot.title = element_text(hjust = 0.5))+
  ylab("Density")+
  xlab("Timescale (Days)")+
  ggtitle("Best Correlation Times (Soil Moisture ~ SPEI) \n (SNOTEL, SCAN, SNOTEL-LT)")

#add labels
for(i in 1:6){
  snotel_summary = snotel_summary + 
    geom_point(data = density_snotel[[i]][which(density_snotel[[i]]$y == max(density_snotel[[i]]$y)),], aes(x = x, y = y))+
    geom_text(data = data.frame(density_snotel[[i]][which(density_snotel[[i]]$y == max(density_snotel[[i]]$y)),],n = sum(!is.na(best_times_matrix[,i])), r = round(mean(best_cor_matrix[,i], na.rm = T), 2)), aes(x = x + 150, y = y, 
                                                                                                              label = paste0(round(x, digits = 0), " Days, n = ", n, ", mean r = ", r)))
}

png(filename = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/time_scale_summary_with_mean.png", 
    width = 8, height = 7, units = "in", res = 300)
snotel_summary
dev.off()

color_names = c("0 in" ,"4 in" ,"8 in", "20 in" ,"36 in", "Mean")

mesonet_summary = ggplot()+
  #geom_line(data = density_mesonet[[1]], aes(x = x, y = y, color = color_names[1]))+
  geom_line(data = density_mesonet[[2]], aes(x = x, y = y, color = color_names[2]))+
  geom_line(data = density_mesonet[[3]], aes(x = x, y = y, color = color_names[3]))+
  geom_line(data = density_mesonet[[4]], aes(x = x, y = y, color = color_names[4]))+
  geom_line(data = density_mesonet[[5]], aes(x = x, y = y, color = color_names[5]))+
  geom_line(data = density_mesonet[[6]], aes(x = x, y = y, color = color_names[6]))+
  
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("0 in" = "yellow","4 in" =  "orange", "8 in" =  "green",
                                "20 in" = "blue", "36 in" =  "purple", "Mean" = "black"),
                     breaks = c("4 in","8 in","20 in","36 in", "Mean"),
                     name = "Probe Depth")+
  xlim(0,730)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.74),
        plot.title = element_text(hjust = 0.5))+
  ylab("Density")+
  xlab("Timescale (Days)")+
  ggtitle("Best Correlation Times (Soil Moisture ~ SPEI) \n (MT Mesonet)")

#add labels
for(i in 2:6){
  mesonet_summary = mesonet_summary + 
    geom_point(data = density_mesonet[[i]][which(density_mesonet[[i]]$y == max(density_mesonet[[i]]$y)),], aes(x = x, y = y))+
    geom_text(data = data.frame(density_mesonet[[i]][which(density_mesonet[[i]]$y == max(density_mesonet[[i]]$y)),],n = sum(!is.na(best_times_matrix_mesonet[,i])), r = round(mean(best_cor_matrix_mesonet[,i], na.rm = T), 2)), aes(x = x + 150, y = y,
                                                                                                              label = paste0(round(x, digits = 0), " Days, n = ", n, ", mean r = ", r)))
}

png(filename = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/time_scale_summary_with_mean_mesonet.png", 
    width = 8, height = 7, units = "in", res = 300)
mesonet_summary
dev.off()

states = sf::st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")

sites = leaflet::leaflet(options = leaflet::tileOptions(minZoom = 4, maxZoom = 10)) %>%
  leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
  leaflet::addProviderTiles("Stamen.TonerLines") %>%
  leaflet::addProviderTiles("Stamen.TonerLabels") %>%
  
  leaflet::addCircleMarkers(master_list$longitude, master_list$latitude, radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = "black", fillColor = "blue", popup = htmltools::htmlEscape(master_list)
  )%>%
  

  leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  leaflet.extras::addDrawToolbar(markerOptions = leaflet.extras::drawMarkerOptions(),
                                 polylineOptions = FALSE,
                                 polygonOptions = FALSE,
                                 circleOptions = FALSE,
                                 rectangleOptions = FALSE,
                                 circleMarkerOptions = FALSE,
                                 editOptions = FALSE,
                                 singleFeature = TRUE,
                                 targetGroup='draw')

htmlwidgets::saveWidget(sites, "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites.html", selfcontained = T)
webshot::webshot("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites.html", 
                 file = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites.png",
        cliprect = "viewport")





























#set up parameters for leaflet map








master_geospatial = data.frame(name  = snotel$site_name,
                               lat = snotel$lat,
                               long = snotel$lon,
                               in_0 = NA,
                               in_2 = best_times_matrix$`2in`,
                               in_4 = NA,
                               in_8 = best_times_matrix$`8in`,
                               in_20 = best_times_matrix$`20in`,
                               in_36 = NA,
                               mean = best_times_matrix$mean)

master_geospatial = rbind(master_geospatial, data.frame(name = station_data$station_key,
                                                        lat = station_data$latitude,
                                                        long = station_data$longitude,
                                                        in_0 = best_times_matrix_mesonet$`0in`,
                                                        in_2 = NA,
                                                        in_4 = best_times_matrix_mesonet$`4in`,
                                                        in_8 = best_times_matrix_mesonet$`8in`,
                                                        in_20 = best_times_matrix_mesonet$`20in`,
                                                        in_36 = best_times_matrix_mesonet$`36in`,
                                                        mean = best_times_matrix_mesonet$mean))

delete_index = master_geospatial%>%
  select(in_0, in_2, in_4, in_8, in_20, in_36, mean)

master_geospatial = (master_geospatial[rowSums((is.na(delete_index)))!= ncol(delete_index), ])

pal <- leaflet::colorNumeric(c("red", "yellow", "blue"), domain = c(0,365), na.color = "grey")

#Map
leaflet::leaflet(options = leaflet::tileOptions(minZoom = 4, maxZoom = 10)) %>%
  leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
  leaflet::addProviderTiles("Stamen.TonerLines") %>%
  leaflet::addProviderTiles("Stamen.TonerLabels") %>%
  
  leaflet::addCircleMarkers(master_geospatial$long, master_geospatial$lat, radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(master_geospatial$mean), group = "0 in", popup = htmltools::htmlEscape(master_geospatial$name)
  )%>%
  leaflet::addLegend("bottomleft", pal = pal, values = master_geospatial$mean,
                     title = "Timescale (Days)",
                     opacity = 1,
                     na.label = "NA"
  )%>%

  leaflet::setMaxBounds( lng1 = -122
                         , lat1 = 53
                         , lng2 = -91
                         , lat2 = 39)%>%
  leaflet::setView(lng = -108, lat = 45.5, zoom = 6) %>%
  leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  leaflet::addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", group = "Weather",
    layers = "nexrad-n0r-900913",
    options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE))%>%
  leaflet::addLayersControl(position = "topleft",
                            overlayGroups = c("States", "Weather"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  leaflet.extras::addDrawToolbar(markerOptions = leaflet.extras::drawMarkerOptions(),
                          polylineOptions = FALSE,
                          polygonOptions = FALSE,
                          circleOptions = FALSE,
                          rectangleOptions = FALSE,
                          circleMarkerOptions = FALSE,
                          editOptions = FALSE,
                          singleFeature = TRUE,
                          targetGroup='draw')









write.csv(best_times_matrix, "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/correlation_times.csv", row.names = F)

# moving window summary

best_times_from_density = c(density_2in$x[which(density_2in$y == max(density_2in$y))],
                            density_8in$x[which(density_8in$y == max(density_8in$y))],
                            density_20in$x[which(density_20in$y == max(density_20in$y))],
                            density_mean$x[which(density_mean$y == max(density_mean$y))])
times = c(seq(15,360,15))

best = lapply(c(1:4),FUN = function(x) which.min(abs(times - best_times_from_density[x])))

best_time_moving = list()

for(i in 1:length(snotel$lat)){
  tryCatch({
    station_best = data.frame(month = moving_correlation_matrix[[i]][[best[1][[1]]]]$month,
                              soil_moisture_2in = moving_correlation_matrix[[i]][[best[1][[1]]]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values ,
                              soil_moisture_8in = moving_correlation_matrix[[i]][[best[2][[1]]]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values,
                              soil_moisture_20in = moving_correlation_matrix[[i]][[best[3][[1]]]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values,
                              mean_soil_moisture = moving_correlation_matrix[[i]][[best[4][[1]]]]$mean_soil_moisture)
    best_time_moving[[i]] = station_best
  },
  error = function(e){
    return(c(NA))
  }) 
}

#remove incomplete data (stations without all months)
for(i in 1:length(snotel$lat)){
  tryCatch({
    if(nrow(best_time_moving[[i]])<12){
      best_time_moving[[i]] = NULL
    }},
    error = function(e){
      return(NA)
    })
}

best_time_moving = best_time_moving[-which(sapply(best_time_moving, is.null))]

time_moving_quantile = as.data.frame(plyr::aaply(plyr::laply(best_time_moving, as.matrix), c(2, 3), quantile, na.rm = T))


#######################################################
seasonal_plot = list()

seasonal_plot[[1]] = ggplot() +
  geom_ribbon(aes(ymax = time_moving_quantile$`soil_moisture_2in.75%`, 
                  ymin = time_moving_quantile$`soil_moisture_2in.25%`,
                  x = time_moving_quantile$`month.25%`), alpha = 0.5,fill = "orange")+
  geom_line(aes(x = time_moving_quantile$`month.25%`, y = time_moving_quantile$`soil_moisture_2in.50%`))+
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month")+
  ylab("Correlation")+
  ggtitle(paste0("Soil Moisture (2in) ~ SPEI"))+
  scale_x_continuous(breaks=c(1:12))

seasonal_plot[[2]] = ggplot() +
  geom_ribbon(aes(ymax = time_moving_quantile$`soil_moisture_8in.75%`, 
                  ymin = time_moving_quantile$`soil_moisture_8in.25%`,
                  x = time_moving_quantile$`month.25%`), alpha = 0.5,fill = "red")+
  geom_line(aes(x = time_moving_quantile$`month.25%`, y = time_moving_quantile$`soil_moisture_8in.50%`))+
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month")+
  ylab("Correlation")+
  ggtitle(paste0("Soil Moisture (8in) ~ SPEI"))+
  scale_x_continuous(breaks=c(1:12))

seasonal_plot[[3]] = ggplot() +
  geom_ribbon(aes(ymax = time_moving_quantile$`soil_moisture_20in.75%`, 
                  ymin = time_moving_quantile$`soil_moisture_20in.25%`,
                  x = time_moving_quantile$`month.25%`), alpha = 0.5,fill = "blue")+
  geom_line(aes(x = time_moving_quantile$`month.25%`, y = time_moving_quantile$`soil_moisture_20in.50%`))+
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month")+
  ylab("Correlation")+
  ggtitle(paste0("Soil Moisture (20in) ~ SPEI"))+
  scale_x_continuous(breaks=c(1:12))

seasonal_plot[[4]] = ggplot() +
  geom_ribbon(aes(ymax = time_moving_quantile$`mean_soil_moisture.75%`, 
                  ymin = time_moving_quantile$`mean_soil_moisture.25%`,
                  x = time_moving_quantile$`month.25%`), alpha = 0.5,fill = "grey")+
  geom_line(aes(x = time_moving_quantile$`month.25%`, y = time_moving_quantile$`mean_soil_moisture.50%`))+
  theme_bw(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Month")+
  ylab("Correlation")+
  ggtitle(paste0("Soil Moisture (Mean) ~ SPEI"))+
  scale_x_continuous(breaks=c(1:12))

png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/seasonality.png"),
    width = 12, height = 10, units = "in", res = 300)
cowplot::plot_grid(seasonal_plot[[4]],seasonal_plot[[1]], seasonal_plot[[2]], seasonal_plot[[3]], labels = c("A", "B", "C", "D"))
dev.off()

#plotting function for timeseries
time_series_plot = function(site_number, depth){
  site_id = which(snotel$site_num == site_number)
  
  if(depth == 2){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,1]
  }
  
  if(depth == 8){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,2]
  }
  
  if(depth == 20){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,3]
  }
  
  #index
  best_spei = which(best_spei_time == c(seq(15,360,15)))
  #extract
  spei = snotel_spei[[site]][[best_spei]]
  #as Date
  spei$time = as.Date(spei$time)
  #filter for comon time
  spei_filter <- spei[spei$time %in% soil_moisture$time,]
  soil_moisture_filter <- soil_moisture[soil_moisture$time %in% spei$time,]
  
  data = cbind(soil_moisture_filter, spei = spei_filter$spei) %>%
    #filter negative soil moisture data
    dplyr::filter(soil_moisture > 0) %>%
    #filter complete cases
    dplyr::filter(complete.cases(.))%>%
    #compute standardized value
    mutate(standardized = gamma_standard_fun(soil_moisture))
  
  
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot(data$time, data$standardized, type = "l", ylim = c(min(data$standardized),max(data$standardized)), xlab = "Time", ylab = "Standardized Soil Moisture",
       xlim = c(data$time[1], data$time[length(data$time)]), main = paste0("Depth = ",depth , "in\nBest timescale = ", best_spei_time, " days")) # first plot
  
  par(new = TRUE)
  
  plot(data$time, data$spei , type = "l", 
       axes = FALSE, bty = "n", xlab = "", ylab = "",  xlim = c(data$time[1], data$time[length(data$time)]), col = "red")
  abline(0, 0,col = "red", lty = 2)
  axis(side=4, at = pretty(range(data$spei)), col = "red")
  mtext("SPEI", side=4, line=3, col = "red")
}

for(i in 1:length(snotel$site_num)){
  tryCatch({
    png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/time_series/Soil_Moisture_SPEI_timeseries_", 
                          i,".png"), width = 10, height = 3, units = "in", res = 300)
    par(mfrow = c(1,3))
    time_series_plot(snotel$site_num[i], 2)
    time_series_plot(snotel$site_num[i], 8)
    time_series_plot(snotel$site_num[i], 20)
    dev.off()
  }, error = function(e){
    plot(1,1, xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    text(1, 1.2, "Sorry, no data to correlate")
    dev.off()
  })
  print(i)
}

foreach(i = 1:length(snotel$lat)) %dopar% {
  tryCatch({
    png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/correlation/Soil_Moisture_SPEI_Correlation_",
                          i,".png"), width = 6, height = 5, units = "in", res = 300)
    plot(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][1,]), type = "l", col = "orange",
         ylim = c(min(correlation_matrix[[i]]),max(correlation_matrix[[i]])),
         xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][2,]), type = "l", col = "red")
    lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][3,]), type = "l", col = "blue")
    lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][4,]), type = "l", col = "black")
    legend(300, max(correlation_matrix[[i]]), legend=c("2 in", "8 in", "20 in", "mean"),
           col=c("orange", "red", "blue", "black"), lty=c(1,1,1,1), cex=0.8)
    dev.off()
  }, error = function(e){
    plot(1,1, xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    text(1, 1.2, "Sorry, no data to correlate")
    dev.off()
  })
}

