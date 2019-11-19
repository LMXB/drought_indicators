library(ggplot2)
library(dplyr)

# load in misc functions to sort data and find best
source("./validation/soil_moisture/R/misc_functions.R")

# correlation matrix data
list.files("~/drought_indicators_data/correlation_matrix/",pattern = ".RData", full.names = T)%>%
  lapply(., load, .GlobalEnv)

# set up storage lists for best cors
best_times_list = list()
best_cor_list = list()
best_times_mesonet_list = list()
best_cor_mesonet_list = list()

for(i in 1:4){
  best_times_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 6))
  colnames(best_times_list[[i]]) = c("2in","4in", "8in", "20in", "40in", "mean")
  
  best_cor_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_spi), ncol = 6))
  colnames(best_cor_list[[i]]) = c("2in","4in", "8in", "20in", "40in", "mean")
  
  best_times_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 6))
  colnames(best_times_mesonet_list[[i]]) = c("0in", "4in", "8in", "20in", "36in", "mean")
  
  best_cor_mesonet_list[[i]] = data.frame(matrix(nrow = length(correlation_matrix_mesonet_spi), ncol = 6))
  colnames(best_cor_mesonet_list[[i]]) = c("0in", "4in", "8in", "20in", "36in", "mean")
}

names(best_times_list) = c("spi","spei","eddi","sedi")
names(best_cor_list) = c("spi","spei","eddi","sedi")
names(best_times_mesonet_list) = c("spi","spei","eddi","sedi")
names(best_cor_mesonet_list) = c("spi","spei","eddi","sedi")

# find best correlations and times for snotel
for(i in 1:length(correlation_matrix_spi)){
  tryCatch({
    best_times_list$spi[i,] = find_best(correlation_matrix_spi[[i]])
    best_times_list$spei[i,] = find_best(correlation_matrix_spei[[i]])
    best_times_list$eddi[i,] = find_best_neg(correlation_matrix_eddi[[i]])
    best_times_list$sedi[i,] = find_best_neg(correlation_matrix_sedi[[i]])
    
    best_cor_list$spi[i,] = find_best_cor(correlation_matrix_spi[[i]])
    best_cor_list$spei[i,] = find_best_cor(correlation_matrix_spei[[i]])
    best_cor_list$eddi[i,] = find_best_cor_neg(correlation_matrix_eddi[[i]])
    best_cor_list$sedi[i,] = find_best_cor_neg(correlation_matrix_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}
# find best correlations and times for mesonet
for(i in 1:length(correlation_matrix_mesonet_spi)){
  tryCatch({
    best_times_mesonet_list$spi[i,] = find_best_mesonet(correlation_matrix_mesonet_spi[[i]])
    best_times_mesonet_list$spei[i,] = find_best_mesonet(correlation_matrix_mesonet_spei[[i]])
    best_times_mesonet_list$eddi[i,] = find_best_mesonet_neg(correlation_matrix_mesonet_eddi[[i]])
    best_times_mesonet_list$sedi[i,] = find_best_mesonet_neg(correlation_matrix_mesonet_sedi[[i]])
    
    best_cor_mesonet_list$spi[i,] = find_best_mesonet_cor(correlation_matrix_mesonet_spi[[i]])
    best_cor_mesonet_list$spei[i,] = find_best_mesonet_cor(correlation_matrix_mesonet_spei[[i]])
    best_cor_mesonet_list$eddi[i,] = find_best_mesonet_cor_neg(correlation_matrix_mesonet_eddi[[i]])
    best_cor_mesonet_list$sedi[i,] = find_best_mesonet_cor_neg(correlation_matrix_mesonet_sedi[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

# aggregate based on generalized depths
best_times_combined = list()
best_cor_combined = list()

for(i in 1:4){
  best_times_combined[[i]] = data.frame(shallow = c(best_times_list[[i]]$`2in`, best_times_list[[i]]$`4in`,
                                                    best_times_mesonet_list[[i]]$`0in`, best_times_mesonet_list[[i]]$`4in`),
                                        middle = c(best_times_list[[i]]$`8in`, best_times_list[[i]]$`20in`,
                                                   best_times_mesonet_list[[i]]$`8in`, best_times_mesonet_list[[i]]$`20in`),
                                        deep = c(best_times_list[[i]]$`40in`, best_times_mesonet_list[[i]]$`36in`),
                                        mean = c(best_times_list[[i]]$mean, best_times_mesonet_list[[i]]$mean))
  
  best_cor_combined[[i]] = data.frame(shallow = c(best_cor_list[[i]]$`2in`, best_cor_list[[i]]$`4in`,
                                                  best_cor_mesonet_list[[i]]$`0in`, best_cor_mesonet_list[[i]]$`4in`),
                                      middle = c(best_cor_list[[i]]$`8in`, best_cor_list[[i]]$`20in`,
                                                 best_cor_mesonet_list[[i]]$`8in`, best_cor_mesonet_list[[i]]$`20in`),
                                      deep = c(best_cor_list[[i]]$`40in`, best_cor_mesonet_list[[i]]$`36in`),
                                      mean = c(best_cor_list[[i]]$mean, best_cor_mesonet_list[[i]]$mean))
}

# rename and organize
names(best_times_combined) = c("spi","spei","eddi","sedi")
names(best_cor_combined) = c("spi","spei","eddi","sedi")
depths = c("2 - 4in","8 - 20in", "36 - 40in", "Mean")

# plot density graphs by depth
plot = list()
for(i in 1:4){
  best_cor_ggplot = c(round(median(best_cor_combined$spi[,i], na.rm = T),2),
                      round(median(best_cor_combined$spei[,i], na.rm = T),2),
                      round(median(best_cor_combined$eddi[,i], na.rm = T),2),
                      round(median(best_cor_combined$sedi[,i], na.rm = T),2))
  
  names = c("SPI: r = ","SPEI: r = ","EDDI: r = ","SEDI: r = ")
  
  names_short = c("SPI","SPEI","EDDI","SEDI")
  
  data = rbind(extract_density(best_times_combined$spi[,i], "SPI"),
               extract_density(best_times_combined$spei[,i], "SPEI"),
               extract_density(best_times_combined$eddi[,i], "EDDI"),
               extract_density(best_times_combined$sedi[,i], "SEDI"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  plot[[i]] = ggplot(data = data, aes(x = x, y=y, color = name))+
    geom_line()+
    ggtitle(paste0("Soil Moisture Depth = ", depths[i]))+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 14)+
    xlim(0,600)+
    theme(legend.position = c(0.75, 0.75))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = names_short,
                       values = c("blue", "black", "orange", "red"),
                       labels = paste0(names, best_cor_ggplot),
                       name = "Drought Metric")
}
plot_grid = cowplot::plot_grid(plot[[4]],plot[[1]],plot[[2]],plot[[3]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/correlation_density_unfrozen.png",
       plot_grid, width = 10, height = 9, units = "in", dpi = 600)


# plot density graphs by metric
depth_plot = list()
names_short = c("SPI","SPEI","EDDI","SEDI")
for(i in 1:length(names_short)){
  
  best_cor_ggplot = c(round(median(best_cor_combined[[i]]$mean, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$shallow, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$middle, na.rm = T),2),
                      round(median(best_cor_combined[[i]]$deep, na.rm = T),2))
  
  data = rbind(extract_density(best_times_combined[[i]]$mean, "Mean"),
               extract_density(best_times_combined[[i]]$shallow, "Shallow"),
               extract_density(best_times_combined[[i]]$middle, "Middle"),
               extract_density(best_times_combined[[i]]$deep, "Deep"))
  
  best_density = data %>%
    group_by(name) %>%
    select(y, name) %>%
    summarise_each(max)
  
  best_times = data %>%
    dplyr::filter(y %in% best_density$y)
  
  depth_plot[[i]] = ggplot(data = data, aes(x = x, y = y, color = name))+
    geom_line()+
    ggtitle(names_short[i])+
    xlab("Timescale (Days)")+
    ylab("Density")+
    theme_bw(base_size = 14)+
    xlim(0,600)+
    theme(legend.position = c(0.75, 0.75))+
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'),
          plot.title = element_text(hjust = 0.5))+
    ggrepel::geom_text_repel(data = best_times, aes(x = x, y=y, color = name, label = mround(x, 5)))+
    geom_point(data = best_times, aes(x = x, y=y, color = name))+
    scale_color_manual(breaks = c("Mean", "Shallow", "Middle", "Deep"),
                       values = c("black", "forestgreen", "blue", "purple"),
                       labels = paste0(c("Mean: r = ", "Shallow: r = ", "Middle: r = ", "Deep: r = "),
                                       best_cor_ggplot),
                       name = "Probe Depth")
  
}

plot_grid_depth = cowplot::plot_grid(depth_plot[[1]],depth_plot[[2]],depth_plot[[3]],depth_plot[[4]], nrow = 2)
ggsave("./validation/soil_moisture/plots/summary/depth_density_unfrozen.png",
       plot_grid_depth, width = 10, height = 9, units = "in", dpi = 600)

# leaflet map 
library(dplyr)
#read in geospatial data
station_data = read.csv("~/drought_indicators_data/mesonet/station_data_clean.csv")
station_data$X = NULL
snotel = read.csv("./validation/soil_moisture/snotel_data/nrcs_soil_moisture.csv")
snotel_cropped = snotel %>%
  dplyr::select(site_name, latitude, longitude) %>%
  rename(station_key = site_name)

snotel_cropped$network = "NRCS"
master_list = rbind(snotel_cropped,station_data)

master_times = cbind(master_list, 
                     mean_time_spi = c(best_times_list$spi$mean, best_times_mesonet_list$spi$mean),
                     mean_time_spei = c(best_times_list$spei$mean, best_times_mesonet_list$spei$mean),
                     mean_time_eddi = c(best_times_list$eddi$mean, best_times_mesonet_list$eddi$mean),
                     mean_time_sedi = c(best_times_list$sedi$mean, best_times_mesonet_list$sedi$mean),
                     mean_cor_spi = c(best_cor_list$spi$mean, best_cor_mesonet_list$spi$mean),
                     mean_cor_spei = c(best_cor_list$spei$mean, best_cor_mesonet_list$spei$mean),
                     mean_cor_eddi = c(best_cor_list$eddi$mean, best_cor_mesonet_list$eddi$mean),
                     mean_cor_sedi = c(best_cor_list$sedi$mean, best_cor_mesonet_list$sedi$mean))

write.csv(master_times, "./validation/soil_moisture/summary_data/geospatial_with_best.csv")

states = sf::st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp") %>%
  dplyr::filter(STATE_NAME != "Alaska" & STATE_NAME != "Hawaii")

pal <- leaflet::colorNumeric(c("red", "yellow", "green", "blue", "purple"), 
                             domain = c(0, 730), na.color = "transparent")

sites = leaflet::leaflet(master_times,options = leaflet::tileOptions(minZoom = 4, maxZoom = 10)) %>%
  leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
  leaflet::addProviderTiles("Stamen.TonerLines") %>%
  leaflet::addProviderTiles("Stamen.TonerLabels") %>%
  leaflet::addCircleMarkers(~longitude, ~latitude, 
                            radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = "blue", fillColor = "white", group = "Locations"
  )%>%
  leaflet::addCircleMarkers(~longitude, ~latitude, 
                            radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = pal(master_times$mean_spi), fillColor = pal(master_times$mean_spi), group = "SPI"
  )%>%
  leaflet::addCircleMarkers(~longitude, ~latitude, 
                            radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = pal(master_times$mean_spei), fillColor = pal(master_times$mean_spei), group = "SPEI"
  )%>%
  leaflet::addCircleMarkers(~longitude, ~latitude, 
                            radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = pal(master_times$mean_eddi), fillColor = pal(master_times$mean_eddi), group = "EDDI"
  )%>%
  leaflet::addCircleMarkers(~longitude, ~latitude, 
                            radius = 8, stroke = TRUE, fillOpacity = 0.9,
                            color = pal(master_times$mean_sedi), fillColor = pal(master_times$mean_sedi), group = "SEDI"
  )%>%
  leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  leaflet.extras::addDrawToolbar(markerOptions = leaflet.extras::drawMarkerOptions(),
                                 polylineOptions = FALSE,
                                 polygonOptions = FALSE,
                                 circleOptions = FALSE,
                                 rectangleOptions = FALSE,
                                 circleMarkerOptions = FALSE,
                                 editOptions = FALSE,
                                 singleFeature = F,
                                 targetGroup='draw') %>%
  leaflet::addLegend(pal = pal, values = master_times$mean_spei, position = "bottomleft",
                     title = "Mean Soil Moisture<br>Best Timescale (days)"
  )%>%
  leaflet::addLayersControl(
    baseGroups = c("Locations","SPI", "SPEI", "EDDI", "SEDI"),
    options = leaflet::layersControlOptions(collapsed = FALSE))



htmlwidgets::saveWidget(sites, "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites_best_times.html", selfcontained = T)

webshot::webshot("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites_best_times.html", 
                 file = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/validation_sites.png",
                 cliprect = "viewport", vwidth = 2000,vheight = 1000)



