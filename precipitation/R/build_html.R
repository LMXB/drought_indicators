#load all libraries for all apps
library(shiny)
library(leaflet)
library(leaflet.extras)
library(scales)
library(shinycssloaders)
library(sf)
library(raster)
library(htmltools)
library(rgdal)
library(dplyr)
library(zoo)
library(rowr)
library(precintcon)
library(gridExtra)
library(fitdistrplus)
library(tictoc)
library(ncdf4) 
library(lubridate)
library(plotly)
library(glogis)
library(PearsonDS)
library(gsl)
library(lmomco)
library(mapview)
library(parallel)
library(foreach)
library(doParallel)
library(htmlwidgets)

source("/home/zhoylman/drought_indicators/mapping_functions/base_map.R")

setwd('/home/zhoylman/drought_indicators/precipitation')

#precipitation data
current_anomaly_15 = raster::raster("../precipitation/maps/current_anomaly_15.tif")
current_anomaly_30 = raster::raster("../precipitation/maps/current_anomaly_30.tif")
current_anomaly_60 = raster::raster("../precipitation/maps/current_anomaly_60.tif")
current_anomaly_90 = raster::raster("../precipitation/maps/current_anomaly_90.tif")
current_anomaly_180 = raster::raster("../precipitation/maps/current_anomaly_180.tif")
current_anomaly_365 = raster::raster("../precipitation/maps/current_anomaly_365.tif")
current_anomaly_water_year = raster::raster("../precipitation/maps/current_anomaly_water_year.tif")
current_anomaly_year_to_date = raster::raster("../precipitation/maps/current_anomaly_year_to_date.tif")
watersheds_15 = st_read("../precipitation/shp/current_anomaly_watershed_15.shp")
watersheds_30 = st_read("../precipitation/shp/current_anomaly_watershed_30.shp")
watersheds_60 = st_read("../precipitation/shp/current_anomaly_watershed_60.shp")
watersheds_90 = st_read("../precipitation/shp/current_anomaly_watershed_90.shp")
watersheds_180 = st_read("../precipitation/shp/current_anomaly_watershed_180.shp")
watersheds_365 = st_read("../precipitation/shp/current_anomaly_watershed_365.shp")
watersheds_water_year = st_read("../precipitation/shp/current_anomaly_watershed_water_year.shp")
watersheds_year_to_date = st_read("../precipitation/shp/current_anomaly_watershed_year_to_date.shp")
county_15 = st_read("../precipitation/shp/current_anomaly_county_15.shp")
county_30 = st_read("../precipitation/shp/current_anomaly_county_30.shp")
county_60 = st_read("../precipitation/shp/current_anomaly_county_60.shp")
county_90 = st_read("../precipitation/shp/current_anomaly_county_90.shp")
county_180 = st_read("../precipitation/shp/current_anomaly_county_180.shp")
county_365 = st_read("../precipitation/shp/current_anomaly_county_365.shp")
county_water_year = st_read("../precipitation/shp/current_anomaly_county_water_year.shp")
county_year_to_date = st_read("../precipitation/shp/current_anomaly_county_year_to_date.shp")

lseq = function(from, to, length.out){
  exp(seq(log(from), log(to), length.out = length.out))
}

#define color pallets
pal_bins <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff","#ffffff", "#add8e6", "#0000ff", "#000d66","#000d66","#000d66","#000d66","#000d66","#000d66"), interpolate = "spline"), 
                     domain = 0:400, bins = seq(0,400))


pal <- colorNumeric(c("#8b0000", "#ff0000", "#ffffff","#ffffff", "#add8e6", "#0000ff", "#000d66","#000d66","#000d66","#000d66","#000d66","#000d66"), seq(0,400), na.color = "transparent")


#lists of layers for loop leaflet map generation
watershed_list = list(watersheds_15,watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_365, watersheds_water_year, watersheds_year_to_date)
county_list = list(county_15,county_30, county_60, county_90, county_180, county_365, county_water_year, county_year_to_date)
raster_list = list(current_anomaly_15, current_anomaly_30, current_anomaly_60,current_anomaly_90, current_anomaly_180, current_anomaly_365, current_anomaly_water_year, current_anomaly_year_to_date)

#labels for aggregated data
labels = list()
for(i in 1:length(watershed_list_names)){
  labels[[i]] <- sprintf(
    "<strong>%s</strong><br/>Anomaly = %g&percnt;<sup></sup>",
    watershed_list[[i]]$NAME, watershed_list[[i]]$average
  ) %>% lapply(htmltools::HTML)
}

labels_county = list()
for(i in 1:length(watershed_list_names)){
  labels_county[[i]] <- sprintf(
    "<strong>%s</strong><br/>Anomaly = %g&percnt;<sup></sup>",
    county_list[[i]]$NAME, county_list[[i]]$average
  ) %>% lapply(htmltools::HTML)
}


for(i in 1:length(watershed_list)){
  #set upper bound for color ramp
  values(raster_list[[i]])[values(raster_list[[i]]) > 400] = 400

  county_list[[i]]$average[county_list[[i]]$average > 400] = 400

  watershed_list[[i]]$average[watershed_list[[i]]$average > 400] = 400
}


watershed_list_names = c("15 Day HUC8","30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year", "Year to Date")
timescale_names = c("15 Day","30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")


################################################################################
############################### BUILD RASTER MAP ###############################
################################################################################
# Add multiple layers with a loop ----------------------------------------------
m_raster = base_map()
for(i in 1:length(watershed_list_names)){
  m_raster = m_raster %>%
    addRasterImage(raster_list[[i]], colors = pal, opacity = 0.8, group = timescale_names[i], project = TRUE)
}

# Add some layer controls
m_raster = m_raster %>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = pal, values = 0:400,
            title = paste0("% Average<br>Precipitation<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")


saveWidget(as_widget(m_raster), "/home/zhoylman/drought_indicators/precipitation/widgets/m_raster.html", selfcontained = T)

################################################################################
############################### BUILD HUC MAP ##################################
################################################################################

m_huc = base_map()

# Add multiple layers with a loop ----------------------------------------------
for(i in 1:length(watershed_list_names)){
  m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = timescale_names[i], fillColor = ~pal_bins(average), weight = 2, opacity = 1, color = "black", 
                                dashArray = "3", fillOpacity = 0.7, highlight = 
                                  highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
}

# Add Layer Controls  ----------------------------------------------    
m_huc = m_huc %>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = pal, values = 0:400,
            title = paste0("% Average<br>Precipitation<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

saveWidget(m_huc, "/home/zhoylman/drought_indicators/precipitation/widgets/m_huc.html", selfcontained = T)


################################################################################
############################### BUILD COUNTY MAP ###############################
################################################################################

m_county = base_map()

# Add multiple layers with a loop ----------------------------------------------
for(i in 1:length(watershed_list_names)){
  m_county = m_county %>% addPolygons(data = county_list[[i]], group = timescale_names[i], fillColor = ~pal_bins(average), weight = 2, opacity = 1, color = "black", 
                                      dashArray = "3", fillOpacity = 0.7, highlight = 
                                        highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_county[[i]], 
                                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
}

# Add Layer Controls  ----------------------------------------------    
m_county = m_county %>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = pal, values = 0:400,
            title = paste0("% Average<br>Precipitation<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

saveWidget(as_widget(m_county), "/home/zhoylman/drought_indicators/precipitation/widgets/m_county.html", selfcontained = T)
