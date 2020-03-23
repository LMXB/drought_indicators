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

setwd('/home/zhoylman/drought_indicators/eddi_app')

#load custom functions
source("../mapping_functions/base_map.R")
source("../eddi_app/R/eddi_calc_plot.R")
source('/home/zhoylman/drought_indicators/tribal/R/aggregate_tribal.R') #!!!

counties_shp = st_read("../shp_kml/larger_extent/county_umrb.shp")


#eddi data
current_eddi_30 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_30.tif")
current_eddi_60 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_60.tif")
current_eddi_90 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_90.tif")
current_eddi_180 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_180.tif")
current_eddi_365 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_365.tif")
current_eddi_water_year = raster::raster("../eddi_app/maps/current_eddi/current_eddi_water_year.tif")
current_eddi_year_to_date = raster::raster("../eddi_app/maps/current_eddi/current_eddi_year_to_date.tif")
watersheds_30 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_30.shp")
watersheds_60 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_60.shp")
watersheds_90 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_90.shp")
watersheds_180 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_180.shp")
watersheds_365 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_365.shp")
watersheds_water_year = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_water_year.shp")
watersheds_year_to_date = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_year_to_date.shp")
county_30 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_30.shp")
county_60 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_60.shp")
county_90 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_90.shp")
county_180 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_180.shp")
county_365 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_365.shp")
county_water_year = st_read("../eddi_app/shp/current_eddi/current_eddi_county_water_year.shp")
county_year_to_date = st_read("../eddi_app/shp/current_eddi/current_eddi_county_year_to_date.shp")

#process tribal  !!
tribal_30 = aggregate_tribal(current_eddi_30)
tribal_60 = aggregate_tribal(current_eddi_60)
tribal_90 = aggregate_tribal(current_eddi_90)
tribal_180 = aggregate_tribal(current_eddi_180)
tribal_365 = aggregate_tribal(current_eddi_365)
tribal_water_year = aggregate_tribal(current_eddi_water_year)
tribal_year_to_date = aggregate_tribal(current_eddi_year_to_date)

tribal_list = list(tribal_30, tribal_60, tribal_90,
                   tribal_180, tribal_365, tribal_water_year, tribal_year_to_date)

labels_tribal = list()
for(i in 1:length(tribal_list)){
  labels_tribal[[i]] <- sprintf(
    "<strong>%s</strong><br/>EDDI = %g<sup></sup>",
    tribal_list[[i]]$GNIS_Name1, tribal_list[[i]]$average
  ) %>% lapply(htmltools::HTML)
}

#define color pallets
pal_bins <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                     domain = -2.5:2.5, bins = seq(-2.5,2.5,0.5))


pal <- colorNumeric(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), -2.51:2.51, na.color = "transparent")

#lists of layers for loop leaflet map generation
watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_365, watersheds_water_year, watersheds_year_to_date)
county_list = list(county_30, county_60, county_90, county_180, county_365, county_water_year, county_year_to_date)
raster_list = list(current_eddi_30, current_eddi_60,current_eddi_90, current_eddi_180, current_eddi_365, current_eddi_water_year, current_eddi_year_to_date)

watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year", "Year to Date")
timescale_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")

#labels for aggregated data
labels = list()
for(i in 1:length(watershed_list_names)){
  labels[[i]] <- sprintf(
    "<strong>%s</strong><br/>EDDI = %g<sup></sup>",
    watershed_list[[i]]$NAME, watershed_list[[i]]$average
  ) %>% lapply(htmltools::HTML)
}

labels_county = list()
for(i in 1:length(watershed_list_names)){
  labels_county[[i]] <- sprintf(
    "<strong>%s</strong><br/>EDDI = %g<sup></sup>",
    county_list[[i]]$NAME, county_list[[i]]$average
  ) %>% lapply(htmltools::HTML)
}

for(i in 1:length(watershed_list)){
  #set upper bound for color ramp
  values(raster_list[[i]])[values(raster_list[[i]]) > 2.5] = 2.5
  values(raster_list[[i]])[values(raster_list[[i]]) < -2.5] = -2.5
  
  county_list[[i]]$average[county_list[[i]]$average > 2.5] = 2.5
  county_list[[i]]$average[county_list[[i]]$average < -2.5] = -2.5
  
  watershed_list[[i]]$average[watershed_list[[i]]$average > 2.5] = 2.5
  watershed_list[[i]]$average[watershed_list[[i]]$average < -2.5] = -2.5
  
  tribal_list[[i]]$average[tribal_list[[i]]$average > 2.5] = 2.5 #!!
  tribal_list[[i]]$average[tribal_list[[i]]$average < -2.5] = -2.5 #!!
}

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
  addPolygons(data = counties_shp, group = "Counties", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  addPolygons(data = tribal, group = "Tribal Lands", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties", 'Tribal Lands'),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  leaflet::hideGroup(c("Counties", "Streets", 'Tribal Lands'))%>%
  addLegend(pal = pal, values = -2.5:2.5,
            title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

save(m_raster, file = "/home/zhoylman/drought_indicators/eddi_app/widgets/m_raster.RData")

saveWidget(as_widget(m_raster), "/home/zhoylman/drought_indicators/eddi_app/widgets/m_raster.html", selfcontained = T)

saveWidget(as_widget(m_raster), "/home/zhoylman/drought_indicators/widgets/m_raster_eddi.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")


################################################################################
############################### BUILD HUC MAP ##################################
################################################################################

m_huc = base_map()

# Add multiple layers with a loop ----------------------------------------------
for(i in 1:length(watershed_list_names)){
  m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = timescale_names[i], fillColor = ~pal(average), weight = 2, opacity = 1, color = "black", 
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
  addLegend(pal = pal, values = -2.5:2.5,
            title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

save(m_huc, file = "/home/zhoylman/drought_indicators/eddi_app/widgets/m_huc.RData")

saveWidget(as_widget(m_huc), "/home/zhoylman/drought_indicators/eddi_app/widgets/m_huc.html", selfcontained = T)

saveWidget(m_huc, "/home/zhoylman/drought_indicators/widgets/m_huc_eddi.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")


################################################################################
############################### BUILD COUNTY MAP ###############################
################################################################################

m_county = base_map()

# Add multiple layers with a loop ----------------------------------------------
for(i in 1:length(watershed_list_names)){
  m_county = m_county %>% addPolygons(data = county_list[[i]], group = timescale_names[i], fillColor = ~pal(average), weight = 2, opacity = 1, color = "black", 
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
  addLegend(pal = pal, values = -2.5:2.5,
            title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

save(m_county, file = "/home/zhoylman/drought_indicators/eddi_app/widgets/m_county.RData")

saveWidget(as_widget(m_county), "/home/zhoylman/drought_indicators/eddi_app/widgets/m_county.html", selfcontained = T)

saveWidget(m_county, "/home/zhoylman/drought_indicators/widgets/m_county_eddi.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")


################################################################################
############################### BUILD TRIBAL MAP ###############################
################################################################################

m_tribal = base_map()

# Add multiple layers with a loop ----------------------------------------------
for(i in 1:length(watershed_list_names)){
  m_tribal = m_tribal %>% addPolygons(data = tribal_list[[i]], group = timescale_names[i], fillColor = ~pal(average), weight = 2, opacity = 1, color = "black", 
                                      dashArray = "3", fillOpacity = 0.7, highlight = 
                                        highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_tribal[[i]], 
                                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
}

# Add Layer Controls  ----------------------------------------------    
m_tribal = m_tribal %>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = pal, values = -2.5:2.5,
            title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
            position = "bottomleft")

saveWidget(m_tribal, "/home/zhoylman/drought_indicators/widgets/m_tribal_eddi.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")
