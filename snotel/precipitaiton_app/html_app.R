library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflet.esri)
library(mapview)

source("/home/zhoylman/drought_indicators/mapping_functions/base_map.R")

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))
snotel$simple_id = c(1:length(snotel$site_num))

#load current conditions 
load("/home/zhoylman/drought_indicators/snotel/climatology/current_precent_SWE.RData")

daily_lookup = data.frame(daily_mean = unlist(daily_lookup[2]),
                          Precip = unlist(daily_lookup[4]),
                          percent = unlist(daily_lookup[6]))

daily_lookup$percent_crop = daily_lookup$percent
daily_lookup$percent_crop[daily_lookup$percent_crop >200] = 200
daily_lookup$percent_crop[daily_lookup$percent_crop == "NaN"] = NA

na.index = which(is.na(daily_lookup$daily_mean), arr.ind=TRUE)

snotel$lat[na.index] = NA
snotel$lon[na.index] = NA

daily_lookup$percent_crop[which(daily_lookup$current == 0)] = NA
daily_lookup$percent_crop[which(daily_lookup$percent_crop == 0)] = NA


#color pallet
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_crop, na.rm = T),max(daily_lookup$percent_crop, na.rm = T)), na.color = "grey")

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

precip_map = base_map() %>%
addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                 #popup = popupImage(src = "remote",img = paste0("http://shiny.cfc.umt.edu:3838/drought_indicators/snotel/plots/precip_snotel_plot_",
                #                                                snotel$simple_id,".png"), height = 300, width = 500),
                 popup = paste0("<img src='http://shiny.cfc.umt.edu:3838/drought_indicators/snotel/plots/precip_snotel_plot_",
                                snotel$simple_id,".png' height='400' width='600'/>"),
                 radius = 10, stroke = TRUE, fillOpacity = 0.9,
                 color = "black", fillColor = pal(daily_lookup$percent_crop)
)%>%
addLegend("bottomleft", pal = pal, values = daily_lookup$percent_crop,
        title = "% Average<br>Accumulated Precipitation<br> (Daily)",
        opacity = 1,
        na.label = "NA"
)%>%
setView(lng = -108, lat = 46.5, zoom = 6)

htmlwidgets::saveWidget(precip_map, "/home/zhoylman/temp/test.html", selfcontained = T)
      
      
 


