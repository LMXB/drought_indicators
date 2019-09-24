library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflet.esri)

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

plots = list.files("/home/zhoylman/drought_indicators/snotel/plots", full.names = T)
plots_pngs = png::readPNG(plots[1])

base_map() %>%
addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, popup = htmlEscape(snotel$site_name), radius = 10, stroke = TRUE, fillOpacity = 0.9,
               color = "black", fillColor = pal(daily_lookup$percent_crop)
)%>%
addLegend("bottomleft", pal = pal, values = daily_lookup$percent_crop,
        title = "% Average<br>Accumulated Precipitation<br> (Daily)",
        opacity = 1,
        na.label = "NA"
)%>%
setView(lng = -108, lat = 46.5, zoom = 6)
      
      
 


