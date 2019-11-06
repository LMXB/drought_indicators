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

daily_lookup$percent_swe[daily_lookup$percent_swe > 200] = 200
daily_lookup$percent_precip[daily_lookup$percent_precip > 200] = 200

daily_lookup$percent_swe[daily_lookup$percent_swe == "NaN"] = NA
daily_lookup$percent_precip[daily_lookup$percent_precip == "NaN"] = NA

na.index = which(is.na(daily_lookup$daily_mean_swe), arr.ind=TRUE)

snotel$lat[na.index] = NA
snotel$lon[na.index] = NA

#color pallet
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

swe_map = base_map() %>%
addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                 popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/snotel/plots/snotel_plot_",
                                snotel$simple_id,".png' height='350' width='600'/>"),
                 radius = 10, stroke = TRUE, fillOpacity = 0.9,
                 color = "black", fillColor = pal(daily_lookup$percent_swe)
)%>%
addLegend("bottomleft", pal = pal, values = daily_lookup$percent_swe,
        title = "% Average<br>Accumulated SWE<br> (Daily)",
        opacity = 1,
        na.label = "NA"
)%>%
setView(lng = -108, lat = 46.5, zoom = 6) %>%
prependContent(tags$style(type = "text/css", css_fix))


htmlwidgets::saveWidget(swe_map, "/home/zhoylman/drought_indicators/snotel/widgets/swe_snotel.html", selfcontained = T)
save(swe_map, file = "/home/zhoylman/drought_indicators/snotel/widgets/swe_snotel.RData")
 
saveWidget(swe_map, "/home/zhoylman/drought_indicators/widgets/swe_snotel.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")


pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_precip, na.rm = T),max(daily_lookup$percent_precip, na.rm = T)), na.color = "grey")

precip_map = base_map() %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                   popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/snotel/plots/precip_snotel_plot_",
                                  snotel$simple_id,".png' height='350' width='600'/>"),
                   radius = 10, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_precip)
  )%>%
  addLegend("bottomleft", pal = pal, values = daily_lookup$percent_precip,
            title = "% Average<br>Accumulated Precipitation<br> (Daily)",
            opacity = 1,
            na.label = "NA"
  )%>%
  setView(lng = -108, lat = 46.5, zoom = 6) %>%
  prependContent(tags$style(type = "text/css", css_fix))


htmlwidgets::saveWidget(precip_map, "/home/zhoylman/drought_indicators/snotel/widgets/precip_snotel.html", selfcontained = T)
save(precip_map, file = "/home/zhoylman/drought_indicators/snotel/widgets/precip_snotel.RData")

saveWidget(precip_map, "/home/zhoylman/drought_indicators/widgets/precip_snotel.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")



##### Mobile SWE ######
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")

source("/home/zhoylman/drought_indicators/mapping_functions/base_map_mobile.R")
swe_map_mobile = base_map_mobile() %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, group = "Snow Water Equiv",
                   popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/snotel/plots/snotel_plot_mobile_",
                                  snotel$simple_id,".png' height='200' width='270'/>"),
                   radius = 12, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_swe)
  )%>%
  leaflet::addLayersControl(position = "topleft",
                            overlayGroups = c("Snow Water Equiv", "States", "Weather"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal, values = daily_lookup$percent_swe,
            title = "% Average<br>SWE (Daily)",
            opacity = 1
  )%>%
  setView(lng = -113.990211, lat = 46.864089, zoom = 7)%>%
  onRender("function(el, x) {
    this.removeControl(this.zoomControl);
  }") %>%
  prependContent(tags$style(type = "text/css", css_fix))


swe_map_mobile
saveWidget(swe_map_mobile, "/home/zhoylman/drought_indicators/widgets/swe_snotel_mobile.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")
