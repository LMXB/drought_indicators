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


time_check = format(tail(file.info("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_1_depth_in.tif")$ctime), "%m-%d-%Y")

current_1 = raster::raster("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_1_depth_in.tif")
current_3 = raster::raster("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_3_depth_in.tif")
current_7 = raster::raster("/home/zhoylman/drought_indicators/snodas/data/processed/delta_snow_depth/delta_7_depth_in.tif")


current_1[current_1 > 19.9] = 19.9
current_1[current_1 < -19.9] = -19.9

current_3[current_3 > 19.9] = 19.9
current_3[current_3 < -19.9] = -19.9

current_7[current_7 > 19.9] = 19.9
current_7[current_7 < -19.9] = -19.9

pal_r <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                  domain = -20:20, bins = c(-20,-10,-5,-3,-1,-0.5,0.5,1,3,5,10,20), na.color = "transparent")

pal_r_rev <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                      domain = -20:20, bins = c(-20,-10,-5,-3,-1,-0.5,0.5,1,3,5,10,20), na.color = "transparent")

pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")
pal_rev <- colorNumeric(rev(c("red", "yellow", "green", "blue", "purple")), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

swe_map = base_map() %>%
addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                 popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/plots/snotel_plot_",
                                snotel$simple_id,".png' height='350' width='600'/>"),
                 radius = 10, stroke = TRUE, fillOpacity = 0.9,
                 color = "black", fillColor = pal(daily_lookup$percent_swe)
)%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7 Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("24hr Change","72hr Change", "7 Day Change"),
                            overlayGroups = c("SNOTEL (SWE)", "States",  "Weather"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = daily_lookup$percent_swe,
            title = "% Average<br>SWE (Daily)",
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomright", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",time_check),
            opacity = 1,
            group = "24hr Snow Change",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))%>%
setView(lng = -108, lat = 46.5, zoom = 6) %>%
prependContent(tags$style(type = "text/css", css_fix))


htmlwidgets::saveWidget(swe_map, "/home/zhoylman/drought_indicators/snotel/widgets/swe_snotel.html", selfcontained = T)
save(swe_map, file = "/home/zhoylman/drought_indicators/snotel/widgets/swe_snotel.RData")
 
saveWidget(swe_map, "/home/zhoylman/drought_indicators/widgets/swe_snotel.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")


pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(min(daily_lookup$percent_precip, na.rm = T),max(daily_lookup$percent_precip, na.rm = T)), na.color = "grey")

precip_map = base_map() %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, 
                   popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/plots/precip_snotel_plot_",
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
pal_rev <- colorNumeric(rev(c("red", "yellow", "green", "blue", "purple")), domain = c(min(daily_lookup$percent_swe, na.rm = T),max(daily_lookup$percent_swe, na.rm = T)), na.color = "grey")


source("/home/zhoylman/drought_indicators/mapping_functions/base_map_mobile.R")


swe_map_mobile = base_map_mobile() %>%
  addCircleMarkers(snotel$lon, snotel$lat, snotel$simple_id, group = "SNOTEL (SWE)",
                   popup = paste0("<img src='https://shiny.cfc.umt.edu/drought_indicators/plots/snotel_plot_mobile_",
                                  snotel$simple_id,".png' height='200' width='270'/>"),
                   radius = 12, stroke = TRUE, fillOpacity = 0.9,
                   color = "black", fillColor = pal(daily_lookup$percent_swe)
  )%>%
  addRasterImage(current_1, colors = pal_r, opacity = 0.8, group = "24hr Change", project = TRUE)%>%
  addRasterImage(current_3, colors = pal_r, opacity = 0.8, group = "72hr Change", project = TRUE)%>%
  addRasterImage(current_7, colors = pal_r, opacity = 0.8, group = "7 Day Change", project = TRUE)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = c("24hr Change","72hr Change", "7 Day Change"),
                            overlayGroups = c("SNOTEL (SWE)", "States",  "Weather"),
                            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomleft", pal = pal_rev, values = daily_lookup$percent_swe,
            title = "% Average<br>SWE (Daily)",
            opacity = 1,
            group = "SNOTEL (SWE)",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  addLegend("bottomright", pal = pal_r_rev, values = 20:-20,
            title = paste0("SNODAS Snow <br>Depth Change (in)<br>",time_check),
            opacity = 1,
            group = "24hr Snow Change",
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
  )%>%
  setView(lng = -113.990211, lat = 46.864089, zoom = 7)%>%
  onRender("function(el, x) {
    this.removeControl(this.zoomControl);
  }") %>%
  prependContent(tags$style(type = "text/css", css_fix))


swe_map_mobile
saveWidget(swe_map_mobile, "/home/zhoylman/drought_indicators/widgets/swe_snotel_mobile.html", selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")
