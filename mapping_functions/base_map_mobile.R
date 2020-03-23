#define base map information as a function used for all leaflet maps
#load base map dependent data
setwd("/home/zhoylman/drought_indicators/mapping_functions/")
states = st_read("../shp_kml/states.shp")

base_map_mobile = function(x){
  #prefer canvas works best for lots of points on mobile
  leaflet::leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    leaflet::addTiles("https://api.maptiler.com/tiles/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    leaflet::setView(lng = -108, lat = 45.5, zoom = 6) %>%
    leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 5, color = "black", opacity = 1)%>%
    leaflet::addWMSTiles(
      "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", group = "Weather",
      layers = "nexrad-n0r-900913",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE))%>%
    leaflet::addLayersControl(position = "topleft",
                     overlayGroups = c("States", "Weather"),
                     options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup("Weather") %>%
    prependContent(tags$head(tags$meta(HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'))))
}

