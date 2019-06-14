#define base map information as a function used for all leaflet maps
base_map = function(x){
  leaflet(options = tileOptions(minZoom = 4, maxZoom = 10)) %>%
    addMapPane("USDM", zIndex = 410) %>%
    addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    setMaxBounds( lng1 = -122
                  , lat1 = 53
                  , lng2 = -91
                  , lat2 = 39)%>%
    setView(lng = -108, lat = 46.5, zoom = 5) %>%
    addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
    addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black",
                fillOpacity = 0.5, options = pathOptions(pane = "USDM"), highlight = 
                  highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))%>%
    addWMSTiles(
      "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", group = "Weather",
      layers = "nexrad-n0r-900913",
      options = WMSTileOptions(format = "image/png", transparent = TRUE))%>%
    addLayersControl(position = "topleft",
                     overlayGroups = c("USDM", "States", "Weather"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addDrawToolbar(markerOptions = drawMarkerOptions(),
                   polylineOptions = FALSE,
                   polygonOptions = FALSE,
                   circleOptions = FALSE,
                   rectangleOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   editOptions = FALSE,
                   singleFeature = TRUE,
                   targetGroup='draw')%>%
    addLegend("bottomright", pal = pal_usdm_legend, values = c("D0 (Abnormally Dry)", "D1 (Moderate Drought)",
                                                               "D2 (Severe Drought)", "D3 (Extreme Drought)",
                                                               "D4 (Exceptional Drought)"),title = "USDM")
}