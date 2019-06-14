#define base map information as a function used for all leaflet maps
#load base map dependent data
states = st_read("../shp_kml/states.shp")
current_usdm = st_read("../USDM_current/current_usdm.shp")
current_usdm_date = read.csv("../USDM_current/usdm_time.csv")
current_usdm_date = as.Date(as.character(current_usdm_date$x), format = "%Y%m%d")
usdm_description = c("(Abnormally Dry)", "(Moderate Drought)",
                     "(Severe Drought)", "(Extreme Drought)",
                     "(Exceptional Drought)")

for(i in 1:length(current_usdm$DM)){
  current_usdm$DM1[i] = paste(current_usdm$DM[i], 
                              usdm_description[i], sep = " ")}

labels_usdm = list()
labels_usdm[[1]] <- sprintf(
  "<strong>%s</strong><br/>USDM = D%g<sup></sup>",
  current_usdm_date, current_usdm$DM
) %>% lapply(htmltools::HTML)

pal_usdm <- colorBin(colorRamp(c("#ffff00", "#918151", "#ffa500", "#ff0000", "#811616"), interpolate = "spline"), 
                     domain = 0:4, bins = seq(0,4,1))

pal_usdm_legend <- colorFactor(c("#ffff00", "#918151", "#ffa500", "#ff0000", "#811616"), domain = c("D0 (Abnormally Dry)", "D1 (Moderate Drought)",
                                                                                                    "D2 (Severe Drought)", "D3 (Extreme Drought)",
                                                                                                    "D4 (Exceptional Drought)"))
#define basemap function
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
