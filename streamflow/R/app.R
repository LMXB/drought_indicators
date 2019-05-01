library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)

gages_raw = st_read("/home/zhoylman/drought_indicators/streamflow/shp/usgs_gages_wgs84.shp")
gages = gages_raw %>% dplyr::filter(STATE == c("MT", "ID", "ND", "SD", "WY"))

states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")

shinyApp(
         ui <- fluidPage(
           leafletOutput("map", height = 650),
           mainPanel(
             htmlOutput("Plot")
           )
         ),
         server <- function(input, output, session) {
           
           map = createLeafletMap(session, 'map')
           
           output$map = renderLeaflet({
             leaflet(gages) %>%
               addTiles() %>%
               addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
               addMarkers(~LNG_GAGE, ~LAT_GAGE, ~STAID,
                 clusterOptions = markerClusterOptions()
               )%>%
               setView(lng = -108, lat = 46.5, zoom = 6)
           })
           observe({
             click = input$map_marker_click
             id = click$id

             output$Plot <- renderText({
               first = "https://waterwatch.usgs.gov/wwapps/wwdur4.php?sno="
               site = as.character(id)
               end = '&amp;yr=2019&amp;ytp=wy&amp;dt=dv01d&amp;nyr=1&amp;xps=line&amp;nyor=10&amp;legend=0&amp;otp=plot'
               
               url = paste0(first,site,end)
               
               c('<center><img src="',url,'"></center>')
             })
           })
         })
