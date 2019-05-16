library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflet.esri)
library(xts)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))
snotel$simple_id = c(1:length(snotel$site_num))

#load current conditions 
correlation_times = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/correlation_times.csv")

na.index = which(is.na(correlation_times$X2in), arr.ind=TRUE)

snotel$lat[na.index] = NA
snotel$lon[na.index] = NA

#color pallet
pal <- colorNumeric(c("red", "yellow", "blue"), domain = c(0,365), na.color = "grey")

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

base_map = function(){
  leaflet(snotel, options = leafletOptions(minZoom = 5, maxZoom = 10)) %>%
    addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
}

shinyApp(
  ui <- fluidPage(
    fillPage(padding = 50,
             column(4,leafletOutput("map", height = 650)%>%
                      prependContent(tags$style(type = "text/css", css_fix)),
                    textOutput('site'),
                    tags$head(tags$style("#time{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }"
                    ))),
             tags$style(type='text/css', "#Plot {margin-top: 0px;}"),
             tags$style(type='text/css', "#Plot2 {margin-top: 200px;}"),
             column(8,imageOutput("Plot")),
             column(8,imageOutput("Plot2"))
    )
  ),
  server <- function(input, output, session) {
    
    map = createLeafletMap(session, 'map')
    
    output$map = renderLeaflet({
      base_map() %>%
        addCircleMarkers(~lon, ~lat, ~simple_id, popup = ~htmlEscape(site_name), radius = 10, stroke = TRUE, fillOpacity = 0.9,
                         color = "black", fillColor = ~pal(correlation_times$X2in), group = "2 in"
        )%>%
        addCircleMarkers(~lon, ~lat, ~simple_id, popup = ~htmlEscape(site_name), radius = 10, stroke = TRUE, fillOpacity = 0.9,
                         color = "black", fillColor = ~pal(correlation_times$X8in), group = "8 in"
        )%>%
        addCircleMarkers(~lon, ~lat, ~simple_id, popup = ~htmlEscape(site_name), radius = 10, stroke = TRUE, fillOpacity = 0.9,
                         color = "black", fillColor = ~pal(correlation_times$X20in), group = "20 in"
        )%>%
        leaflet::addLegend("bottomleft", pal = pal, values = ~correlation_times$X2in,
                  title = "Timescale",
                  opacity = 1,
                  na.label = "NA"
        )%>%
        addLayersControl(position = "topleft",
                         baseGroups = c("2 in", "8 in", "20 in"),
                         options = layersControlOptions(collapsed = FALSE))%>%
        setView(lng = -108, lat = 46.5, zoom = 6)
    })
    observe({
      click = input$map_marker_click
      id = click$id
      if(length(id)==0){
        id = 1
      }
      first = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/correlation/Soil_Moisture_SPEI_Correlation_"
      mid = as.character(id)
      end = ".png"
      filename = paste0(first,mid,end)
      
      print(filename)
      
      width  <- session$clientData$output_Plot_width
      height <- session$clientData$output_Plot_height
      
      output$Plot <- renderImage({
        list(src =filename,
             width = (width*0.6),
             height = "auto")
      }, deleteFile = FALSE)
      
      first = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/time_series/Soil_Moisture_SPEI_timeseries_"
      mid = as.character(id)
      end = ".png"
      filename2 = paste0(first,mid,end)
      
      width  <- session$clientData$output_Plot_width
      height <- session$clientData$output_Plot_height
      
      output$Plot2 <- renderImage({
        list(src =filename2,
             width = width,
             height = "auto")
      }, deleteFile = FALSE)
    })
  })

