library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))


shinyApp(
  ui <- fluidPage(
    fillPage(padding = 50,
      column(6,leafletOutput("map", height = 650),
      textOutput('site'),
      tags$head(tags$style("#time{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }"
      ))),
      column(6,htmlOutput("Plot"))
    )
  ),
  server <- function(input, output, session) {
    
    map = createLeafletMap(session, 'map')
    
    output$map = renderLeaflet({
      leaflet(snotel) %>%
        addTiles() %>%
        addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
        addMarkers(~lon, ~lat, ~site_num, popup = ~htmlEscape(site_name)#, clusterOptions = markerClusterOptions()
        )%>%
        setView(lng = -108, lat = 46.5, zoom = 6)
    })
    observe({
      click = input$map_marker_click
      id = click$id

      output$Plot <- renderText({
        first = "https://wcc.sc.egov.usda.gov/nwcc/plot?sitenum="
        site = as.character(id)
        mid = '&report=WYGRAPH&timeseries=Daily&interval=WATERYEAR='
        year = as.character(year(Sys.time()))
        end = '&temp_unit=8&format=plot&autoscale=false&legendpos=right'
        url = paste0(first,site,mid,year,end)

        c('<center><img src="',url,'"></center>')
      })
    })
  })
