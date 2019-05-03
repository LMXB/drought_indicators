library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

#load current conditions 
load("/home/zhoylman/drought_indicators/snotel/climatology/current_precent_SWE.RData")

daily_lookup$percent_crop = daily_lookup$percent
daily_lookup$percent_crop[daily_lookup$percent_crop >200] = 200

#color pallet
pal <- colorNumeric(c("red", "white", "blue"), domain = c(0,200))

#custom legend fix
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"

shinyApp(
  ui <- fluidPage(
    fillPage(padding = 50,
      column(6,leafletOutput("map", height = 650)%>%
               prependContent(tags$style(type = "text/css", css_fix)),
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
        addCircleMarkers(~lon, ~lat, ~site_num, popup = ~htmlEscape(site_name), radius = 10,
                         color = ~pal(daily_lookup$percent_crop)#, clusterOptions = markerClusterOptions()
        )%>%
        addLegend("bottomleft", pal = pal, values = ~daily_lookup$percent_crop,
                  title = "% Average <br>Daily SWE",
                  opacity = 1,
                  na.label = "NA"
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
