library(shiny)
library(png)
library(leaflet)
library(rgdal)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflet.esri)

#define input shp files
snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))
snotel$simple_id = c(1:length(snotel$site_num))

#load current conditions 
load("/home/zhoylman/drought_indicators/snotel/climatology/current_precent_SWE.RData")

daily_lookup = data.frame(daily_mean = unlist(daily_lookup[1]),
                  SWE = unlist(daily_lookup[2]),
                  percent = unlist(daily_lookup[3]))

daily_lookup$percent_crop = daily_lookup$percent
daily_lookup$percent_crop[daily_lookup$percent_crop >200] = 200
daily_lookup$percent_crop[daily_lookup$percent_crop == "NaN"] = NA

na.index = which(is.na(daily_lookup$daily_mean), arr.ind=TRUE)

snotel$lat[na.index] = NA
snotel$lon[na.index] = NA

daily_lookup$percent_crop[which(daily_lookup$current == 0)] = NA
daily_lookup$percent_crop[which(daily_lookup$percent_crop == 0)] = NA


#color pallet
pal <- colorNumeric(c("red", "yellow", "green", "blue", "purple"), domain = c(0,200), na.color = "black")

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
      tags$style(type='text/css', "#Plot {margin-top: 150px;}"),
      column(6,imageOutput("Plot"))
    )
  ),
  server <- function(input, output, session) {
    
    map = createLeafletMap(session, 'map')
    
    output$map = renderLeaflet({
      leaflet(snotel, options = leafletOptions(minZoom = 5, maxZoom = 10)) %>%
        setMaxBounds( lng1 = -118.239466
                      , lat1 = 49.177568
                      , lng2 = -95.818551
                      , lat2 = 42.270448) %>%
        addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
        leaflet::addProviderTiles("Stamen.TonerLines") %>%
        leaflet::addProviderTiles("Stamen.TonerLabels") %>%
        addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
        addCircleMarkers(~lon, ~lat, ~simple_id, popup = ~htmlEscape(site_name), radius = 10, stroke = TRUE, fillOpacity = 0.9,
                         color = "black", fillColor = ~pal(daily_lookup$percent_crop)
        )%>%
        addLegend("bottomleft", pal = pal, values = ~daily_lookup$percent_crop,
                  title = "% Average<br>SWE (Daily)",
                  opacity = 1,
                  na.label = "No Snow or NA"
        )%>%
        setView(lng = -108, lat = 46.5, zoom = 6)
    })
    observe({
      click = input$map_marker_click
      id = click$id
      first = "/home/zhoylman/drought_indicators/snotel/plots/snotel_plot_"
      mid = as.character(id)
      end = ".png"
      filename = paste0(first,mid,end)

      width  <- session$clientData$output_Plot_width
      height <- session$clientData$output_Plot_height
      
      output$Plot <- renderImage({
        list(src =filename,
             width = width,
             height = "auto")
      }, deleteFile = FALSE)
    })
  })




###using NRCS plots

# })
# observe({
#   click = input$map_marker_click
#   id = click$id
#   
#   output$Plot <- renderText({
#     first = "https://wcc.sc.egov.usda.gov/nwcc/plot?sitenum="
#     site = as.character(id)
#     mid = '&report=WYGRAPH&timeseries=Daily&interval=WATERYEAR='
#     year = as.character(year(Sys.time()))
#     end = '&temp_unit=8&format=plot&autoscale=false&legendpos=right'
#     url = paste0(first,site,mid,year,end)
#     
#     c('<center><img src="',url,'"></center>')
#   })
# })
# })

