#load all libraries for all apps
library(shiny)
library(leaflet)
library(leaflet.extras)
library(scales)
library(shinycssloaders)
library(sf)
library(raster)
library(htmltools)
library(rgdal)
library(dplyr)
library(zoo)
library(rowr)
library(precintcon)
library(gridExtra)
library(fitdistrplus)
library(tictoc)
library(ncdf4) 
library(lubridate)
library(plotly)
library(glogis)
library(PearsonDS)
library(gsl)
library(lmomco)
library(mapview)
library(parallel)
library(foreach)
library(doParallel)
library(htmlwidgets)


setwd('/home/zhoylman/drought_indicators/spi_app')

#load custom functions
source("../spi_app/R/gamma_fit.R")
source("../mapping_functions/base_map.R")
source("../spi_app/R/spi_calc_plot.R")

#SPI data
current_spi_30 = raster::raster("../spi_app/maps/current_spi/current_spi_30.tif")
current_spi_60 = raster::raster("../spi_app/maps/current_spi/current_spi_60.tif")
current_spi_90 = raster::raster("../spi_app/maps/current_spi/current_spi_90.tif")
current_spi_180 = raster::raster("../spi_app/maps/current_spi/current_spi_180.tif")
current_spi_365 = raster::raster("../spi_app/maps/current_spi/current_spi_365.tif")
current_spi_water_year = raster::raster("../spi_app/maps/current_spi/current_spi_water_year.tif")
current_spi_year_to_date = raster::raster("../spi_app/maps/current_spi/current_spi_year_to_date.tif")
watersheds_30 = st_read("../spi_app/shp/current_spi/current_spi_watershed_30.shp")
watersheds_60 = st_read("../spi_app/shp/current_spi/current_spi_watershed_60.shp")
watersheds_90 = st_read("../spi_app/shp/current_spi/current_spi_watershed_90.shp")
watersheds_180 = st_read("../spi_app/shp/current_spi/current_spi_watershed_180.shp")
watersheds_365 = st_read("../spi_app/shp/current_spi/current_spi_watershed_365.shp")
watersheds_water_year = st_read("../spi_app/shp/current_spi/current_spi_watershed_water_year.shp")
watersheds_year_to_date = st_read("../spi_app/shp/current_spi/current_spi_watershed_year_to_date.shp")
county_30 = st_read("../spi_app/shp/current_spi/current_spi_county_30.shp")
county_60 = st_read("../spi_app/shp/current_spi/current_spi_county_60.shp")
county_90 = st_read("../spi_app/shp/current_spi/current_spi_county_90.shp")
county_180 = st_read("../spi_app/shp/current_spi/current_spi_county_180.shp")
county_365 = st_read("../spi_app/shp/current_spi/current_spi_county_365.shp")
county_water_year = st_read("../spi_app/shp/current_spi/current_spi_county_water_year.shp")
county_year_to_date = st_read("../spi_app/shp/current_spi/current_spi_county_year_to_date.shp")

#define color pallets
pal_bins <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                          domain = -3.5:3.5, bins = seq(-3.5,3.5,0.5))


pal <- colorNumeric(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), -3.5:3.5, na.color = "transparent")

################################################################################################
############################################## SHINY APP #######################################
################################################################################################

shinyApp( 
  #set up the user interface
  ui <- fluidPage(
                  verticalLayout(),
                  br(),
                  wellPanel(align = "center",textOutput('time'), style = "padding: 5px;"),
                  tags$head(tags$style("#time{color: black; font-size: 16px; font-style: bold;}"
                  )),
                  inputPanel(align = "center",
                    actionButton("evRaster", "Gridded Data"),
                    actionButton("evHUC", "Watersheds"),
                    actionButton("evCounty", "Counties"),
                    style="color: #add8e6; background-color: #337ab7; border-color: #00000"),
                  leafletOutput("mymap", height = 650),
                  tags$head(tags$style(type="text/css", "
                                  #loadmessage {position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px; text-align: 
                                  center; font-weight: bold; font-size: 100%; color: #ffffff; background-color: #003366;z-index: 105;}")),
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$div("Calculating Climatology...",id="loadmessage")),
                  plotOutput("testPlot", width = "100%", height = "900px") 
  ),
  
  # set up the server side of the app
  server <- function(input, output) { 
    #add text to show when the most current data was available
    output$time = renderText({paste("The most recent data available is from ",as.character(watersheds_30$crrnt_t[1]))})
    
    output$mymap = renderLeaflet({
      m_raster
    })
    
    #lists of layers for loop leaflet map generation
    watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_365, watersheds_water_year, watersheds_year_to_date)
    county_list = list(county_30, county_60, county_90, county_180, county_365, county_water_year, county_year_to_date)
    raster_list = list(current_spi_30, current_spi_60,current_spi_90, current_spi_180, current_spi_365, current_spi_water_year, current_spi_year_to_date)
    
    watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year", "Year to Date")
    timescale_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")
    
    #labels for aggregated data
    labels = list()
    for(i in 1:length(watershed_list_names)){
      labels[[i]] <- sprintf(
        "<strong>%s</strong><br/>SPI = %g<sup></sup>",
        watershed_list[[i]]$NAME, watershed_list[[i]]$average
      ) %>% lapply(htmltools::HTML)
    }
    
    labels_county = list()
    for(i in 1:length(watershed_list_names)){
      labels_county[[i]] <- sprintf(
        "<strong>%s</strong><br/>SPI = %g<sup></sup>",
        county_list[[i]]$NAME, county_list[[i]]$average
      ) %>% lapply(htmltools::HTML)
    }
    

    ################################################################################
    ############################### BUILD RASTER MAP ###############################
    ################################################################################
    # Add multiple layers with a loop ----------------------------------------------
    m_raster = base_map()
    for(i in 1:length(watershed_list_names)){
      m_raster = m_raster %>%
        addRasterImage(raster_list[[i]], colors = pal, opacity = 0.8, group = timescale_names[i], project = FALSE)
    }

    # Add some layer controls
      m_raster = m_raster %>%
      addLayersControl(position = "topleft",
                       baseGroups = timescale_names,
                       overlayGroups = c("USDM", "States", "Weather"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = -3.5:3.5,
                title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                position = "bottomleft")



    # mapview::mapshot(m_raster,"/home/zhoylman/drought_indicators/spi_app/widgets/m_raster.html")
    # test = leaflet(includeHTML("/home/zhoylman/drought_indicators/spi_app/widgets/m_raster.html"))
    #widgetframe::saveWidgetframe(m_raster, "/home/zhoylman/drought_indicators/spi_app/widgets/m_raster.html", selfcontained = T)
      saveWidget(as_widget(m_raster), "/home/zhoylman/drought_indicators/spi_app/widgets/m_raster_test.html", selfcontained = T)

    ################################################################################
    ############################### BUILD HUC MAP ##################################
    ################################################################################
    
    m_huc = base_map()
    
    # Add multiple layers with a loop ----------------------------------------------
    for(i in 1:length(watershed_list_names)){
      m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = timescale_names[i], fillColor = ~pal_bins(average), weight = 2, opacity = 1, color = "black", 
                                    dashArray = "3", fillOpacity = 0.7, highlight = 
                                      highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
    }
    
    # Add Layer Controls  ----------------------------------------------    
    m_huc = m_huc %>%
      addLayersControl(position = "topleft",
                       baseGroups = timescale_names,
                       overlayGroups = c("USDM", "States", "Weather"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = -3.5:3.5,
                title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                position = "bottomleft")
    
    #widgetframe::saveWidgetframe(m_huc, "/home/zhoylman/drought_indicators/spi_app/widgets/m_huc.html", selfcontained = T)
    
    
    ################################################################################
    ############################### BUILD COUNTY MAP ###############################
    ################################################################################
      
    m_county = base_map()
    
    # Add multiple layers with a loop ----------------------------------------------
    for(i in 1:length(watershed_list_names)){
      m_county = m_county %>% addPolygons(data = county_list[[i]], group = timescale_names[i], fillColor = ~pal_bins(average), weight = 2, opacity = 1, color = "black", 
                                          dashArray = "3", fillOpacity = 0.7, highlight = 
                                            highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_county[[i]], 
                                          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
    }

    # Add Layer Controls  ----------------------------------------------    
    m_county = m_county %>%
      addLayersControl(position = "topleft",
                       baseGroups = timescale_names,
                       overlayGroups = c("USDM", "States", "Weather"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(pal = pal, values = -3.5:3.5,
                title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                position = "bottomleft")

    #create action button responses
    observeEvent(input$evHUC,{
      output$mymap <- renderLeaflet(m_huc)
    })
    
    observeEvent(input$evCounty,{
      output$mymap <- renderLeaflet(m_county)
    })
    
    observeEvent(input$evRaster,{
      output$mymap <- renderLeaflet(m_raster)
    })
    
    ################################################################################
    ############################### POINT SCALE ANALYSIS ###########################
    ################################################################################
    
    # see "../spi_app/R/spi_calc_plot.R"
    observeEvent(input$mymap_draw_new_feature,{
      feature <- input$mymap_draw_new_feature
      output$testPlot <- renderPlot({
        spi_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])
      })
    })
  }
)
