setwd('/home/zhoylman/drought_indicators/spei_app')

#load custom functions
source("../mapping_functions/base_map.R")
source("../spei_app/R/spei_calc_plot.R")

#spei data
current_spei_30 = raster::raster("../spei_app/maps/current_spei/current_spei_30.tif")
current_spei_60 = raster::raster("../spei_app/maps/current_spei/current_spei_60.tif")
current_spei_90 = raster::raster("../spei_app/maps/current_spei/current_spei_90.tif")
current_spei_180 = raster::raster("../spei_app/maps/current_spei/current_spei_180.tif")
current_spei_365 = raster::raster("../spei_app/maps/current_spei/current_spei_365.tif")
current_spei_water_year = raster::raster("../spei_app/maps/current_spei/current_spei_water_year.tif")
current_spei_year_to_date = raster::raster("../spei_app/maps/current_spei/current_spei_year_to_date.tif")
watersheds_30 = st_read("../spei_app/shp/current_spei/current_spei_watershed_30.shp")
watersheds_60 = st_read("../spei_app/shp/current_spei/current_spei_watershed_60.shp")
watersheds_90 = st_read("../spei_app/shp/current_spei/current_spei_watershed_90.shp")
watersheds_180 = st_read("../spei_app/shp/current_spei/current_spei_watershed_180.shp")
watersheds_365 = st_read("../spei_app/shp/current_spei/current_spei_watershed_365.shp")
watersheds_water_year = st_read("../spei_app/shp/current_spei/current_spei_watershed_water_year.shp")
watersheds_year_to_date = st_read("../spei_app/shp/current_spei/current_spei_watershed_year_to_date.shp")
county_30 = st_read("../spei_app/shp/current_spei/current_spei_county_30.shp")
county_60 = st_read("../spei_app/shp/current_spei/current_spei_county_60.shp")
county_90 = st_read("../spei_app/shp/current_spei/current_spei_county_90.shp")
county_180 = st_read("../spei_app/shp/current_spei/current_spei_county_180.shp")
county_365 = st_read("../spei_app/shp/current_spei/current_spei_county_365.shp")
county_water_year = st_read("../spei_app/shp/current_spei/current_spei_county_water_year.shp")
county_year_to_date = st_read("../spei_app/shp/current_spei/current_spei_county_year_to_date.shp")

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
    raster_list = list(current_spei_30, current_spei_60,current_spei_90, current_spei_180, current_spei_365, current_spei_water_year, current_spei_year_to_date)
    
    watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year", "Year to Date")
    timescale_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")
    
    #labels for aggregated data
    labels = list()
    for(i in 1:length(watershed_list_names)){
      labels[[i]] <- sprintf(
        "<strong>%s</strong><br/>SPEI = %g<sup></sup>",
        watershed_list[[i]]$NAME, watershed_list[[i]]$average
      ) %>% lapply(htmltools::HTML)
    }
    
    labels_county = list()
    for(i in 1:length(watershed_list_names)){
      labels_county[[i]] <- sprintf(
        "<strong>%s</strong><br/>SPEI = %g<sup></sup>",
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
                title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
                position = "bottomleft")
    
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
                title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
                position = "bottomleft")
    
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
                title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
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
    
    # see "../spei_app/R/spei_calc_plot.R"
    observeEvent(input$mymap_draw_new_feature,{
      feature <- input$mymap_draw_new_feature
      output$testPlot <- renderPlot({
        spei_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])
      })
    })
  }
)
