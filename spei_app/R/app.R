
#SPEI Data
current_spei_30 = raster::raster("../spei_app/maps/current_spei/current_spei_30.tif")
current_spei_60 = raster::raster("../spei_app/maps/current_spei/current_spei_60.tif")
current_spei_90 = raster::raster("../spei_app/maps/current_spei/current_spei_90.tif")
current_spei_180 = raster::raster("../spei_app/maps/current_spei/current_spei_180.tif")
current_spei_300 = raster::raster("../spei_app/maps/current_spei/current_spei_300.tif")

watersheds_30 = st_read("../spei_app/shp/current_spei/current_spei_watershed_30.shp")
watersheds_60 = st_read("../spei_app/shp/current_spei/current_spei_watershed_60.shp")
watersheds_90 = st_read("../spei_app/shp/current_spei/current_spei_watershed_90.shp")
watersheds_180 = st_read("../spei_app/shp/current_spei/current_spei_watershed_180.shp")
watersheds_300 = st_read("../spei_app/shp/current_spei/current_spei_watershed_300.shp")

county_30 = st_read("../spei_app/shp/current_spei/current_spei_county_30.shp")
county_60 = st_read("../spei_app/shp/current_spei/current_spei_county_60.shp")
county_90 = st_read("../spei_app/shp/current_spei/current_spei_county_90.shp")
county_180 = st_read("../spei_app/shp/current_spei/current_spei_county_180.shp")
county_300 = st_read("../spei_app/shp/current_spei/current_spei_county_300.shp")

#actual app
shinyApp(
  ui <- fluidPage(class = "text-center",
                  verticalLayout(),
                  br(),
                  inputPanel(
                    actionButton("evRaster", "Raw Map"),
                    actionButton("evHUC", "Watersheds"),
                    actionButton("evCounty", "County"),
                    style="color: #add8e6; background-color: #337ab7; border-color: #00000"),
           #mainPanel(width = "100%",
             leafletOutput("mymap", height = 600),
             
             
             tags$head(tags$style(type="text/css", "
                                  #loadmessage {
                                  position: fixed;
                                  top: 0px;
                                  left: 0px;
                                  width: 100%;
                                  padding: 5px 0px 5px 0px;
                                  text-align: center;
                                  font-weight: bold;
                                  font-size: 100%;
                                  color: #ffffff;
                                  background-color: #003366;
                                  z-index: 105;
                                  }
                                  ")),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tags$div("Calculating Climatology...",id="loadmessage")),
           plotOutput("testPlot", width = "100%", height = "900px")# %>% withSpinner(color="#0dc5c1", type = 8, proxy.height = "200px") 
  ),
           #),
         server <- function(input, output) {
           
           output$mymap = renderLeaflet({
             leaflet() %>%
               addTiles() %>%
               setView(lng = -108, lat = 46.5, zoom = 6)
           })
           
           
           watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_300)
           county_list = list(county_30, county_60, county_90, county_180, county_300)
           
           watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "300 Day HUC8")
           watershed_raster_names = c("30 Day", "60 Day", "90 Day", "180 Day", "300 Day")
           
           #labels for watershed highligh
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
           
           
           
           #color pallets
           pal_watershed <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#003366"), interpolate = "spline"), 
                                     domain = -3.5:3.5, bins = seq(-3.5,3.5,0.5))
           
           pal <- colorNumeric(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#003366"), -3.5:3.5, na.color = "transparent")
           
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           
           
           
           # Create leaflet widget --------------------------------------------------------
           m_raster = leaflet(watersheds_30) %>%
             addTiles() 
           
           # Add multiple layers with a loop ----------------------------------------------
           m_raster = m_raster %>% 
             addRasterImage(current_spei_30, colors = pal, opacity = 0.8, group = "30 Day") %>%
             addRasterImage(current_spei_60, colors = pal, opacity = 0.8, group = "60 Day") %>%
             addRasterImage(current_spei_90, colors = pal, opacity = 0.8, group = "90 Day") %>%
             addRasterImage(current_spei_180, colors = pal, opacity = 0.8, group = "180 Day") %>%
             addRasterImage(current_spei_300, colors = pal, opacity = 0.8, group = "300 Day")
           
           # Add Layer Controls  ----------------------------------------------    
           m_raster = m_raster %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             addDrawToolbar(markerOptions = drawMarkerOptions(),
                            polylineOptions = FALSE,
                            polygonOptions = FALSE,
                            circleOptions = FALSE,
                            rectangleOptions = FALSE,
                            circleMarkerOptions = FALSE,
                            editOptions = FALSE,
                            singleFeature = TRUE,
                            targetGroup='draw')
           

           m_huc = leaflet(watersheds_30) %>%
             addTiles() 
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "white", 
                                           dashArray = "3", fillOpacity = 0.7, highlight = 
                                             highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                           labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
           }
           
           
           # Add Layer Controls  ----------------------------------------------    
           m_huc = m_huc %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             addDrawToolbar(markerOptions = drawMarkerOptions(),
                            polylineOptions = FALSE,
                            polygonOptions = FALSE,
                            circleOptions = FALSE,
                            rectangleOptions = FALSE,
                            circleMarkerOptions = FALSE,
                            editOptions = FALSE,
                            singleFeature = TRUE,
                            targetGroup='draw')
           
           
           
           m_county = leaflet(watersheds_30) %>%
             addTiles() 
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_county = m_county %>% addPolygons(data = county_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "white", 
                                                 dashArray = "3", fillOpacity = 0.7, highlight = 
                                                   highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_county[[i]], 
                                                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
           }
           
           
           # Add Layer Controls  ----------------------------------------------    
           m_county = m_county %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPEI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             addDrawToolbar(markerOptions = drawMarkerOptions(),
                            polylineOptions = FALSE,
                            polygonOptions = FALSE,
                            circleOptions = FALSE,
                            rectangleOptions = FALSE,
                            circleMarkerOptions = FALSE,
                            editOptions = FALSE,
                            singleFeature = TRUE,
                            targetGroup='draw')
           
           
           
           
           observeEvent(input$evHUC,{
             output$mymap <- renderLeaflet(m_huc)
           })
           
           observeEvent(input$evCounty,{
             output$mymap <- renderLeaflet(m_county)
           })
           
           observeEvent(input$evRaster,{
             output$mymap <- renderLeaflet(m_raster)
           })
           
           #output$mymap <- renderLeaflet(m %>% hideGroup(watershed_list_names))
           
         }
  )