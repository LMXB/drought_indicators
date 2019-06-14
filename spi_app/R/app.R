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
#library(shinydashboard)

setwd('/home/zhoylman/drought_indicators/spi_app')

#load custom functions
source("../spi_app/R/gamma_fit.R")

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

current_usdm = st_read("../USDM_current/current_usdm.shp")

current_usdm_date = read.csv("../USDM_current/usdm_time.csv")
current_usdm_date = as.Date(as.character(current_usdm_date$x), format = "%Y%m%d")

states = st_read("../shp_kml/states.shp")

shinyApp( 
         ui <- fluidPage(class = "text-center",
                         verticalLayout(),
                         br(),
                         inputPanel(
                           actionButton("evRaster", "Raw Map"),
                           actionButton("evHUC", "Watersheds"),
                           actionButton("evCounty", "County"),
                           style="color: #add8e6; background-color: #337ab7; border-color: #00000"),
                         textOutput('time'),
                         tags$head(tags$style("#time{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }"
                         )
                         ),
                         # downloadButton(outputId = "dl"),
           #mainPanel(
             leafletOutput("mymap", height = 650),
             

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
           
           output$time = renderText({paste("The most recent data available is from ",as.character(watersheds_30$crrnt_t[1]))})
           
           output$mymap = renderLeaflet({#m_raster
             leaflet(current_spi_30, options = tileOptions(minZoom = 5, maxZoom = 10)) %>%
               addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
               leaflet::addProviderTiles("Stamen.TonerLines") %>%
               leaflet::addProviderTiles("Stamen.TonerLabels") %>%
               setMaxBounds( lng1 = -118.239466
                             , lat1 = 49.177568
                             , lng2 = -95.818551
                             , lat2 = 42.270448 )%>%
               setView(lng = -108, lat = 46.5, zoom = 6)
             
           })
           
          #spatial datasets and libraries are in global.R
           
           watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_365, watersheds_water_year, watersheds_year_to_date)
           county_list = list(county_30, county_60, county_90, county_180, county_365, county_water_year, county_year_to_date)
           
           watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year", "Year to Date")
           watershed_raster_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")
           
           #labels for watershed highligh
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
           
           usdm_description = c("(Abnormally Dry)", "(Moderate Drought)",
                                "(Severe Drought)", "(Extreme Drought)",
                                "(Exceptional Drought)")
           
           #for some reason this kills the app??????
           for(i in 1:length(current_usdm$DM)){
             current_usdm$DM1[i] = paste(current_usdm$DM[i], 
                                              usdm_description[i], sep = " ")}
           ##########################################
           
           labels_usdm = list()
           labels_usdm[[1]] <- sprintf(
               "<strong>%s</strong><br/>USDM = D%g<sup></sup>",
               current_usdm_date, current_usdm$DM
             ) %>% lapply(htmltools::HTML)
           
           
           
           #color pallets
           pal_watershed <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                                     domain = -3.5:3.5, bins = seq(-3.5,3.5,0.5))
           
           pal_usdm <- colorBin(colorRamp(c("#ffff00", "#918151", "#ffa500", "#ff0000", "#811616"), interpolate = "spline"), 
                                     domain = 0:4, bins = seq(0,4,1))
           
           
           pal <- colorNumeric(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), -3.5:3.5, na.color = "transparent")
           # pal <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
           #                 domain = -3.5:3.5, bins = c(-Inf,-3,-2.5,-2,-1.2,-0.7,-0.2,0.2,0.7,1.2,2,2.5,3,Inf), na.color = "transparent")
           
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           #-----------------------------------------------------------------------------------#
           
           
           
           # Create default --------------------------------------------------------
           leaflet(current_spi_30) %>%
             addTiles() %>%
             addRasterImage(current_spi_30, colors = pal, opacity = 0.8, group = "30 Day")
           
           # Create leaflet widget --------------------------------------------------------
           m_raster = leaflet(current_spi_30, options = tileOptions(minZoom = 5, maxZoom = 10)) %>%
             addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
             leaflet::addProviderTiles("Stamen.TonerLines") %>%
             leaflet::addProviderTiles("Stamen.TonerLabels") 
           
           # Add multiple layers with a loop ----------------------------------------------
           m_raster = m_raster %>% 
             addRasterImage(current_spi_30, colors = pal, opacity = 0.8, group = "30 Day", project = FALSE) %>%
             addRasterImage(current_spi_60, colors = pal, opacity = 0.8, group = "60 Day", project = FALSE) %>%
             addRasterImage(current_spi_90, colors = pal, opacity = 0.8, group = "90 Day", project = FALSE) %>%
             addRasterImage(current_spi_180, colors = pal, opacity = 0.8, group = "180 Day", project = FALSE) %>%
             addRasterImage(current_spi_365, colors = pal, opacity = 0.8, group = "365 Day", project = FALSE) %>%
             addRasterImage(current_spi_water_year, colors = pal, opacity = 0.8, group = "Water Year", project = FALSE) %>%
             addRasterImage(current_spi_year_to_date, colors = pal, opacity = 0.8, group = "Year to Date", project = FALSE) %>%
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
             addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black", 
                         fillOpacity = 0.5, highlight = 
                           highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                           labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
                         
           
                      # Add Layer Controls  ----------------------------------------------    
           m_raster = m_raster %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               overlayGroups = c("USDM", "States"),
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             setMaxBounds( lng1 = -118.239466
                           , lat1 = 49.177568
                           , lng2 = -95.818551
                           , lat2 = 42.270448 )%>%
             addDrawToolbar(markerOptions = drawMarkerOptions(),
                            polylineOptions = FALSE,
                            polygonOptions = FALSE,
                            circleOptions = FALSE,
                            rectangleOptions = FALSE,
                            circleMarkerOptions = FALSE,
                            editOptions = FALSE,
                            singleFeature = TRUE,
                            targetGroup='draw')
           

           m_huc = leaflet(watersheds_30, options = tileOptions(minZoom = 5, maxZoom = 10)) %>%
             addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
             leaflet::addProviderTiles("Stamen.TonerLines") %>%
             leaflet::addProviderTiles("Stamen.TonerLabels") %>%
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "black", 
                                           dashArray = "3", fillOpacity = 0.7, highlight = 
                                             highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                           labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
             if(i == length(watershed_list_names)){
               m_huc = m_huc %>% addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black", 
                                             fillOpacity = 0.5, highlight = 
                                               highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                                             labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
             }
           }
           
           
           # Add Layer Controls  ----------------------------------------------    
           m_huc = m_huc %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               overlayGroups = c("USDM", "States"),
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             setMaxBounds( lng1 = -118.239466
                           , lat1 = 49.177568
                           , lng2 = -95.818551
                           , lat2 = 42.270448 )%>%
             addDrawToolbar(markerOptions = drawMarkerOptions(),
                            polylineOptions = FALSE,
                            polygonOptions = FALSE,
                            circleOptions = FALSE,
                            rectangleOptions = FALSE,
                            circleMarkerOptions = FALSE,
                            editOptions = FALSE,
                            singleFeature = TRUE,
                            targetGroup='draw')
           
           
           
           m_county = leaflet(watersheds_30, options = tileOptions(minZoom = 5, maxZoom = 10)) %>%
             addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
             leaflet::addProviderTiles("Stamen.TonerLines") %>%
             leaflet::addProviderTiles("Stamen.TonerLabels") %>%
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_county = m_county %>% addPolygons(data = county_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "black", 
                                                 dashArray = "3", fillOpacity = 0.7, highlight = 
                                                   highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_county[[i]], 
                                                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
             if(i == length(watershed_list_names)){
               m_county = m_county %>% addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black", 
                                                   fillOpacity = 0.5, highlight = 
                                                     highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                                                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
             }
           }
           
           
           # Add Layer Controls  ----------------------------------------------    
           m_county = m_county %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               overlayGroups = c("USDM", "States"),
               options = layersControlOptions(collapsed = FALSE)) %>%
             addLegend(pal = pal, values = -3.5:3.5,
                       title = paste0("Current SPI<br>", as.character(watersheds_30$crrnt_t[1])),
                       position = "bottomleft")%>%
             
             #     #add
             #     # addWMSTiles(
             #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
             #     #   layers = "nexrad-n0r-900913",
             #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
             
             setView(lng = -108, lat = 46.5, zoom = 6) %>%
             setMaxBounds( lng1 = -118.239466
                           , lat1 = 49.177568
                           , lng2 = -95.818551
                           , lat2 = 42.270448 )%>%
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
           #   output$dl <- downloadHandler(
           #     filename = paste0( Sys.Date()
           #                        , "_custom_drought_map"
           #                        , ".png"
           #     )
           #     
           #     , content = function(file) {
           #       mapshot( x = m_huc
           #                , file = file
           #                , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
           #                , selfcontained = FALSE
           #                , remove_controls = c("zoomControl")# when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
           #       )
           #     } # end of content() function
           #   ) # end of downloadHandler() function
           })
           
           observeEvent(input$evCounty,{
             output$mymap <- renderLeaflet(m_county)
           })
           
           observeEvent(input$evRaster,{
             output$mymap <- renderLeaflet(m_raster)
           })
           
           #output$mymap <- renderLeaflet(m %>% hideGroup(watershed_list_names))
           
           
           spi_calc_plot = function(lat_in, lon_in){
             lat_of_interest = lat_in
             lon_of_interest = lon_in

             ### DEFINE THE URL to net cdf
             urltotal<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
             
             ## OPEN THE FILE
             nc <- nc_open(urltotal)
             
             ## NOTE: FILE DIMENSIONS ARE lon,lat,time
             v3 <- nc$var[[1]]
             lonsize <- v3$varsize[1]
             latsize <- v3$varsize[2] 
             endcount <- v3$varsize[3] 
             
             ### DEFINE OUR POINT OF INTEREST 
             lon_matrix = nc$var[[1]]$dim[[1]]$vals
             lat_matrix = nc$var[[1]]$dim[[2]]$vals
             
             #find lat long that corispond
             lon=which(abs(lon_matrix-lon_of_interest)==min(abs(lon_matrix-lon_of_interest)))  
             lat=which(abs(lat_matrix-lat_of_interest)==min(abs(lat_matrix-lat_of_interest))) 
             
             ## DEFINE OUR VARIABLE NAME 
             var="precipitation_amount"
             
             ## READ THE DATA VARIABLE 
             data <- ncvar_get(nc, var, start=c(lon,lat,1),count=c(1,1,endcount))
             ## READ THE TIME VARIABLE
             time <- ncvar_get(nc, "day", start=c(1),count=c(endcount))
             ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
             time=as.Date(time, origin="1900-01-01") 
             # PUT EVERYTHING INTO A DATA FRAME
             c <- data.frame(time,data)
             
             ## CLOSE THE FILE
             nc_close(nc)
             
             #define some date based variables
             c$day = yday(c$time)
             c$year = year(c$time)
             c$month = month(c$time)
             
             spi_calc = function(data, time_scale){
               #Start SPI calculation
               for(i in rev((length(data$time)-365):length(data$time))){
                 #calcualte index vectors of interest based on time
                 first_date_breaks = which(data$day == data$day[i])
                 second_date_breaks = first_date_breaks-(time_scale-1)
                 
                 #if there are negative indexes remove last year (incomplete data range)
                 #change this to remove all indexes from both vectors that are negative
                 if(!all(second_date_breaks < 0)){
                   pos_index = which(second_date_breaks > 0)
                   first_date_breaks = first_date_breaks[c(pos_index)]
                   second_date_breaks = second_date_breaks[c(pos_index)]
                 }
                 
                 #create slice vectors and group by vectors
                 for(j in 1:length(first_date_breaks)){
                   if(j == 1){
                     slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
                     group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
                   }
                   else{
                     slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
                     group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
                   }
                 }
                 
                 #slice data for appropriate periods
                 data_time_filter = data %>%
                   slice(slice_vec) %>%
                   tibble::add_column(group_by_vec = group_by_vec)%>%
                   group_by(group_by_vec)%>%
                   dplyr::summarise(sum = sum(data))
                 
                 #remove zeros because they cause the gamma dist to blow up to Inf
                 data_time_filter$sum[data_time_filter$sum == 0] = 0.01
                 
                 #compute date time for day/year of interest
                 date_time = as.POSIXct(paste(data$day[first_date_breaks], data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
                 
                 #fit gamma distrobution to data
                 fit.gamma = gamma_fit(data_time_filter$sum)
                 
                 #calcualte CDF values for the theoretical distrobution
                 fit.cdf = pgamma(data_time_filter$sum, shape = fit.gamma$shape, rate = fit.gamma$rate)
                 
                 #equaprobaility transformation for cdf quantiles
                 if(i == length(data$time)){
                   output.df = data.frame(spi = qnorm(fit.cdf, mean = 0, sd = 1),
                                          time = date_time)
                 }
                 
                 else{
                   output.df = rbind(output.df, data.frame(spi = qnorm(fit.cdf, mean = 0, sd = 1),
                                                           time = date_time))
                 }
                 
               }
               output.df = output.df[order(output.df$time),]
               output.df$col = output.df$spi
               output.df$col[output.df$col < 0] = "red"
               output.df$col[as.numeric(output.df$col) > 0] = "blue"
               
               return(output.df)
             }
             
             spi_30 = spi_calc(c,30)
             spi_365 = spi_calc(c,365)
             
             #calculate monthly precip totals
             monthly_precip1 = c %>%
               dplyr:::group_by(as.yearmon(time)) %>%
               dplyr:::summarise(monthly_sum = sum(data, na.rm = T))
             
             monthly_precip1$time = as.POSIXct(strptime(as.Date(monthly_precip1$`as.yearmon(time)`), format = "%Y-%m-%d", tz = "UTC"))
             
             #calcualte mean monthly precip
             mean_monthly = monthly_precip1 %>%
               dplyr::group_by(month(time)) %>%
               dplyr::summarise(monthly_mean = mean(monthly_sum, na.rm = T))
             
             monthly_precip1$percent_average = round(100*(monthly_precip1$monthly_sum / rep(mean_monthly$monthly_mean, length.out = length(monthly_precip1$time))),0)
             
             color_ramp = colorRampPalette(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"))
             
             spi_plot = function(data,title_str){
               plot1 = ggplot(data = data, aes(x = time, y = spi))+
                 geom_bar(stat = "identity", aes(colour=spi), size = 0.5)+
                 scale_color_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
                 theme_bw(base_size = base_font_size)+
                 xlab("Time")+
                 ylab("SPI")+
                 theme(legend.position="none")+
                 ylim(c(-3.5,3.5))+
                 ggtitle(title_str)+
                 scale_x_datetime(limits = as.POSIXct(c(data$time[length(data$time)-365*4],data$time[length(data$time)]), format = "%Y-%m-%d"))
               return(plot1)
             }
             
             #interactive plot
             spi_plot_interactive = function(data, title_str){
               p <- plot_ly() %>%
                 add_bars(
                   x = data$time,
                   y = data$spi,
                   color = data$col,
                   name = title_str
                 )
               return(p)
             }
             
             spi_plot_fast= function(data, title_str){
               plot(data$time, data$spi, col = data$col, main = title_str, 
                        xlim = c(as.POSIXct(c(data$time[length(data$time)-365*10],
                                             data$time[length(data$time)]), format = "%Y-%m-%d")),
                       type = "h", xlab = "Time", ylab = "SPI")
             }
             
            
             
             
             hist_plot = function(data, title_str){
               
               data$bins = cut(data$spi, breaks=c(-Inf, seq(-3.5,3.5, length.out = 99), Inf))
               
               count = data %>% 
                 dplyr::group_by(bins, .drop=FALSE)%>%
                 dplyr::summarize(n = length(bins))
               
               count$n[count$n == 0] = NA
               
                count$bin_edge = seq(-3.5,3.5, length.out = 100)
               
               hist_plot = ggplot(data=count, aes(x = bin_edge, y = n)) + 
                 geom_bar(stat = "identity", aes(fill=bin_edge, colour = bin_edge), size = 0.5)+
                 stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, colour = "black")+
                 scale_fill_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
                 scale_colour_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
                 geom_vline(xintercept = data$spi[length(data$spi)], size = 2)+
                 xlab(title_str)+
                 ylab("Frequency")+
                 theme_bw(base_size = base_font_size)+
                 ggtitle("")+
                 theme(legend.position="none")
               return(hist_plot)
             }
             
             precip_plot = function(data, title_str){
               precip_plot = ggplot(data = data, aes(x = time, y = monthly_sum, label = as.character(percent_average)))+
                 geom_bar(stat = 'identity', fill = "blue")+
                 xlab("")+
                 ylab(title_str)+
                 theme_bw(base_size = base_font_size)+
                 ggtitle("")+
                 geom_text(data = data, aes(x = time, y = monthly_sum, label = paste(percent_average,'%',sep = "")), nudge_y = 20, size = 3)+
                 theme(legend.position="none",
                       axis.text.x = element_text(angle = 60, vjust = 0.5))+
                 scale_x_datetime(breaks = date_breaks("2 month"), labels=date_format("%b / %Y"), 
                                  limits= c(data$time[length(data$time)-24]-(86400*12.6), data$time[length(data$time)]+(86400*12.6))) #add perfect amount of time for clean plotting
               return(precip_plot)
             }
             
             base_font_size = 16
             
             #raw Precip
             monthly_precip_plot = precip_plot(monthly_precip1,"Precipitation (mm)")
             
             #calcualte timeseries plots
             spi_30_plot = spi_plot(spi_30, "30 Day SPI")
             spi_365_plot = spi_plot(spi_365, "365 Day SPI")
             
             #calcualte histogram plots
             spi_30_hist = hist_plot(spi_30, "30 Day SPI")
             spi_365_hist = hist_plot(spi_365, "365 Day SPI")
             
             #combine for presentation
             final_plot = gridExtra::grid.arrange(monthly_precip_plot, spi_30_plot,spi_365_plot, 
                                                  spi_30_hist, spi_365_hist,
                                                  layout_matrix = rbind(c(1,1,1,1,1),
                                                                        c(2,2,2,4,4),
                                                                        c(3,3,3,5,5)))
             return(final_plot)
           }
           
           ########### Initial Lat Long (Missoula) ##########
           
           # output$testPlot <- renderPlot({
           #   spi_calc_plot(46.865933, -113.985862)
           # })
           
           ############# User Defined Lat Long ############
           
           observeEvent(input$mymap_draw_new_feature,{
             feature <- input$mymap_draw_new_feature
             
             output$testPlot <- renderPlot({
               
               spi_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])
               
             })
             
           })
         }
         
      )