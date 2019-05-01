
#SPEI Data
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

current_usdm = st_read("../USDM_current/current_usdm.shp")

current_usdm_date = read.csv("../USDM_current/usdm_time.csv")
current_usdm_date = as.Date(as.character(current_usdm_date$x), format = "%Y%m%d")

states = st_read("../shp_kml/states.shp")

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
                  textOutput('time'),
                  tags$head(tags$style("#time{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }"
                  )
                  ),
           #mainPanel(width = "100%",
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
           
           output$mymap = renderLeaflet({
             leaflet() %>%
               addTiles() %>%
               setView(lng = -108, lat = 46.5, zoom = 6)
           })
           
           
           watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_365, watersheds_water_year, watersheds_year_to_date)
           county_list = list(county_30, county_60, county_90, county_180, county_365, county_water_year, county_year_to_date)
           
           watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "365 Day HUC8", "Water Year HUC8", "Year to Date HUC8")
           watershed_raster_names = c("30 Day", "60 Day", "90 Day", "180 Day", "365 Day", "Water Year", "Year to Date")
           
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
           
           labels_usdm = list()
           labels_usdm[[1]] <- sprintf(
             "<strong>%s</strong><br/>USDM = D%g<sup></sup>",
             current_usdm_date, current_usdm$DM
           ) %>% lapply(htmltools::HTML)
           
           #color pallets
           pal_watershed <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                                     domain = -3.5:3.5, bins = seq(-3.5,3.5,0.5))
           
           pal <- colorNumeric(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#003366"), -3.5:3.5, na.color = "transparent")
           
           # pal <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
           #                 domain = -3.5:3.5, bins = c(-Inf,-3,-2.5,-2,-1.2,-0.7,-0.2,0.2,0.7,1.2,2,2.5,3,Inf), na.color = "transparent")
           
           pal_usdm <- colorBin(colorRamp(c("#ffff00", "#918151", "#ffa500", "#ff0000", "#811616"), interpolate = "spline"), 
                                domain = 0:4, bins = seq(0,4,1))
           
           
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
             addRasterImage(current_spei_365, colors = pal, opacity = 0.8, group = "365 Day") %>%
             addRasterImage(current_spei_water_year, colors = pal, opacity = 0.8, group = "Water Year") %>%
             addRasterImage(current_spei_year_to_date, colors = pal, opacity = 0.8, group = "Year to Date") %>%
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
             addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1,  color = "black", 
                         fillOpacity = 0.5, highlight = 
                           highlightOptions(weight = 5, bringToFront = TRUE,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"))
             
           
            
           # Add Layer Controls  ----------------------------------------------    
           m_raster = m_raster %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               overlayGroups = c("USDM","States"),
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
             addTiles() %>%
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_huc = m_huc %>% addPolygons(data = watershed_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "black", 
                                           dashArray = "3", fillOpacity = 0.7, highlight = 
                                             highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels[[i]], 
                                           labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
             if(i == length(watershed_list_names)){
               m_huc = m_huc %>% 
                 addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black", 
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
             addTiles() %>% 
             addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
           
           # Add multiple layers with a loop ----------------------------------------------
           for(i in 1:length(watershed_list_names)){
             m_county = m_county %>% addPolygons(data = county_list[[i]], group = watershed_raster_names[i], fillColor = ~pal_watershed(average), weight = 2, opacity = 1, color = "black", 
                                                 dashArray = "3", fillOpacity = 0.7, highlight = 
                                                   highlightOptions(weight = 5,color = "#666",dashArray = "",fillOpacity = 0.7, bringToFront = TRUE),label = labels_county[[i]], 
                                                 labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
             if(i == length(watershed_list_names)){
               m_county = m_county %>%
                 addPolygons(data = current_usdm, group = "USDM", fillColor = ~pal_usdm(DM), weight = 2, opacity = 1, color = "black", 
                                                   fillOpacity = 0.5, highlight = 
                                                     highlightOptions(weight = 5,color = "#666",fillOpacity = 0.7),label = labels_usdm[[1]], 
                                                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) 
             }
           }
           
           
           # Add Layer Controls  ----------------------------------------------    
           m_county = m_county %>%
             addLayersControl(position = "topleft",
               baseGroups = watershed_raster_names,
               overlayGroups = c("USDM","States"),
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
           
           spei_calc_plot = function(lat_in, lon_in){
             lat_of_interest = lat_in
             lon_of_interest = lon_in
             
             ### DEFINE THE URL to net cdf
             urltotal_precip<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
             urltotal_pet<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc"
             
             urls = c(urltotal_precip, urltotal_pet)
             var=c("precipitation_amount", "daily_mean_reference_evapotranspiration_grass")
             
             dataset = list()
             
             for(i in 1:length(urls)){
               ## OPEN THE FILES
               nc <- nc_open(urls[i])
               
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
               
               ## READ THE DATA VARIABLE 
               data <- ncvar_get(nc, var[i], start=c(lon,lat,1),count=c(1,1,endcount))
               
               ## READ THE TIME VARIABLE
               time <- ncvar_get(nc, "day", start=c(1),count=c(endcount))
               ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
               time=as.Date(time, origin="1900-01-01") 
               # PUT EVERYTHING INTO A DATA FRAME
               dataset[[i]] <- data.frame(time,data)
               
               ## CLOSE THE FILE
               nc_close(nc)
               
               #define some date based variables
               dataset[[i]]$day = yday(dataset[[i]]$time)
               dataset[[i]]$year = year(dataset[[i]]$time)
               dataset[[i]]$month = month(dataset[[i]]$time)
             }
             
             diff_data = data.frame(time = dataset[[1]]$time,
                                    data = dataset[[1]]$data - dataset[[2]]$data,
                                    day = dataset[[1]]$day,
                                    year = dataset[[1]]$year,
                                    month = dataset[[1]]$month)
             
             spei_calc = function(data, time_scale){
               #Start SPEI calculation
               for(i in rev((length(data$time)-364):length(data$time))){
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
                 
                 #compute date time for day/year of interest
                 date_time = as.POSIXct(paste(data$day[first_date_breaks], data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
                 
                 #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
                 pwm = pwm.ub(data_time_filter$sum)
                 #Probability-Weighted Moments to L-moments
                 lmoments_x = pwm2lmom(pwm)
                 #fit generalized logistic
                 fit.parglo = parglo(lmoments_x)
                 #compute probabilistic cdf 
                 fit.cdf = cdfglo(data_time_filter$sum, fit.parglo)
                 
                 #equaprobaility transformation for cdf quantiles
                 if(i == length(data$time)){
                   output.df = data.frame(spei = qnorm(fit.cdf, mean = 0, sd = 1),
                                          time = date_time)
                 }
                 else{
                   output.df = rbind(output.df, data.frame(spei = qnorm(fit.cdf, mean = 0, sd = 1),
                                                           time = date_time))
                 }
                 
               }
               
               output.df = output.df[order(output.df$time),]
               output.df$col = output.df$spei
               output.df$col[output.df$col < 0] = "red"
               output.df$col[as.numeric(output.df$col) > 0] = "blue"
               
               return(output.df)
             }
             
             spei_30 = spei_calc(diff_data,30)
             spei_365 = spei_calc(diff_data,365)
             
             #calculate monthly precip totals
             monthly_precip1 = dataset[[1]] %>%
               dplyr:::group_by(as.yearmon(time)) %>%
               dplyr:::summarise(monthly_sum = sum(data, na.rm = T))
             
             #calculate monthly pet totals
             monthly_pet1 = dataset[[2]] %>%
               dplyr:::group_by(as.yearmon(time)) %>%
               dplyr:::summarise(monthly_sum = sum(data, na.rm = T))
             
             monthly_precip1$time = as.POSIXct(strptime(as.Date(monthly_precip1$`as.yearmon(time)`), format = "%Y-%m-%d", tz = "UTC"))
             monthly_pet1$time = as.POSIXct(strptime(as.Date(monthly_pet1$`as.yearmon(time)`), format = "%Y-%m-%d", tz = "UTC"))
             
             #calcualte mean monthly precip
             mean_monthly_precip = monthly_precip1 %>%
               dplyr::group_by(month(time)) %>%
               dplyr::summarise(monthly_mean = mean(monthly_sum, na.rm = T))
             
             mean_monthly_pet = monthly_pet1 %>%
               dplyr::group_by(month(time)) %>%
               dplyr::summarise(monthly_mean = mean(monthly_sum, na.rm = T))
             
             monthly_precip1$percent_average = round(100*(monthly_precip1$monthly_sum / rep(mean_monthly_precip$monthly_mean, length.out = length(monthly_precip1$time))),0)
             monthly_pet1$percent_average = round(100*(monthly_pet1$monthly_sum / rep(mean_monthly_pet$monthly_mean, length.out = length(monthly_pet1$time))),0)
             
             monthly_precip1$var = "blue"
             monthly_pet1$var = "red"
             
             monthly_total = rbind(monthly_precip1, monthly_pet1)
             
             spei_plot = function(data,title_str){
               plot1 = ggplot(data = data, aes(x = time, y = spei))+
                 geom_bar(stat = "identity", aes(fill=col), size = 1.5)+
                 scale_fill_manual(values = c("#0000FF","#ff0000"))+
                 theme_bw(base_size = base_font_size)+
                 xlab("Time")+
                 ylab("SPEI")+
                 theme(legend.position="none")+
                 ylim(c(-3.5,3.5))+
                 ggtitle(title_str)+
                 scale_x_datetime(limits = as.POSIXct(c(data$time[length(data$time)-365*10],data$time[length(data$time)]), format = "%Y-%m-%d"))
               return(plot1)
             }
             
             # #interactive plot
             # spi_plot_interactive = function(data, title_str){
             #   p <- plot_ly() %>%
             #     add_bars(
             #       x = data$time,
             #       y = data$spi,
             #       color = data$col,
             #       name = title_str
             #     )
             #   return(p)
             # }
             
             # spi_plot_fast= function(data, title_str){
             #   plot(data$time, data$spi, col = data$col, main = title_str, 
             #        xlim = c(as.POSIXct(c(data$time[length(data$time)-365*10],
             #                              data$time[length(data$time)]), format = "%Y-%m-%d")),
             #        type = "h", xlab = "Time", ylab = "SPI")
             # }
             # 
             
             hist_plot = function(data, title_str){
               hist_plot = ggplot(data=data, aes(spei)) + 
                 geom_histogram(binwidth = 0.05, aes(fill = col))+
                 scale_fill_manual(values = c("#0000FF","#ff0000"))+
                 geom_vline(xintercept = data$spei[length(data$spei)], size = 2)+
                 xlab(title_str)+
                 ylab("Frequency")+
                 theme_bw(base_size = base_font_size)+
                 ggtitle("")+
                 theme(legend.position="none")
               return(hist_plot)
             }
             
             precip_plot = function(data, title_str){
               precip_plot = ggplot(data = data, aes(x = time, y = monthly_sum, label = as.character(percent_average), fill = var,
                                                       color = as.character(var)))+
                 geom_bar(stat = 'identity', position = "dodge")+
                 scale_fill_manual(values = c("#0000ff", "#ff0000"))+
                 scale_color_manual(values = c("#0000ff", "#ff0000"))+
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
             monthly_precip_plot = precip_plot(monthly_total, "Precipitation / PET (mm)")
             
             #calcualte timeseries plots
             spei_30_plot = spei_plot(spei_30, "30 Day SPEI")
             spei_365_plot = spei_plot(spei_365, "365 Day SPEI")
             
             #calcualte histogram plots
             spei_30_hist = hist_plot(spei_30, "30 Day SPEI")
             spei_365_hist = hist_plot(spei_365, "365 Day SPEI")
             
             #combine for presentation
             final_plot = gridExtra::grid.arrange(monthly_precip_plot, spei_30_plot,spei_365_plot, 
                                                  spei_30_hist, spei_365_hist,
                                                  layout_matrix = rbind(c(1,1,1,1,1),
                                                                        c(2,2,2,4,4),
                                                                        c(3,3,3,5,5)))
             return(final_plot)
           }
           
           ########### Initial Lat Long (Missoula) ##########
           
           # output$testPlot <- renderPlot({
           #   spei_calc_plot(46.865933, -113.985862)
           # })
           
           ############# User Defined Lat Long ############
           
           observeEvent(input$mymap_draw_new_feature,{
             feature <- input$mymap_draw_new_feature
             
             output$testPlot <- renderPlot({
               
               spei_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])
               
             })
             
           })
         }
  
)