
#EDDI Data
current_eddi_30 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_30.tif")
current_eddi_60 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_60.tif")
current_eddi_90 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_90.tif")
current_eddi_180 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_180.tif")
current_eddi_300 = raster::raster("../eddi_app/maps/current_eddi/current_eddi_300.tif")

watersheds_30 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_30.shp")
watersheds_60 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_60.shp")
watersheds_90 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_90.shp")
watersheds_180 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_180.shp")
watersheds_300 = st_read("../eddi_app/shp/current_eddi/current_eddi_watershed_300.shp")

county_30 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_30.shp")
county_60 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_60.shp")
county_90 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_90.shp")
county_180 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_180.shp")
county_300 = st_read("../eddi_app/shp/current_eddi/current_eddi_county_300.shp")

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
        "<strong>%s</strong><br/>EDDI = %g<sup></sup>",
        watershed_list[[i]]$NAME, watershed_list[[i]]$average
      ) %>% lapply(htmltools::HTML)
    }
    
    labels_county = list()
    for(i in 1:length(watershed_list_names)){
      labels_county[[i]] <- sprintf(
        "<strong>%s</strong><br/>EDDI = %g<sup></sup>",
        county_list[[i]]$NAME, county_list[[i]]$average
      ) %>% lapply(htmltools::HTML)
    }
    
    labels_usdm = list()
    labels_usdm[[1]] <- sprintf(
      "<strong>%s</strong><br/>USDM = D%g<sup></sup>",
      current_usdm_date, current_usdm$DM
    ) %>% lapply(htmltools::HTML)
    
    #color pallets
    pal_watershed <- colorBin(colorRamp(rev(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66")), interpolate = "spline"), 
                              domain = -3.5:3.5, bins = seq(-3.5,3.5,0.5))
    
    pal <- colorNumeric(rev(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#003366")), -3.5:3.5, na.color = "transparent")
    
    # pal <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66"), interpolate = "spline"), 
    #                 domain = -3.5:3.5, bins = c(-Inf,-3,-2.5,-2,-1.2,-0.7,-0.2,0.2,0.7,1.2,2,2.5,3,Inf), na.color = "transparent")
    
    pal_usdm <- colorBin(colorRamp((c("#ffff00", "#918151", "#ffa500", "#ff0000", "#811616")), interpolate = "spline"), 
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
      addRasterImage(current_eddi_30, colors = pal, opacity = 0.8, group = "30 Day") %>%
      addRasterImage(current_eddi_60, colors = pal, opacity = 0.8, group = "60 Day") %>%
      addRasterImage(current_eddi_90, colors = pal, opacity = 0.8, group = "90 Day") %>%
      addRasterImage(current_eddi_180, colors = pal, opacity = 0.8, group = "180 Day") %>%
      addRasterImage(current_eddi_300, colors = pal, opacity = 0.8, group = "300 Day") %>%
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
                title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
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
                title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
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
                title = paste0("Current EDDI<br>", as.character(watersheds_30$crrnt_t[1])),
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
    
    eddi_calc_plot = function(lat_in, lon_in){
      lat_of_interest = lat_in
      lon_of_interest = lon_in

      ### DEFINE THE URL to net cdf
      urltotal_pet<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pet_1979_CurrentYear_CONUS.nc"

      var="daily_mean_reference_evapotranspiration_grass"
      
        ## OPEN THE FILES
        nc <- nc_open(urltotal_pet)

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
        c <- data.frame(time,data)

        ## CLOSE THE FILE
        nc_close(nc)

        #define some date based variables
        c$day = yday(c$time)
        c$year = year(c$time)
        c$month = month(c$time)

      eddi_calc = function(data, time_scale){
        #Start EDDI calculation
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

          #define coeffitients
          C0 = 2.515517
          C1 = 0.802853
          C2 = 0.010328
          d1 = 1.432788
          d2 = 0.189269
          d3 = 0.001308
          
          eddi_fun <- function(x) {
            # following Hobbins et al., 2016
            x = as.numeric(x)
            
            if(all(is.na(x))){
              return(NA)
            } else {
              
              #Rank PET (1 = max)
              rank_1 = rank(-x)
              
              #Calcualte emperical probabilities
              prob = ((rank_1 - 0.33)/(length(rank_1) + 0.33))
              
              #compute W (whaterver that is)
              W = numeric(length(prob))
              for(i in 1: length(prob)){
                if(prob[i] <= 0.5){
                  W[i] = sqrt(-2*log(prob[i]))
                } else {
                  W[i] = sqrt(-2*log(1 - prob[i]))
                }
              }
              
              #Find indexes which need inverse EDDI sign
              reverse_index = which(prob > 0.5)
              
              #Compute EDDI
              EDDI = W - ((C0 + C1*W + C2*W^2)/(1 + d1*W + d2*W^2 + d3*W^3))
              
              #Reverse sign of EDDI values where prob > 0.5
              EDDI[reverse_index] = -EDDI[reverse_index]
              
              #Return Current Value
              return(EDDI)
            }
          }

          #equaprobaility transformation for cdf quantiles
          if(i == length(data$time)){
            output.df = data.frame(eddi = eddi_fun(data_time_filter$sum),
                                   time = date_time)
          }
          else{
            output.df = rbind(output.df, data.frame(eddi = eddi_fun(data_time_filter$sum),
                                                    time = date_time))
          }

        }

        output.df = output.df[order(output.df$time),]
        output.df$col = output.df$eddi
        output.df$col[output.df$col < 0] = "blue"
        output.df$col[as.numeric(output.df$col) > 0] = "red"

        return(output.df)
      }

      eddi_30 = eddi_calc(c,30)
      eddi_300 = eddi_calc(c,300)

      #calculate monthly pet totals
      monthly_pet1 = c %>%
        dplyr:::group_by(as.yearmon(time)) %>%
        dplyr:::summarise(monthly_sum = sum(data, na.rm = T))

      monthly_pet1$time = as.POSIXct(strptime(as.Date(monthly_pet1$`as.yearmon(time)`), format = "%Y-%m-%d", tz = "UTC"))

      mean_monthly_pet = monthly_pet1 %>%
        dplyr::group_by(month(time)) %>%
        dplyr::summarise(monthly_mean = mean(monthly_sum, na.rm = T))

      monthly_pet1$percent_average = round(100*(monthly_pet1$monthly_sum / rep(mean_monthly_pet$monthly_mean, length.out = length(monthly_pet1$time))),0)

      eddi_plot = function(data,title_str){
        plot1 = ggplot(data = data, aes(x = time, y = eddi))+
          geom_bar(stat = "identity", aes(fill=col), size = 1.5)+
          scale_fill_manual(values = (c("#0000FF","#ff0000")))+
          theme_bw(base_size = base_font_size)+
          xlab("Time")+
          ylab("EDDI")+
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
        hist_plot = ggplot(data=data, aes(eddi)) +
          geom_histogram(binwidth = 0.05, aes(fill = col))+
          scale_fill_manual(values = (c("#0000FF","#ff0000")))+
          geom_vline(xintercept = data$eddi[length(data$eddi)], size = 2)+
          xlab(title_str)+
          ylab("Frequency")+
          theme_bw(base_size = base_font_size)+
          ggtitle("")+
          theme(legend.position="none")
        return(hist_plot)
      }

      pet_plot = function(data, title_str){
        pet_plot = ggplot(data = data, aes(x = time, y = monthly_sum, label = as.character(percent_average)))+
          geom_bar(stat = 'identity', fill = "red")+
          xlab("")+
          ylab(title_str)+
          theme_bw(base_size = base_font_size)+
          ggtitle("")+
          geom_text(data = data, aes(x = time, y = monthly_sum, label = paste(percent_average,'%',sep = "")), nudge_y = 20, size = 3)+
          theme(legend.position="none",
                axis.text.x = element_text(angle = 60, vjust = 0.5))+
          scale_x_datetime(breaks = date_breaks("2 month"), labels=date_format("%b / %Y"), 
                           limits= c(data$time[length(data$time)-24]-(86400*12.6), data$time[length(data$time)]+(86400*12.6))) #add perfect amount of time for clean plotting
        return(pet_plot)
      }
      
      base_font_size = 16

      #raw Precip
      pet_plot = pet_plot(monthly_pet1, "Potential Evapotranspiration (mm)")

      #calcualte timeseries plots
      eddi_30_plot = eddi_plot(eddi_30, "30 Day EDDI")
      eddi_300_plot = eddi_plot(eddi_300, "300 Day EDDI")

      #calcualte histogram plots
      eddi_30_hist = hist_plot(eddi_30, "30 Day EDDI")
      eddi_300_hist = hist_plot(eddi_300, "300 Day EDDI")

      #combine for presentation
      final_plot = gridExtra::grid.arrange(pet_plot, eddi_30_plot,eddi_300_plot,
                                           eddi_30_hist, eddi_300_hist,
                                           layout_matrix = rbind(c(1,1,1,1,1),
                                                                 c(2,2,2,4,4),
                                                                 c(3,3,3,5,5)))
      return(final_plot)
    }

    ########### Initial Lat Long (Missoula) ##########

    # output$testPlot <- renderPlot({
    #   eddi_calc_plot(46.865933, -113.985862)
    # })

    ############# User Defined Lat Long ############

    observeEvent(input$mymap_draw_new_feature,{
      feature <- input$mymap_draw_new_feature

      output$testPlot <- renderPlot({

        eddi_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])

      })

    })
  }
  
  )