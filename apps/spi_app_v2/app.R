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
library(ncdf4) # Downlaoded from https://github.com/pmjherman/r-ncdf4-build-opendap-windows
library(lubridate)


options(height = 1000)

#actual app
shinyApp(
  ui <- fluidPage(
    sidebarPanel(br(),
      actionButton("evRaster", "Raw Map"),
      actionButton("evHUC", "Watersheds"),
      actionButton("evCounty", "County"), width = 5),
    leafletOutput("mymap",height=400, width = 700),
    mainPanel(
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
      plotOutput("testPlot",height=900, width = 700) %>% withSpinner(color="#0dc5c1", type = 8, proxy.height = "200px") 
    )
  ),
  server <- function(input, output) {
    map_path = "Y:\\Projects\\\\MCO_Drought_Indicators\\"
    
    current_spi_30 = raster::raster(paste(map_path, "maps\\current_spi\\current_spi_30.tif", sep = ""))
    current_spi_60 = raster::raster(paste(map_path, "maps\\current_spi\\current_spi_60.tif", sep = ""))
    current_spi_90 = raster::raster(paste(map_path, "maps\\current_spi\\current_spi_90.tif", sep = ""))
    current_spi_180 = raster::raster(paste(map_path, "maps\\current_spi\\current_spi_180.tif", sep = ""))
    current_spi_300 = raster::raster(paste(map_path, "maps\\current_spi\\current_spi_300.tif", sep = ""))
    
    watersheds_30 = st_read(paste(map_path, "shp\\current_spi\\current_spi_watershed_30.shp", sep = ""))
    watersheds_60 = st_read(paste(map_path, "shp\\current_spi\\current_spi_watershed_60.shp", sep = ""))
    watersheds_90 = st_read(paste(map_path, "shp\\current_spi\\current_spi_watershed_90.shp", sep = ""))
    watersheds_180 = st_read(paste(map_path, "shp\\current_spi\\current_spi_watershed_180.shp", sep = ""))
    watersheds_300 = st_read(paste(map_path, "shp\\current_spi\\current_spi_watershed_300.shp", sep = ""))
    
    county_30 = st_read(paste(map_path, "shp\\current_spi\\current_spi_county_30.shp", sep = ""))
    county_60 = st_read(paste(map_path, "shp\\current_spi\\current_spi_county_60.shp", sep = ""))
    county_90 = st_read(paste(map_path, "shp\\current_spi\\current_spi_county_90.shp", sep = ""))
    county_180 = st_read(paste(map_path, "shp\\current_spi\\current_spi_county_180.shp", sep = ""))
    county_300 = st_read(paste(map_path, "shp\\current_spi\\current_spi_county_300.shp", sep = ""))
    
    watershed_list = list(watersheds_30, watersheds_60, watersheds_90, watersheds_180, watersheds_300)
    county_list = list(county_30, county_60, county_90, county_180, county_300)
    
    watershed_list_names = c("30 Day HUC8", "60 Day HUC8", "90 Day HUC8", "180 Day HUC8", "300 Day HUC8")
    watershed_raster_names = c("30 Day", "60 Day", "90 Day", "180 Day", "300 Day")
    
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
          addRasterImage(current_spi_30, colors = pal, opacity = 0.8, group = "30 Day") %>%
          addRasterImage(current_spi_60, colors = pal, opacity = 0.8, group = "60 Day") %>%
          addRasterImage(current_spi_90, colors = pal, opacity = 0.8, group = "90 Day") %>%
          addRasterImage(current_spi_180, colors = pal, opacity = 0.8, group = "180 Day") %>%
          addRasterImage(current_spi_300, colors = pal, opacity = 0.8, group = "300 Day")
      
    # Add Layer Controls  ----------------------------------------------    
      m_raster = m_raster %>%
        addLayersControl(
            baseGroups = watershed_raster_names,
            options = layersControlOptions(collapsed = TRUE)) %>%
          addLegend(pal = pal, values = -3.5:3.5,
                    title = "Current SPI",
                    position = "bottomleft")%>%

    #     #add
    #     # addWMSTiles(
    #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    #     #   layers = "nexrad-n0r-900913",
    #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%

        setView(lng = -108, lat = 46.5, zoom = 5) %>%
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
        addLayersControl(
          baseGroups = watershed_raster_names,
          options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend(pal = pal, values = -3.5:3.5,
                  title = "Current SPI",
                  position = "bottomleft")%>%
        
        #     #add
        #     # addWMSTiles(
        #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        #     #   layers = "nexrad-n0r-900913",
        #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
        
        setView(lng = -108, lat = 46.5, zoom = 5) %>%
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
        addLayersControl(
          baseGroups = watershed_raster_names,
          options = layersControlOptions(collapsed = TRUE)) %>%
        addLegend(pal = pal, values = -3.5:3.5,
                  title = "Current SPI",
                  position = "bottomleft")%>%
        
        #     #add
        #     # addWMSTiles(
        #     #   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        #     #   layers = "nexrad-n0r-900913",
        #     #   options = WMSTileOptions(format = "image/png", transparent = TRUE, group = "Weather"))%>%
        
        setView(lng = -108, lat = 46.5, zoom = 5) %>%
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
    
    
    spi_calc_plot = function(lat_in, lon_in){
      # generate an rnorm distribution and plot it
      ## LOAD THE REQUIRED LIBRARYS
      
      lat_of_interest = lat_in
      lon_of_interest = lon_in
      
      
      ### DEFINE THE URL to net cdf
      urltotal<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
      
      ## OPEN THE FILE
      nc <- nc_open(urltotal)
      
      ## SHOW SOME METADATA if you need to ficure out the attribute name
      nc
      
      ## DISPLAY INFORMATION ABOUT AN ATTRIBUTE
      ncatt_get(nc,"precipitation_amount")
      
      ## GET DATA SIZES: http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get
      ## NOTE: FILE DIMENSIONS ARE lon,lat,time
      v3 <- nc$var[[1]]
      lonsize <- v3$varsize[1]
      latsize <- v3$varsize[2] 
      endcount <- v3$varsize[3] 
      
      ### DEFINE OUR POINT OF INTEREST 
      ## NOTE: MAKE SURE TO CHECK WHETHER YOUR SOURCE STARTS COUNTING AT 0 OR 1
      ## e.g. ncdf4 PACKAGE STARTS COUNTING AT 1 BUT OPeNDAP DATASET ACCESS FORM STARTS AT 0:
      
      lon_matrix = nc$var[[1]]$dim[[1]]$vals
      lat_matrix = nc$var[[1]]$dim[[2]]$vals
      
      lon=which(abs(lon_matrix-lon_of_interest)==min(abs(lon_matrix-lon_of_interest)))  
      lat=which(abs(lat_matrix-lat_of_interest)==min(abs(lat_matrix-lat_of_interest))) 
      
      
      ## DEFINE OUR VARIABLE NAME 
      var="precipitation_amount"
      
      ## READ THE DATA VARIABLE 
      ## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
      ## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
      ## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
      ## As a special case, the value "-1" indicates that all entries along that dimension should be read."
      data <- ncvar_get(nc, var, start=c(lon,lat,1),count=c(1,1,endcount))
      ## READ THE TIME VARIABLE
      time <- ncvar_get(nc, "day", start=c(1),count=c(endcount))
      ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
      time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
      # PUT EVERYTHING INTO A DATA FRAME
      c <- data.frame(time,data)
      
      ## CLOSE THE FILE
      nc_close(nc)
      
      c$day = yday(c$time)
      c$year = year(c$time)
      c$month = month(c$time)
      
      #load spi function from alternative file
      git_repo_path = "D:\\Git_Repo\\drought_indicators\\"
      source(paste(git_repo_path,"functions\\SPI_Function.R",sep = ""))
      
      spi_30 = spi_calc(c,30)
      spi_300 = spi_calc(c,300)
      
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
        
      
      spi_plot = function(data,title_str){
        plot1 = ggplot(data = data, aes(x = time, y = spi))+
          geom_bar(stat = "identity", aes(fill=col), size = 1.5)+
          scale_fill_manual(values = c("#ff0000", "#0000FF"))+
          theme_bw(base_size = base_font_size)+
          xlab("Time")+
          ylab("SPI")+
          theme(legend.position="none")+
          ylim(c(-3.5,3.5))+
          ggtitle(title_str)+
          scale_x_datetime(limits = as.POSIXct(c(data$time[length(data$time)-365*10],data$time[length(data$time)]), format = "%Y-%m-%d"))
        return(plot1)
      }
      
      hist_plot = function(data, title_str){
        hist_plot = ggplot(data=data, aes(spi)) + 
          geom_histogram(binwidth = 0.05, aes(fill = col))+
          scale_fill_manual(values = c("#ff0000", "#0000FF"))+
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
      
      #monthly_precip_plot
      
      #calcualte timeseries plots
      spi_30_plot = spi_plot(spi_30, "30 Day SPI")
      spi_300_plot = spi_plot(spi_300, "300 Day SPI")
      
      #calcualte histogram plots
      spi_30_hist = hist_plot(spi_30, "30 Day SPI")
      spi_300_hist = hist_plot(spi_300, "300 Day SPI")
     
      #combine for presentation
      final_plot = gridExtra::grid.arrange(monthly_precip_plot, spi_30_plot,spi_300_plot, 
                                           spi_30_hist, spi_300_hist,
                                           layout_matrix = rbind(c(1,1,1,1,1),
                                                                 c(2,2,2,4,4),
                                                                 c(3,3,3,5,5)))
      return(final_plot)
    }
    
    ########### Initial Lat Long (Missoula) ##########
    
    output$testPlot <- renderPlot({
      spi_calc_plot(46.865933, -113.985862)
    })
    
    ############# User Defined Lat Long ############
    
    observeEvent(input$mymap_draw_new_feature,{
      feature <- input$mymap_draw_new_feature
      
      output$testPlot <- renderPlot({
        
        spi_calc_plot(feature$geometry$coordinates[[2]],feature$geometry$coordinates[[1]])
        
      })
    })
  }
)
      