library(shiny)
library(leaflet)
library(leaflet.extras)

options(height = 1000)

shinyApp(
  ui <- fluidPage(
    leafletOutput("mymap",height=400, width = 700),
    mainPanel(
      plotOutput("testPlot",height=900, width = 700)
    )
  ),
  server <- function(input, output) {
    #
    output$mymap <- renderLeaflet(
      leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldStreetMap) %>% #Add normal map
        addWMSTiles(
          "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
          layers = "nexrad-n0r-900913",
          options = WMSTileOptions(format = "image/png", transparent = TRUE)
        ) %>%
        setView(lng = -110.5, lat = 46.5, zoom = 6) %>%
        addDrawToolbar(markerOptions = drawMarkerOptions(),
                       polylineOptions = FALSE,
                       polygonOptions = FALSE,
                       circleOptions = FALSE,
                       rectangleOptions = FALSE,
                       circleMarkerOptions = FALSE, 
                       editOptions = FALSE,
                       singleFeature = TRUE,
                       targetGroup='draw')
    )
    
    
    spi_calc_plot = function(lat_in, lon_in){
      # generate an rnorm distribution and plot it
      ## LOAD THE REQUIRED LIBRARYS
      library(ncdf4) # Downlaoded from https://github.com/pmjherman/r-ncdf4-build-opendap-windows
      library(lubridate)
      library(dplyr)
      library(zoo)
      library(plyr)
      library(rowr)
      library(precintcon)
      library(gridExtra)
      
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
      
      #function to format netcdf to spi input format
      daily_data_table = function(data){
        
        #calcuculate time identifiers to parse and join data
        data$year_mon = as.yearmon(data$time)
        data$day = day(data$time)
        data$year = year(data$time)
        data$month = month(data$time)
        
        #create empty list for storing daily data
        daily_data = list()
        
        for(i in 1:31){
          daily_data[[i]] = filter(data, day == i)
          if(i == 1){
            daily_data_formatted = data.frame(daily_data[[1]][c('year','month',"year_mon",'data')])
          }
          else{
            temp = data.frame(daily_data[[i]][c('data', "year_mon")])
            daily_data_formatted = dplyr::left_join(daily_data_formatted, temp, 
                                                    by = c("year_mon" = "year_mon"), copy = T)
          }
        }
        
        #remove unwanted column used during parsing
        daily_data_formatted$year_mon = NULL
        
        #rename collumns
        d_cols = paste("d", seq(1:31), sep = "")
        colnames(daily_data_formatted) = c('year','month', d_cols)
        
        #remove incomplete month
        #daily_data_formatted = daily_data_formatted[1:(nrow(daily_data_formatted)-1),]
        
        #define dataframe as.daily for spi package
        daily_data_formatted = as.daily(daily_data_formatted, na.value = NA)
        
        return(daily_data_formatted)
      }
      
      #orginize data
      daily_data_formatted = daily_data_table(c)
      
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
        
      #function to calcualte spi 
      spi_calc = function(daily_data_formatted,timescale){
        spi = precintcon::spi(daily_data_formatted, period = timescale, distribution = "Gamma")
        spi$time = as.POSIXct(paste(spi$year, spi$month,"1", sep = "/", format = "%Y/%m/%d"))
        spi$col = spi$spi
        spi$col[spi$col > 0] = "Wet"
        spi$col[spi$col < 0] = "Dry"
        return(spi)
      }
      
      #calcualte SPI for different time scales
      spi_three_month = spi_calc(daily_data_formatted, 3)
      spi_six_month = spi_calc(daily_data_formatted, 6)
      spi_twelve_month = spi_calc(daily_data_formatted, 12)
      
      base_font_size = 16
      
      spi_plot = function(data,title_str){
        plot1 = ggplot(data = data, aes(x = time, y = spi))+
          geom_bar(stat = "identity", aes(fill=col), size = 1.5)+
          scale_fill_manual(values = c("#ff0000", "#0000FF"))+
          theme_bw(base_size = base_font_size)+
          xlab("Time")+
          ylab("SPI")+
          theme(legend.position="none")+
          ylim(c(-3.2,3.2))+
          ggtitle(title_str)
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
      
      #raw Precip
      monthly_precip_plot = precip_plot(monthly_precip1,"Precipitation (mm)")
      
      #monthly_precip_plot
      
      #calcualte timeseries plots
      three_month_plot = spi_plot(spi_three_month, "3 Month SPI")
      six_month_plot = spi_plot(spi_six_month, "6 Month SPI")
      twelve_month_plot = spi_plot(spi_twelve_month, "12 Month SPI")
      
      #calcualte histogram plots
      three_month_hist = hist_plot(spi_three_month, "3 Month SPI")
      six_month_hist = hist_plot(spi_six_month, "6 Month SPI")
      twelve_month_hist = hist_plot(spi_twelve_month, "12 Month SPI")
      
      #combine for presentation
      final_plot = gridExtra::grid.arrange(monthly_precip_plot, three_month_plot,six_month_plot,twelve_month_plot, 
                                           three_month_hist, six_month_hist,twelve_month_hist,
                                           layout_matrix = rbind(c(1,1,1,1,1),
                                                                 c(2,2,2,5,5),
                                                                 c(3,3,3,6,6),
                                                                 c(4,4,4,7,7)))
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
      