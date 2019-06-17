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
  eddi_365 = eddi_calc(c,365)
  
  #calculate monthly pet totals
  monthly_pet1 = c %>%
    dplyr:::group_by(as.yearmon(time)) %>%
    dplyr:::summarise(monthly_sum = sum(data, na.rm = T))
  
  monthly_pet1$time = as.POSIXct(strptime(as.Date(monthly_pet1$`as.yearmon(time)`), format = "%Y-%m-%d", tz = "UTC"))
  
  mean_monthly_pet = monthly_pet1 %>%
    dplyr::group_by(month(time)) %>%
    dplyr::summarise(monthly_mean = mean(monthly_sum, na.rm = T))
  
  monthly_pet1$percent_average = round(100*(monthly_pet1$monthly_sum / rep(mean_monthly_pet$monthly_mean, length.out = length(monthly_pet1$time))),0)
  
  color_ramp = colorRampPalette(rev(c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#000d66")))
  
  eddi_plot = function(data,title_str){
    plot1 = ggplot(data = data, aes(x = time, y = eddi))+
      geom_bar(stat = "identity", aes(colour=eddi), size = 0.5)+
      scale_color_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
      theme_bw(base_size = base_font_size)+
      xlab("Time")+
      ylab("EDDI")+
      theme(legend.position="none")+
      ylim(c(-3.5,3.5))+
      ggtitle(title_str)+
      scale_x_datetime(limits = as.POSIXct(c(data$time[length(data$time)-365*4],data$time[length(data$time)]), format = "%Y-%m-%d"))
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
    
    data$bins = cut(data$eddi, breaks=c(-Inf, seq(-3.5,3.5, length.out = 99), Inf))
    
    count = data %>% 
      dplyr::group_by(bins, .drop=FALSE)%>%
      dplyr::summarize(n = length(bins))
    
    count$n[count$n == 0] = NA
    
    count$bin_edge = seq(-3.5,3.5, length.out = 100)
    
    hist_plot = ggplot(data=count, aes(x = bin_edge, y = n)) + 
      geom_bar(stat = "identity", aes(fill=bin_edge, colour = bin_edge), size = 0.5)+
      #stat_smooth(method = lm, formula = y ~ poly(x, 10), se = FALSE, colour = "black")+
      scale_fill_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
      scale_colour_gradientn(colours = color_ramp(100), limits = c(-3.5,3.5))+
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
  eddi_365_plot = eddi_plot(eddi_365, "365 Day EDDI")
  
  #calcualte histogram plots
  eddi_30_hist = hist_plot(eddi_30, "30 Day EDDI")
  eddi_365_hist = hist_plot(eddi_365, "365 Day EDDI")
  
  #combine for presentation
  final_plot = gridExtra::grid.arrange(pet_plot, eddi_30_plot,eddi_365_plot,
                                       eddi_30_hist, eddi_365_hist,
                                       layout_matrix = rbind(c(1,1,1,1,1),
                                                             c(2,2,2,4,4),
                                                             c(3,3,3,5,5)))
  return(final_plot)
}