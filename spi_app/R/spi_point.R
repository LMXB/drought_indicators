library(ncdf4) 
library(dplyr)
library(lmomco)
library(lubridate)
source("/home/zhoylman/drought_indicators/spi_app/R/SPI_Function.R")

spi_point = function(lat_in, lon_in, time_scale){
  lat_of_interest = lat_in
  lon_of_interest = lon_in
  time_scale = time_scale
  
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
  precip_data <- data.frame(time,data)
  
  ## CLOSE THE FILE
  nc_close(nc)
  
  #define some date based variables
  precip_data$day = yday(precip_data$time)
  precip_data$year = year(precip_data$time)
  precip_data$month = month(precip_data$time)
  
  output.list = list()
  
  #Start SPI calculation
  for(t in 1:length(time_scale)){
    for(i in rev((length(precip_data$time)-364):length(precip_data$time))){
      #calcualte index vectors of interest based on time
      first_date_breaks = which(precip_data$day == precip_data$day[i])
      second_date_breaks = first_date_breaks-(time_scale[t]-1)
      
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
      data_time_filter = precip_data %>%
        slice(slice_vec) %>%
        tibble::add_column(group_by_vec = group_by_vec)%>%
        group_by(group_by_vec)%>%
        dplyr::summarise(sum = sum(data))
      
      #remove zeros because they cause the gamma dist to blow up to Inf
      data_time_filter$sum[data_time_filter$sum == 0] = 0.01
      
      #compute date time for day/year of interest
      date_time = as.POSIXct(paste(precip_data$day[first_date_breaks], precip_data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
      
      #fit gamma distrobution to data
      fit.gamma = gamma_fit(data_time_filter$sum)
      #calcualte CDF values for the theoretical distrobution
      fit.cdf = pgamma(data_time_filter$sum, shape = fit.gamma$shape, rate = fit.gamma$rate)
      
      #equaprobaility transformation for cdf quantiles
      if(i == length(precip_data$time)){
        output.df = data.frame(time = date_time, 
                               spi = qnorm(fit.cdf, mean = 0, sd = 1))
      }
      
      else{
        output.df = rbind(output.df, data.frame(time = date_time, 
                                                spi = qnorm(fit.cdf, mean = 0, sd = 1)))
      } 
    }
    output.df = output.df[order(output.df$time),]
    output.list[[t]] = output.df
  }
  #if there is only one timescale to calculate return a data frame
  if(length(time_scale) == 1){
    return(output.df)
  }
  # otherwise return a list
  else{
    return(output.list)
  }
}
