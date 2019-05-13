library(ncdf4) 
library(dplyr)
library(lmomco)
library(lubridate)

spei_point = function(lat_in, lon_in, time_scale){
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
  
  #Start SPEI calculation
  for(i in rev((length(diff_data$time)-364):length(diff_data$time))){
    #calcualte index vectors of interest based on time
    first_date_breaks = which(diff_data$day == diff_data$day[i])
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
    data_time_filter = diff_data %>%
      slice(slice_vec) %>%
      tibble::add_column(group_by_vec = group_by_vec)%>%
      group_by(group_by_vec)%>%
      dplyr::summarise(sum = sum(data))
    
    #compute date time for day/year of interest
    date_time = as.POSIXct(paste(diff_data$day[first_date_breaks], diff_data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
    
    #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
    pwm = pwm.ub(data_time_filter$sum)
    #Probability-Weighted Moments to L-moments
    lmoments_x = pwm2lmom(pwm)
    #fit generalized logistic
    fit.parglo = parglo(lmoments_x)
    #compute probabilistic cdf 
    fit.cdf = cdfglo(data_time_filter$sum, fit.parglo)
    
    #equaprobaility transformation for cdf quantiles
    if(i == length(diff_data$time)){
      output.df = data.frame(time = date_time,
                             spei = qnorm(fit.cdf, mean = 0, sd = 1))
    }
    else{
      output.df = rbind(output.df, data.frame(time = date_time,
                                              spei = qnorm(fit.cdf, mean = 0, sd = 1)))
    }
    
  }
  
  output.df = output.df[order(output.df$time),]
  return(output.df)
}
