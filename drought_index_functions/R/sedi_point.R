library(ncdf4) 
library(dplyr)
library(lmomco)
library(lubridate)

sedi_point = function(x, time, time_scale){
  
  x = x
  time = time
  time_scale = time_scale
  
  data = data.frame(def = x, time = time, day = yday(time),
                    year = year(time))
  
  output.list = list()
  
  #Start SEDI calculation for multiple timescales
  for(t in 1:length(time_scale)){
    for(i in rev((length(data$time)-364):length(data$time))){
      #calcualte index vectors of interest based on time
      first_date_breaks = which(data$day == data$day[i])
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
      data_time_filter = data %>%
        slice(slice_vec) %>%
        tibble::add_column(group_by_vec = group_by_vec)%>%
        group_by(group_by_vec)%>%
        dplyr::summarise(sum = sum(def))
      
      #compute date time for day/year of interest
      date_time = as.POSIXct(paste(data$day[first_date_breaks], data$year[first_date_breaks], sep = "-"), format = "%j-%Y")
      
      #Unbiased Sample Probability-Weighted Moments (following Vicente-Serrano et al., 2018)
      pwm = pwm.ub(data_time_filter$sum)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit generalized logistic
      fit.parglo = parglo(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfglo(data_time_filter$sum, fit.parglo)
      
      #equaprobaility transformation for cdf quantiles
      if(i == length(data$time)){
        output.df = data.frame(time = date_time,
                               sedi = qnorm(fit.cdf, mean = 0, sd = 1))
      }
      else{
        output.df = rbind(output.df, data.frame(time = date_time,
                                                sedi = qnorm(fit.cdf, mean = 0, sd = 1)))
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
