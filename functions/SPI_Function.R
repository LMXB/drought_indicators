## LOAD THE REQUIRED LIBRARYS
library(lubridate)
library(dplyr)
library(zoo)
library(plyr)
library(rowr)
library(precintcon)
library(gridExtra)
library(fitdistrplus)
library(tictoc)

git_repo_path = "C:\\Users\\zhoyl\\Documents\\Git_Repo\\drought_indicators\\"

source(paste(git_repo_path,"functions\\gamma_fit.R",sep = ""))

# # #troubleshooting data
# c = read.csv("C:\\Users\\zhoyl\\Google Drive\\Drought_Markdown\\R_Markdown_UMRB\\precip_data.csv")
# c = read.csv("C:\\Users\\zachary.hoylman.UM\\Google Drive\\Drought_Markdown\\R_Markdown_UMRB\\precip_data.csv")
# 
# # # #calculate time ids for filtering and grouping
# c$day = yday(c$time)
# c$year = year(c$time)
# c$month = month(c$time)
# # 
# # #troubleshooting parameters
# data = c
# time_scale = 30
# i = length(data$time)
# 

spi_calc = function(data, time_scale){
  #Start SPI calculation
  for(i in rev((length(data$time)-364):length(data$time))){
    #calcualte index vectors of interest based on time
    first_date_breaks = which(data$day == data$day[i])
    second_date_breaks = first_date_breaks-(time_scale-1)
    
    #if there are negative indexes remove last year (incomplete data range)
    #change this to remove all indexes from both vectors that are negative
    
    # if(!all(second_date_breaks < 0)){
    #   first_date_breaks = first_date_breaks[-1]
    #   second_date_breaks = second_date_breaks[-1]
    # }
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
    data_time_filter$sum[data_time_filter$sum == 0] = NA
    
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
  output.df$col[output.df$col > 0] = "Wet"
  output.df$col[output.df$col < 0] = "Dry"
  
  return(output.df)
}
