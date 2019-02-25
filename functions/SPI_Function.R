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

# #troubleshooting data
# c = read.csv("C:\\Users\\zhoyl\\Google Drive\\Drought_Markdown\\R_Markdown_UMRB\\precip_data.csv")
# c = read.csv("C:\\Users\\zachary.hoylman.UM\\Google Drive\\Drought_Markdown\\R_Markdown_UMRB\\precip_data.csv")
# 
# # #calculate time ids for filtering and grouping
# c$day = yday(c$time)
# c$year = year(c$time)
# c$month = month(c$time)
# 
# #troubleshooting parameters
# data = c
# time_scale = 1000
# i = length(data$time)



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
    
    #compute date time for day/year of interest
    date_time = as.POSIXct(paste(data$day[first_date_breaks], data$year[first_date_breaks], sep = "-"), format = "%j-%Y")

    #if you cant fit the gamma distrobution move on to next i
    possibleError <- tryCatch(
      fitdist(data_time_filter$sum, distr = "gamma", method = "mle"),
      error=function(e) e
    )
    
    if(inherits(possibleError, "error")) next
    
    #fit gamma distrobution to data
    fit.gamma = fitdist(data_time_filter$sum, distr = "gamma", method = "mle") 
    
    #calcualte CDF values for the theoretical distrobution
    fit.cdf = pgamma(data_time_filter$sum, fit.gamma$estimate[1], fit.gamma$estimate[2])
    
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
# 
# tic()
# spi_30 = spi_calc(c,30)
# toc()
# tic()
# spi_100 = spi_calc(c,100)
# toc()
# tic()
# spi_200 = spi_calc(c,200)
# toc()
# tic()
# spi_330 = spi_calc(c,330)
# toc()
# tic()
# spi_400 = spi_calc(c,400)
# toc()
# tic()
# spi_1000 = spi_calc(c,1000)
# toc()
# 
# 
# plot(spi_30$time, spi_30$spi, type = "l",xlim=c(as.POSIXct('2012-01-01', format="%Y-%m-%d"),
#                                                 as.POSIXct('2019-02-18', format="%Y-%m-%d")))
# lines(spi_100$time, spi_100$spi, col = "red")
# lines(spi_200$time, spi_200$spi, col = "blue")
# lines(spi_330$time, spi_330$spi, col = "green")
# lines(spi_1000$time, spi_1000$spi, col = "forestgreen")
# 
# spi_plot = function(data,title_str){
#   plot1 = ggplot(data = data, aes(x = time, y = spi))+
#     geom_bar(stat = "identity", aes(fill=col), size = 1.5)+
#     scale_fill_manual(values = c("#ff0000", "#0000FF"))+
#     theme_bw(base_size = base_font_size)+
#     xlab("Time")+
#     ylab("SPI")+
#     theme(legend.position="none")+
#     ylim(c(-3.2,3.2))+
#     ggtitle(title_str)
#   return(plot1)
# }
# 
# base_font_size = 16
# 
# spi_plot_30 = spi_plot(spi_30, "1 Month SPI")
# spi_plot_30
# 
# spi_plot_1000 = spi_plot(spi_1000, "1000 Day SPI")
# spi_plot_1000
