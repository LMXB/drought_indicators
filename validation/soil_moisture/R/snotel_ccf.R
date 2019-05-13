library(dplyr)

load("/home/zhoylman/drought_indicators_data/snotel_spei/snotel_spei.RData")
load("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")

cross_cor = function(snotel_sm,snotel_spei){
  for(i in 1:length(snotel_spei)){
    x = snotel_sm
    y = snotel_spei[[i]]
    
    #convert time sype
    y$time = as.Date(y$time)
    
    #compute temporal breaks
    time_start_x = x$Date[1]
    time_start_y = y$time[1]
    time_start = min(c(time_start_x, time_start_y))
    
    time_end_x = x$Date[length(x$Date)]
    time_end_y = y$time[length(y$time)]
    time_end = max(c(time_end_x, time_end_y))
    
    #compute time scale 
    time_scale = y$time[1]+1 - as.Date("1979-01-01")
    
    #filter shared time periods
    x_filter = x %>% 
      filter(Date >= time_start & Date <= time_end)
    
    y_filter = y %>% 
      filter(time >= time_start & time <= time_end)
    
    #computing for the longest stretch of time that there is continuous data
    #na.contiguous
    
    cor_2in = ccf(x_filter$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values,
                  y_filter$spei,lag.max	= 0, na.action = na.contiguous, plot = FALSE)
    
    cor_8in = ccf(x_filter$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values,
                  y_filter$spei,lag.max	= 0, na.action = na.contiguous, plot = FALSE)
    
    cor_20in = ccf(x_filter$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values,
                   y_filter$spei,lag.max	= 0, na.action = na.contiguous, plot = FALSE) 
    
    if(i == 1){
      #nrows for depth, ncols for time scales
      correlation_matrix = data.frame()
      correlation_matrix[1:3,i] = c(cor_2in$acf, cor_8in$acf, cor_20in$acf)
      names(correlation_matrix)[i] = paste0("spei_",as.numeric(time_scale),"_day")
      rownames(correlation_matrix) = c("sm_2in", "sm_8in","sm_20in")
    }
    else{
      correlation_matrix[1:3,i] = c(cor_2in$acf, cor_8in$acf, cor_20in$acf)
      names(correlation_matrix)[i] = paste0("spei_",as.numeric(time_scale),"_day")
    }
  }
  return(correlation_matrix)
}

site = 1

correlation_matrix = cross_cor(snotel_soil_moisture[[site]], snotel_spei[[site]])

plot(c(15,30,45,60,90,180,365), as.numeric(correlation_matrix[1,]), type = "l", ylim = c(min(correlation_matrix),max(correlation_matrix)), xlab = "Time scale (Days)", ylab = "Corelation")
lines(c(15,30,45,60,90,180,365), as.numeric(correlation_matrix[2,]), type = "l", col = "red")
lines(c(15,30,45,60,90,180,365), as.numeric(correlation_matrix[3,]), type = "l", col = "blue")
legend(300, max(correlation_matrix), legend=c("2 in", "8 in", "20 in"),
       col=c("black", "red", "blue"), lty=c(1,1,1), cex=0.8)


par(mar = c(5, 4, 4, 4) + 0.3)
plot(as.POSIXct(snotel_soil_moisture[[site]]$Date), snotel_soil_moisture[[site]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values, 
     xlim = c(as.POSIXct("2007-01-01"), as.POSIXct("2008-01-01")), type = "l", xlab = "time", ylab = "Soil Moisture")

par(new = TRUE)

plot(snotel_spei[[site]][[2]]$time, snotel_spei[[site]][[2]]$spei, 
     xlim = c(as.POSIXct("2007-01-01"), as.POSIXct("2008-01-01")), type = "l", col = "blue", axes = FALSE, bty = "n", xlab = "", ylab = "")

axis(side=4, at = pretty(range(snotel_spei[[1]][[2]]$spei)), col = "blue")
mtext("SPEI", side=4, line=3, col = "blue")
