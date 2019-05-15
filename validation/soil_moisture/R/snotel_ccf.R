library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)

load("/home/zhoylman/drought_indicators_data/snotel_spei/snotel_spei.RData")
load("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")

snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

length_vec = data.frame()

for(i in 1:length(snotel_soil_moisture)){
  tryCatch({
    length_vec[i,1] = length(na.contiguous(snotel_soil_moisture[[i]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values))
    length_vec[i,2] = length(na.contiguous(snotel_soil_moisture[[i]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values))
    length_vec[i,3] = length(na.contiguous(snotel_soil_moisture[[i]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values))
  },
  error = function(e){
    return(c(NA,NA,NA))
  })
  print(i)
}

cross_cor = function(snotel_sm,snotel_spei){
  tryCatch({
    for(i in 1:length(snotel_spei)){
      x = snotel_sm
      y = snotel_spei[[i]]
      
      # length_test = na.contiguous(x)
      # if(length_test < 100){
      #   stop()
      # }
      
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
  },
  error = function(e){
    return(NA)
  })
}

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

tic()

correlation_matrix = foreach(site = 1:length(snotel$lat)) %dopar% {
  library(dplyr)
  cross_cor(snotel_soil_moisture[[site]], snotel_spei[[site]])
}
toc()
# 
# foreach(i = 1:length(snotel$lat)) %dopar% {
#   tryCatch({
#     png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/correlation/Soil_Moisture_SPEI_Correlation_", 
#                           i,".png"), width = 6, height = 5, units = "in", res = 300)
#     plot(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][1,]), type = "l", 
#          ylim = c(min(correlation_matrix[[i]]),max(correlation_matrix[[i]])), 
#          xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
#     lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][2,]), type = "l", col = "red")
#     lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][3,]), type = "l", col = "blue")
#     legend(300, max(correlation_matrix[[i]]), legend=c("2 in", "8 in", "20 in"),
#            col=c("black", "red", "blue"), lty=c(1,1,1), cex=0.8)
#     dev.off()
#   }, error = function(e){
#     plot(1,1, xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
#     text(1, 1.2, "Sorry, no data to correlate")
#     dev.off()
#   })
# }

stopCluster(cl)
find_best = function(x){
  times = c(seq(15,360,15))
  best_2in = times[which(x[1,]==max(x[1,]))]
  best_8in = times[which(x[2,]==max(x[2,]))]
  best_20in = times[which(x[3,]==max(x[3,]))]
  best_times = c(best_2in, best_8in, best_20in)
  return(best_times)
}

best_times_matrix = data.frame(matrix(nrow = length(snotel$site_num), ncol = 3))
colnames(best_times_matrix) = c("2in", "8in", "20in")

for(i in 1:length(snotel$lat)){
  tryCatch({
    best_times_matrix[i,] = find_best(correlation_matrix[[i]])
  },
  error = function(e){
    return(c(NA,NA,NA))
  })
}

write.csv(best_times_matrix, "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/correlation_times.csv", row.names = F)

which(snotel$site_num == 460)

#plotting function for timeseries
time_series_plot = function(site_number, depth){
  site_id = which(snotel$site_num == site_number)
  
  if(depth == 2){
    data = data.frame(x = as.Date(snotel_soil_moisture[[site_id]]$Date),
                      y = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values)
    best_spei_time = best_times_matrix[site_id,1]
  }
  if(depth == 8){
    data = data.frame(x = as.Date(snotel_soil_moisture[[site_id]]$Date),
                      y = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values)
    best_spei_time = best_times_matrix[site_id,2]
  }
  if(depth == 20){
    data = data.frame(x = as.Date(snotel_soil_moisture[[site_id]]$Date),
                      y = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values)
    best_spei_time = best_times_matrix[site_id,3]
  }
  
  best_spei = which(best_spei_time == c(seq(15,360,15)))
  
  #find data that was used for corelation
  data = timeSeries::na.contiguous(data)
  data = as.data.frame(data)
  data$x = as.Date(data$x)
  
  #filter SPEI data
  data2 = as.data.frame(snotel_spei[[site_id]][[best_spei]])
  data2$time = as.Date(data2$time)
  data2 = data2 %>%
    dplyr::filter(time >= data$x[1] & time <= data$x[length(data$x)])
  
  
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot(data$x, data$y, type = "l", ylim = c(min(data$y),max(data$y)), xlab = "Time", ylab = "Soil Moisture",
       xlim = c(data$x[1], data$x[length(data$x)]), main = paste0("Depth = ",depth , "in\nBest timescale = ", best_spei_time, " days")) # first plot
  
  par(new = TRUE)
  
  plot(as.Date(data2$time), data2$spei, type = "l", 
       axes = FALSE, bty = "n", xlab = "", ylab = "",xlim = c(data$x[1], data$x[length(data$x)]), col = "red")
  abline(0, 0,col = "red", lty = 2)
  axis(side=4, at = pretty(range(data2$spei)), col = "red")
  mtext("SPEI", side=4, line=3, col = "red")
}

for(i in 1:length(snotel$site_num)){
  tryCatch({
    png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/time_series/Soil_Moisture_SPEI_timeseries_", 
                          i,".png"), width = 10, height = 3, units = "in", res = 300)
    par(mfrow = c(1,3))
    time_series_plot(snotel$site_num[i], 2)
    time_series_plot(snotel$site_num[i], 8)
    time_series_plot(snotel$site_num[i], 20)
    dev.off()
  }, error = function(e){
    plot(1,1, xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    text(1, 1.2, "Sorry, no data to correlate")
    dev.off()
  })
  print(i)
}
  
