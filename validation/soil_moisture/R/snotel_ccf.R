library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(sf)
library(tictoc)
library(timeSeries)
library(stringr)

load("/home/zhoylman/drought_indicators_data/snotel_spei/snotel_spei.RData")
load("/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/snotel_soil_moisture.RData")

snotel = st_read("/home/zhoylman/drought_indicators/snotel/shp/Snotel_Sites.shp")
states = st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp")
snotel$site_num = gsub("[^0-9.]","",as.character(snotel$site_name))

source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/gamma_standard_fun.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/cross_cor.R")
source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/moving_cross_cor.R")


site = 1
spei = snotel_spei[[site]]
soil_moisture = snotel_soil_moisture[[site]]
cross_cor(spei,soil_moisture)

##################################################################################
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)
clusterExport(cl, "gamma_fit")
clusterExport(cl, "gamma_standard_fun")

tic()
correlation_matrix = foreach(site = 1:length(snotel_soil_moisture)) %dopar% {
  #source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")
  #source("/home/zhoylman/drought_indicators/validation/soil_moisture/R/gamma_standard_fun.R")
  library(dplyr)
  cross_cor(snotel_spei[[site]], snotel_soil_moisture[[site]])
}
toc()
##################################################################################


foreach(i = 1:length(snotel$lat)) %dopar% {
  tryCatch({
    png(filename = paste0("/home/zhoylman/drought_indicators/validation/soil_moisture/plots/correlation/Soil_Moisture_SPEI_Correlation_",
                          i,".png"), width = 6, height = 5, units = "in", res = 300)
    plot(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][1,]), type = "l",
         ylim = c(min(correlation_matrix[[i]]),max(correlation_matrix[[i]])),
         xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][2,]), type = "l", col = "red")
    lines(c(seq(15,360,15)), as.numeric(correlation_matrix[[i]][3,]), type = "l", col = "blue")
    legend(300, max(correlation_matrix[[i]]), legend=c("2 in", "8 in", "20 in"),
           col=c("black", "red", "blue"), lty=c(1,1,1), cex=0.8)
    dev.off()
  }, error = function(e){
    plot(1,1, xlab = "Time scale (Days)", ylab = "Correlation", main = paste0(snotel$site_name[i], " Soil Moisture ~ SPEI Correlation"))
    text(1, 1.2, "Sorry, no data to correlate")
    dev.off()
  })
}

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


#extract density ploot information
density_2in = data.frame(x = density(best_times_matrix$`2in`, na.rm = T)[[1]],
                         y = density(best_times_matrix$`2in`, na.rm = T)[[2]])
density_8in = data.frame(x = density(best_times_matrix$`8in`, na.rm = T)[[1]],
                         y = density(best_times_matrix$`8in`, na.rm = T)[[2]])
density_20in = data.frame(x = density(best_times_matrix$`20in`, na.rm = T)[[1]],
                          y = density(best_times_matrix$`20in`, na.rm = T)[[2]])

library(ggplot2)
summary_plot = ggplot() + 
  geom_line(data = density_2in, aes(x = x, y = y, color = "2in"))+
  geom_point(data = density_2in[which(density_2in$y == max(density_2in$y)),], aes(x = x, y = y))+
  geom_text(data = density_2in[which(density_2in$y == max(density_2in$y)),], aes(x = x + 40, y = y, label = paste0(round(x, digits = 0), " Days")))+
  
  geom_line(data = density_8in, aes(x = x, y = y, color = "8in"))+
  geom_point(data = density_8in[which(density_8in$y == max(density_8in$y)),], aes(x = x, y = y))+
  geom_text(data = density_8in[which(density_8in$y == max(density_8in$y)),], aes(x = x+ 40, y = y, label = paste0(round(x, digits = 0), " Days")))+
  
  geom_line(data = density_20in, aes(x = x, y = y, color = "20in"))+
  geom_point(data = density_20in[which(density_20in$y == max(density_20in$y)),], aes(x = x, y = y))+
  geom_text(data = density_20in[which(density_20in$y == max(density_20in$y)),], aes(x = x+ 40, y = y, label = paste0(round(x, digits = 0), " Days")))+
  
  scale_color_manual("Depth", values = c("2in" = "black", 
                                "8in" = "red", 
                                "20in" = "blue"),
                     breaks=c("2in","8in","20in"))+
  theme_bw(base_size = 16) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.9, 0.8),
        plot.title = element_text(hjust = 0.5))+
  ylab("Density")+
  xlab("Timescale (Days)")+
  ggtitle("Best Correlation Times (Soil Moisture ~ SPEI)")

png(filename = "/home/zhoylman/drought_indicators/validation/soil_moisture/plots/summary/time_scale_summary.png", width = 7, height = 5, units = "in", res = 300)
summary_plot
dev.off()

write.csv(best_times_matrix, "/home/zhoylman/drought_indicators/validation/soil_moisture/snotel_data/correlation_times.csv", row.names = F)

#plotting function for timeseries
time_series_plot = function(site_number, depth){
  site_id = which(snotel$site_num == site_number)
  
  if(depth == 2){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,1]
  }
  
  if(depth == 8){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,2]
  }
  
  if(depth == 20){
    soil_moisture = data.frame(time = as.Date(snotel_soil_moisture[[site_id]]$Date),
                               soil_moisture = snotel_soil_moisture[[site_id]]$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values) 
    #find best spei
    best_spei_time = best_times_matrix[site_id,3]
  }

    #index
    best_spei = which(best_spei_time == c(seq(15,360,15)))
    #extract
    spei = snotel_spei[[site]][[best_spei]]
    #as Date
    spei$time = as.Date(spei$time)
    #filter for comon time
    spei_filter <- spei[spei$time %in% soil_moisture$time,]
    soil_moisture_filter <- soil_moisture[soil_moisture$time %in% spei$time,]
    
    data = cbind(soil_moisture_filter, spei = spei_filter$spei) %>%
      #filter negative soil moisture data
      dplyr::filter(soil_moisture > 0) %>%
      #filter complete cases
      dplyr::filter(complete.cases(.))%>%
      #compute standardized value
      mutate(standardized = gamma_standard_fun(soil_moisture))
  
  
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot(data$time, data$standardized, type = "l", ylim = c(min(data$standardized),max(data$standardized)), xlab = "Time", ylab = "Standardized Soil Moisture",
       xlim = c(data$time[1], data$time[length(data$time)]), main = paste0("Depth = ",depth , "in\nBest timescale = ", best_spei_time, " days")) # first plot
  
  par(new = TRUE)
  
  plot(data$time, data$spei , type = "l", 
       axes = FALSE, bty = "n", xlab = "", ylab = "",  xlim = c(data$time[1], data$time[length(data$time)]), col = "red")
  abline(0, 0,col = "red", lty = 2)
  axis(side=4, at = pretty(range(data$spei)), col = "red")
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
  
