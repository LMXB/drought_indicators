library(ggplot2)

load("~/drought_indicators_data/preprocessed_soil_moisture/mesonet_soil_moisture_list.Rdata")


data = data.frame(x = mesonet_soil_moisture_list[[28]]$Date,
                  y = mesonet_soil_moisture_list[[28]]$soilwc00)

