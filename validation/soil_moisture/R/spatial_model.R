library(rgdal)
library(sf)
library(raster)
library(dplyr)
library(spdplyr)

states = sf::st_read("/home/zhoylman/drought_indicators/shp_kml/states.shp") %>%
  dplyr::filter(STATE_NAME != "Alaska" & STATE_NAME != "Hawaii")

data_geospatial = read.csv("./validation/soil_moisture/summary_data/geospatial_with_best.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

st_write(data_geospatial, "./validation/soil_moisture/summary_data/geospatial_with_best.shp")

plot(states$geometry)
plot(data_geospatial$geometry, add = T)

data = read.csv("./validation/soil_moisture/summary_data/geospatial_with_best.csv")%>%
  arrange(station_key)

predictors = read.csv("/home/zhoylman/drought_indicators/validation/soil_moisture/summary_data/validation_extract.csv")%>%
  arrange(sttn_)
predictors$system.index = NULL
predictors$.geo = NULL

master = cbind(data, predictors)

library(MASS)
#gamma distrobution with heavy right skew
fit_model_times = function(y){
  #fit model with stepwise AIC
  model = glm(get(y) ~ TPI + TWI + aspect + bulk_density + clay + elevation + 
                 organicC + sand + slope + swhc, data = master, family = Gamma(link = "log")) %>%
    stepAIC(trace = FALSE)
  
  #predict values for plotting
  predictions = model %>% predict(master, type = "response")
  
  #plot and print model diagnostics
  plot(predictions, master[y][,1], ylab = y)
  abline(lm(master[y][,1] ~ predictions), col = 'red')
  abline(0,1)
  print(summary(model)$coefficients)
  print(paste0("r2 (lm) = ", round(summary(lm(master[y][,1] ~ predictions))$r.squared, 3)))
}

vars = c('mean_time_spi','mean_time_spei','mean_time_eddi','mean_time_sedi')

for(i in 1:4){
  print(vars[i])
  fit_model_times(vars[i])
  print("")}

# normal distrobution for correlations
fit_model_cor = function(y){
  #fit model with stepwise AIC
  model = lm(get(y) ~ TPI + TWI + aspect + bulk_density + clay + elevation + 
                organicC + sand + slope + swhc, data = master) %>%
    stepAIC(trace = FALSE)
  
  #predict values for plotting
  predictions = model %>% predict(master, type = "response")
  
  #plot and print model diagnostics
  plot(predictions, master[y][,1], ylab = y)
  abline(0,1)
  print(summary(model)$coefficients)
  print(paste0("r2 = ",round(summary(model)$r.squared, 3)))
}

fit_model_cor('mean_cor_spi')

vars = c('mean_cor_spi','mean_cor_spei','mean_cor_eddi','mean_cor_sedi')

for(i in 1:4){
  print(vars[i])
  fit_model_cor(vars[i])
  print("")}

write.csv(master, "/home/zhoylman/temp/master.csv")
