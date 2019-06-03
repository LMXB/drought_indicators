moving_cross_cor = function(spei,soil_moisture){ 
  tryCatch({
    x_size = length(spei)
    for(t in 1:length(spei)){
      x1 = spei[[t]]
      
      x1$time = as.POSIXct(x1$time)
      soil_moisture$Date = as.POSIXct(soil_moisture$Date)
      
      x_filter <- x1[x1$time %in% soil_moisture$Date,]
      y_filter <- soil_moisture[soil_moisture$Date %in% x1$time,]
      
      merged = cbind(x_filter, y_filter$Soil.Moisture.Percent..2in..pct..Start.of.Day.Values,
                     y_filter$Soil.Moisture.Percent..8in..pct..Start.of.Day.Values,
                     y_filter$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values,
                     y_filter$Snow.Water.Equivalent..in..Start.of.Day.Values)
      
      depth = c("soil_moisture_2in", "soil_moisture_8in", "soil_moisture_20in")
      
      colnames(merged)[3:6] = c(depth,"swe")
      
      if(t == 1){
        correlation_matrix = list()
      }
      
      for(i in 1: length(depth)){
        x_select = merged %>%
          #select the collums I want, depth
          select(time, spei, depth[i]) %>%
          #filter negative soil moisture data
          dplyr::filter(get(depth[i]) > 0) %>%
          #filter for complete cases
          dplyr::filter(complete.cases(.))%>%
          #compute standardized value
          mutate(standardized = gamma_standard_fun(get(depth[i])))%>%
          #add yday
          mutate(month = lubridate::month(time))
        
        if(i == 1){
          correlation = x_select %>%
          dplyr::group_by(month) %>%
          dplyr::summarize(correlation = cor(spei,standardized))
          
          correlation_full = correlation
        }
        else{
          correlation = x_select %>%
            dplyr::group_by(month) %>%
            dplyr::summarize(correlation = cor(spei,standardized))
          
          correlation_full = cbind(correlation_full, correlation$correlation)
        }
      }
    }
    return(correlation_full)
  }, error = function(e){
    return(NA)
  })
}