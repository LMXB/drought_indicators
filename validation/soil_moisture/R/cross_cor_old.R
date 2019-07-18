cross_cor_old = function(spei,soil_moisture){ 
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
                     y_filter$Soil.Moisture.Percent..20in..pct..Start.of.Day.Values) 
      
      depth = c("soil_moisture_2in", "soil_moisture_8in", "soil_moisture_20in")
      
      colnames(merged)[3:5] = c(depth)
      
      merged = merged %>%
        rowwise() %>%
        mutate(mean_soil_moisture = mean(c(soil_moisture_2in,soil_moisture_8in,soil_moisture_20in), na.rm = T))
      
      depth = c("soil_moisture_2in", "soil_moisture_8in", "soil_moisture_20in", "mean_soil_moisture")
      
      if(t == 1){
        correlation_matrix = data.frame(matrix(nrow = length(depth),
                                               ncol = x_size))
        rownames(correlation_matrix) = depth
        colnames(correlation_matrix) = paste0("spei_",c(seq(15,360,15)))
      }
      
      for(i in 1: length(depth)){
        x_select = merged %>%
          #select the collums I want, depth
          select(time, spei, depth[i]) %>%
          #filter negative soil moisture data
          dplyr::filter(get(depth[i]) > 0) %>%
          #filter for complete cases
          tidyr::drop_na()%>%
          #compute standardized value
          mutate(standardized = gamma_standard_fun(get(depth[i])))
        
        correlation_matrix[i,t] = cor(x_select['spei'], x_select['standardized'])
      }
    }
    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}