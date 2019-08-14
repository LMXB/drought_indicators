moving_cross_cor = function(spei,soil_moisture){ 
  tryCatch({
    x_size = length(spei)
    for(t in 1:length(spei)){
      x1 = spei[[t]]
      
      # define time format
      x1$time = round(as.POSIXct(x1$time), unit = "days")
      soil_moisture$Date = round(as.POSIXct(soil_moisture$Date, format = "%Y-%m-%d"), unit = "days")
      
      # filter datasets for consitant obs 
      x_filter <- x1[x1$time %in% soil_moisture$Date,]
      y_filter <- soil_moisture[soil_moisture$Date %in% x1$time,]
      
      #rename date time collumn
      colnames(y_filter) = c("time", names(y_filter)[-1])
      
      #redefine the time format (looses this data after filtering step)
      x_filter$time = as.POSIXct(x_filter$time)
      y_filter$time = as.POSIXct(y_filter$time)
      
      #merge based 
      merged = dplyr::full_join(x_filter, y_filter, by = "time")
      
      depth = colnames(merged)[-c(1:2)]
      
      merged = merged %>%
        select(time, spei, depth)%>%
        mutate(mean_soil_moisture = rowMeans(.[depth], na.rm = T))%>%
        as_tibble()
      
      depth = c(depth, "mean_soil_moisture")
      
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
          tidyr::drop_na()%>%
          #compute standardized value
          mutate(standardized = gamma_standard_fun(get(depth[i])))%>%
          #add month id
          mutate(month = lubridate::month(time))
        
        if(i == 1){
          correlation = x_select %>%
          dplyr::group_by(month) %>%
          dplyr::summarize(correlation = cor(spei,standardized))
          
          correlation_full = correlation
          if(length(correlation_full$month) == 0){
            correlation_full = data.frame(month = 1:12,
                                          correlation = rep(NA,12))%>%
              as_tibble()
          }
        }
        else{
          correlation = x_select %>%
            dplyr::group_by(month) %>%
            dplyr::summarize(correlation = cor(spei,standardized))
          
          if(length(correlation$month) == 0){
            correlation = data.frame(month = 1:12,
                                     correlation = rep(NA,12))%>%
              as_tibble()
          }
          
          correlation_full = cbind(correlation_full, correlation$correlation)
        }
        if(i == length(depth)){
          colnames(correlation_full) = c("month",depth)
          correlation_matrix[[t]] = correlation_full
        }
      }
    }
    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}