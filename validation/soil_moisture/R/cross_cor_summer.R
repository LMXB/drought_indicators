cross_cor_summer = function(drought_index,soil_moisture){ 
  tryCatch({
    x_size = length(drought_index)
    for(t in 1:length(drought_index)){
      index_timescale_select = drought_index[[t]]
      
      # define time format
      index_timescale_select$time = round(as.POSIXct(index_timescale_select$time), unit = "days")
      soil_moisture$Date = round(as.POSIXct(soil_moisture$Date, format = "%Y-%m-%d"), unit = "days")
      
      # filter datasets for consitant obs 
      x_filter <- index_timescale_select[index_timescale_select$time %in% soil_moisture$Date,]
      y_filter <- soil_moisture[soil_moisture$Date %in% index_timescale_select$time,]
      
      #rename date time collumn
      colnames(y_filter) = c("time", names(y_filter)[-1])
      
      #redefine the time format (looses this data after filtering step)
      x_filter$time = as.POSIXct(x_filter$time)
      y_filter$time = as.POSIXct(y_filter$time)
      
      #merge based on time
      merged = dplyr::full_join(x_filter, y_filter, by = "time")
      
      #extract names for indexing (drought index names and depths change across networks/indicies)
      drought_metric_name = colnames(merged)[2]
      depth = colnames(merged)[-c(1:2)]
      
      merged = merged %>%
        dplyr::select(time, drought_metric_name, depth)%>%
        mutate(mean_soil_moisture = rowMeans(.[depth], na.rm = T))%>%
        as_tibble() %>%
        mutate(month = lubridate::month(time)) %>%
        dplyr::filter(month > 4 & month < 11) %>%
        dplyr::select(time, drought_metric_name, depth, mean_soil_moisture)
      
      depth = c(depth, "mean_soil_moisture")
      
      if(t == 1){
        correlation_matrix = data.frame(matrix(nrow = length(depth),
                                               ncol = x_size))
        rownames(correlation_matrix) = depth
        colnames(correlation_matrix) = paste0(drought_metric_name,"_",c(seq(5,730,5)))
      }
      
      for(i in 1: length(depth)){
        x_select = merged %>%
          #select the collums I want, depth
          dplyr::select(time, drought_metric_name, depth[i]) %>%
          #filter negative soil moisture data
          dplyr::filter(get(depth[i]) > 0) %>%
          #filter bad soil moisture data
          dplyr::filter(get(depth[i]) < 100) %>%
          #filter Inf and -Inf
          filter_all(all_vars(is.finite(.))) %>%
          #filter for complete cases
          tidyr::drop_na()%>%
          #compute standardized value
          mutate(standardized = gamma_standard_fun(get(depth[i])))
        
        #check to see if at least a year of data?
        if(length(x_select$time) > 180){
          correlation = cor.test(dplyr::pull(x_select[drought_metric_name]), dplyr::pull(x_select['standardized']), na.rm = T)
          #check to see if correlation is significant
          if(correlation$p.value < 0.05){
            correlation_matrix[i,t] = correlation$estimate
          }
          # not significant? NA
          else{
            correlation_matrix[i,t] = NA
          }
        }
        # not a year worth of data? NA
        else{
          correlation_matrix[i,t] = NA
        }
      }
    }
    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}
