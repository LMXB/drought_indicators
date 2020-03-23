cor_sm = function(drought_index,soil_moisture, plots){ 
  tryCatch({
    index_timescale_select = drought_index
    
    # define time format
    index_timescale_select$time = round(as.POSIXct(index_timescale_select$time), unit = "days")
    soil_moisture$Date = round(as.POSIXct(soil_moisture$Date, format = "%Y-%m-%d"), unit = "days")
    
    #filter negatives
    index_timescale_select$time = as.POSIXct(index_timescale_select$time)
    #compute standardized modeled sm metrics
    index_timescale_select = index_timescale_select %>%
      dplyr::filter(sm > 0)%>%
      mutate(sm_percentile = ntile(sm, 100),
             sm_z_score = ((sm - mean(sm))/ sd(sm)),
             sm_gamma = gamma_standard_fun(sm))
    
    #redefine time formatfor common time steps below
    index_timescale_select$time = round(as.POSIXct(index_timescale_select$time), unit = "days")
    
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
    drought_metric_name = colnames(merged)[2:5]
    depth = colnames(merged)[-c(1:5)]
    
    merged = merged %>%
      dplyr::select(time, drought_metric_name, depth)%>%
      mutate(mean_soil_moisture = rowMeans(.[depth], na.rm = T))%>%
      as_tibble() %>%
      mutate(month = lubridate::month(time)) %>%
      dplyr::select(time, drought_metric_name, depth, mean_soil_moisture)
    
    depth = c(depth, "mean_soil_moisture")
    
    
    #build corelation matrix (nrow = soil moisture depths, )
    correlation_matrix = data.frame(matrix(nrow = length(depth),
                                           ncol = 4))
    rownames(correlation_matrix) = depth
    colnames(correlation_matrix) = c("sm_raw", "sm_percentile", "sm_z_score", "sm_gamma")
    
    
    for(i in 1: length(depth)){
      x_select <- merged %>%
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
      
      if(length(x_select$time) == 0){
        correlation_matrix[i,] = NA
      }
      else{
        #check to see if at least a year of data?
        for(s in 1:length(drought_metric_name)){
          if(length(x_select$time) > 365){
            correlation = cor.test(dplyr::pull(x_select[drought_metric_name[s]]), dplyr::pull(x_select['standardized']), na.rm = T)
            
            #check to see if correlation is significant
            if(correlation$p.value < 0.05){
              correlation_matrix[i,s] = correlation$estimate
            }
            
            # not significant? NA
            else{
              correlation_matrix[i,s] = NA
            }
          }
          
          # not a year worth of data? NA
          else{
            correlation_matrix[i,s] = NA
          }
        }
      }
    }
    
    if(plots == T){
      print(plot(x_select$sm_gamma, x_select$standardized, xlab = "Predicted Gamma Standard",
                 ylab = "Observed VWC", 
                 main = paste0('r = ', round(correlation_matrix$sm_gamma[nrow(correlation_matrix)],3), "\n n = ", nrow(x_select))))
      
    }
    
    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}



cor_sm_summer = function(drought_index,soil_moisture, plots){ 
  tryCatch({
      index_timescale_select = drought_index
      
      # define time format
      index_timescale_select$time = round(as.POSIXct(index_timescale_select$time), unit = "days")
      soil_moisture$Date = round(as.POSIXct(soil_moisture$Date, format = "%Y-%m-%d"), unit = "days")
      
      #filter negatives
      index_timescale_select$time = as.POSIXct(index_timescale_select$time)
      #compute standardized modeled sm metrics
      index_timescale_select = index_timescale_select %>%
        dplyr::filter(sm > 0)%>%
        mutate(sm_percentile = ntile(sm, 100),
               sm_z_score = ((sm - mean(sm))/ sd(sm)),
               sm_gamma = gamma_standard_fun(sm))
      
      #redefine time formatfor common time steps below
      index_timescale_select$time = round(as.POSIXct(index_timescale_select$time), unit = "days")
      
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
      drought_metric_name = colnames(merged)[2:5]
      depth = colnames(merged)[-c(1:5)]
      
      merged = merged %>%
        dplyr::select(time, drought_metric_name, depth)%>%
        mutate(mean_soil_moisture = rowMeans(.[depth], na.rm = T))%>%
        as_tibble() %>%
        mutate(month = lubridate::month(time)) %>%
        dplyr::filter(month > 4 & month < 11) %>%
        dplyr::select(time, drought_metric_name, depth, mean_soil_moisture)
      
      depth = c(depth, "mean_soil_moisture")
      
      
      #build corelation matrix (nrow = soil moisture depths, )
      correlation_matrix = data.frame(matrix(nrow = length(depth),
                                             ncol = 4))
      rownames(correlation_matrix) = depth
      colnames(correlation_matrix) = c("sm_raw", "sm_percentile", "sm_z_score", "sm_gamma")
      
      
      for(i in 1: length(depth)){
        x_select <- merged %>%
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
        
        if(length(x_select$time) == 0){
          correlation_matrix[i,] = NA
        }
        else{
          #check to see if at least a year of data?
          for(s in 1:length(drought_metric_name)){
            if(length(x_select$time) > 180){
              correlation = cor.test(dplyr::pull(x_select[drought_metric_name[s]]), dplyr::pull(x_select['standardized']), na.rm = T)
              
              #check to see if correlation is significant
              if(correlation$p.value < 0.05){
                correlation_matrix[i,s] = correlation$estimate
              }
              
              # not significant? NA
              else{
                correlation_matrix[i,s] = NA
              }
            }
            
            # not a year worth of data? NA
            else{
              correlation_matrix[i,s] = NA
            }
          }
        }
      }
      
    if(plots == T){
      print(plot(x_select$sm_gamma, x_select$standardized, xlab = "Predicted Gamma Standard",
                 ylab = "Observed VWC", 
                 main = paste0('r = ', round(correlation_matrix$sm_gamma[nrow(correlation_matrix)],3), "\n n = ", nrow(x_select))))
      
    }

    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}

