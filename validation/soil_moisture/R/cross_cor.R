cross_cor = function(spei,soil_moisture){ 
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
        dplyr::select(time, spei, depth)%>%
        mutate(mean_soil_moisture = rowMeans(.[depth], na.rm = T))%>%
        as_tibble()
      
      depth = c(depth, "mean_soil_moisture")
      
      if(t == 1){
        correlation_matrix = data.frame(matrix(nrow = length(depth),
                                               ncol = x_size))
        rownames(correlation_matrix) = depth
        colnames(correlation_matrix) = paste0("spei_",c(seq(5,730,5)))
      }
      
      for(i in 1: length(depth)){
        x_select = merged %>%
          #select the collums I want, depth
          dplyr::select(time, spei, depth[i]) %>%
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
        if(length(x_select$time) > 365){
          correlation = cor.test(dplyr::pull(x_select['spei']), dplyr::pull(x_select['standardized']), na.rm = T)
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
