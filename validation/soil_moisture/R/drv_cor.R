#define the not in opperator. Super useful 
'%notin%' = Negate('%in%')

#function to run correlations for wetting and drying time periods independently
drv_cor = function(drought_index,soil_moisture){ 
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
        as_tibble()
      
      depth = c(depth, "mean_soil_moisture")
      
      if(t == 1){
        correlation_matrix = list()
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
          mutate(standardized = gamma_standard_fun(get(depth[i])))%>%
          #add derivative vector
          mutate(drv = c(NA, diff(standardized)))%>%
          #catigorize into positive (wetting) or negative (drying)
          mutate(drv_cond = as.factor(ifelse(drv > 0, "Wetting", "Drying"))) %>%
          #filter for complete cases (remove first time period )
          tidyr::drop_na()
          
        #plots for trouble shooting
        #plot(x_select$time, x_select$standardized, col = x_select$drv_cond)
        #lines(x_select$time, x_select$standardized)
        
        #plot(x_select$standardized, x_select$spi, col = x_select$drv_cond)
        
        if(i == 1){
          correlation = x_select %>%
            #group by month
            dplyr::group_by(drv_cond) %>%
            # run correlation by month and calcualte number of obs in each month 
            dplyr::summarize(correlation = cor(get(drought_metric_name), get(depth[i])),
                             length = length(get(depth[i])))
          # remove data that is derived from less than a year of data
          if(sum(correlation$length) < 365){
            correlation$correlation = NA
          }
          
          # select data we want
          correlation = correlation %>%
            select(drv_cond, correlation)
          
          #fill in dummy filler data if there isnt any data in the first probe
          # otherwise there will be no wetting/drying factor varibale names
          if(length(correlation$drv_cond)==1){
            correlation = data.frame(drv_cond = c("Drying", "Wetting"),
                                     correlation = c(NA,NA)) %>%
              as_tibble()
          }
          
          # first i, start the bigger dataframe
          correlation_full = correlation
          
        }
        else{
          correlation = x_select %>%
            #group by month
            dplyr::group_by(drv_cond) %>%
            # run correlation by month and calcualte number of obs in each month 
            dplyr::summarize(correlation = cor(get(drought_metric_name), get(depth[i])),
                             length = length(get(depth[i])))
          # remove data that is derived from less than a year of data
          if(sum(correlation$length) < 365){
            correlation$correlation = NA
          }
          #bind (this is the reason for the if else cascade above)
          correlation_full = cbind(correlation_full, correlation$correlation)
        }
        if(i == length(depth)){
          colnames(correlation_full) = c("drv_cond",depth)
          correlation_matrix[[t]] = correlation_full
        }
      }
    }
    return(correlation_matrix)
  }, error = function(e){
    return(NA)
  })
}