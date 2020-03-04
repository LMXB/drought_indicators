find_best = function(x){
  times = c(seq(5,730,5))
  best_2in = times[which(x[1,]==max(x[1,], na.rm = T))]
  best_4in = times[which(x[2,]==max(x[2,], na.rm = T))]
  best_8in = times[which(x[3,]==max(x[3,], na.rm = T))]
  best_20in = times[which(x[4,]==max(x[4,], na.rm = T))]
  best_40in = times[which(x[5,]==max(x[5,], na.rm = T))]
  best_mean = times[which(x[6,]==max(x[6,], na.rm = T))]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_neg = function(x){
  times = c(seq(5,730,5))
  best_2in = times[which(x[1,]==min(x[1,], na.rm = T))]
  best_4in = times[which(x[2,]==min(x[2,], na.rm = T))]
  best_8in = times[which(x[3,]==min(x[3,], na.rm = T))]
  best_20in = times[which(x[4,]==min(x[4,], na.rm = T))]
  best_40in = times[which(x[5,]==min(x[5,], na.rm = T))]
  best_mean = times[which(x[6,]==min(x[6,], na.rm = T))]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_cor = function(x){
  times = c(seq(5,730,5))
  best_2in = x[which(x[1,]==max(x[1,], na.rm = T))][1,1]
  best_4in = x[which(x[2,]==max(x[2,], na.rm = T))][2,1]
  best_8in = x[which(x[3,]==max(x[3,], na.rm = T))][3,1]
  best_20in = x[which(x[4,]==max(x[4,], na.rm = T))][4,1]
  best_40in = x[which(x[5,]==max(x[5,], na.rm = T))][5,1]
  best_mean = x[which(x[6,]==max(x[6,], na.rm = T))][6,1]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_cor_neg = function(x){
  times = c(seq(5,730,5))
  best_2in = x[which(x[1,]==min(x[1,], na.rm = T))][1,1]
  best_4in = x[which(x[2,]==min(x[2,], na.rm = T))][2,1]
  best_8in = x[which(x[3,]==min(x[3,], na.rm = T))][3,1]
  best_20in = x[which(x[4,]==min(x[4,], na.rm = T))][4,1]
  best_40in = x[which(x[5,]==min(x[5,], na.rm = T))][5,1]
  best_mean = x[which(x[6,]==min(x[6,], na.rm = T))][6,1]
  
  if(length(best_2in)==0){best_2in = NA}
  if(length(best_4in)==0){best_4in = NA}
  if(length(best_8in)==0){best_8in = NA}
  if(length(best_20in)==0){best_20in = NA}
  if(length(best_40in)==0){best_40in = NA}
  if(length(best_mean)==0){best_mean = NA}
  
  best_times = c(best_2in, best_4in, best_8in, best_20in, best_40in, best_mean)
  return(best_times)
}

find_best_mesonet = function(x){
  tryCatch({
    times = c(seq(5,730,5))
    best_0in = times[which(x[1,]==max(x[1,], na.rm = T))]
    best_4in = times[which(x[2,]==max(x[2,], na.rm = T))]
    best_8in = times[which(x[3,]==max(x[3,], na.rm = T))]
    best_20in = times[which(x[4,]==max(x[4,], na.rm = T))]
    best_36in = times[which(x[5,]==max(x[5,], na.rm = T))]
    best_mean = times[which(x[6,]==max(x[6,], na.rm = T))]
    
    if(length(best_0in)==0){best_0in = NA}
    if(length(best_4in)==0){best_4in = NA}
    if(length(best_8in)==0){best_8in = NA}
    if(length(best_20in)==0){best_20in = NA}
    if(length(best_36in)==0){best_36in = NA}
    if(length(best_mean)==0){best_mean = NA}
    
    best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
    return(best_times)
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
  
}

find_best_mesonet_neg = function(x){
  tryCatch({
    times = c(seq(5,730,5))
    best_0in = times[which(x[1,]==min(x[1,], na.rm = T))]
    best_4in = times[which(x[2,]==min(x[2,], na.rm = T))]
    best_8in = times[which(x[3,]==min(x[3,], na.rm = T))]
    best_20in = times[which(x[4,]==min(x[4,], na.rm = T))]
    best_36in = times[which(x[5,]==min(x[5,], na.rm = T))]
    best_mean = times[which(x[6,]==min(x[6,], na.rm = T))]
    
    if(length(best_0in)==0){best_0in = NA}
    if(length(best_4in)==0){best_4in = NA}
    if(length(best_8in)==0){best_8in = NA}
    if(length(best_20in)==0){best_20in = NA}
    if(length(best_36in)==0){best_36in = NA}
    if(length(best_mean)==0){best_mean = NA}
    
    best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
    return(best_times)
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
  
}

find_best_mesonet_cor = function(x){
  tryCatch({
    times = c(seq(5,730,5))
    best_0in = x[which(x[1,]==max(x[1,], na.rm = T))][1,1]
    best_4in = x[which(x[2,]==max(x[2,], na.rm = T))][2,1]
    best_8in = x[which(x[3,]==max(x[3,], na.rm = T))][3,1]
    best_20in = x[which(x[4,]==max(x[4,], na.rm = T))][4,1]
    best_36in = x[which(x[5,]==max(x[5,], na.rm = T))][5,1]
    best_mean = x[which(x[6,]==max(x[6,], na.rm = T))][6,1]
    
    if(length(best_0in)==0){best_0in = NA}
    if(length(best_4in)==0){best_4in = NA}
    if(length(best_8in)==0){best_8in = NA}
    if(length(best_20in)==0){best_20in = NA}
    if(length(best_36in)==0){best_36in = NA}
    if(length(best_mean)==0){best_mean = NA}
    
    best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
    return(best_times)
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
}

find_best_mesonet_cor_neg = function(x){
  tryCatch({
    times = c(seq(5,730,5))
    best_0in = x[which(x[1,]==min(x[1,], na.rm = T))][1,1]
    best_4in = x[which(x[2,]==min(x[2,], na.rm = T))][2,1]
    best_8in = x[which(x[3,]==min(x[3,], na.rm = T))][3,1]
    best_20in = x[which(x[4,]==min(x[4,], na.rm = T))][4,1]
    best_36in = x[which(x[5,]==min(x[5,], na.rm = T))][5,1]
    best_mean = x[which(x[6,]==min(x[6,], na.rm = T))][6,1]
    
    if(length(best_0in)==0){best_0in = NA}
    if(length(best_4in)==0){best_4in = NA}
    if(length(best_8in)==0){best_8in = NA}
    if(length(best_20in)==0){best_20in = NA}
    if(length(best_36in)==0){best_36in = NA}
    if(length(best_mean)==0){best_mean = NA}
    
    best_times = c(best_0in, best_4in, best_8in, best_20in, best_36in, best_mean)
    return(best_times)
  },
  error = function(e){
    return(c(NA,NA,NA,NA,NA,NA))
  })
  
}

extract_density = function(x, name){
  data = density(x, na.rm = T)
  data_extract = data.frame(x = data$x, y = data$y, name = name)
  return(data_extract)
}

extract_density_linetype = function(x, name, linetype){
  data = density(x, na.rm = T)
  data_extract = data.frame(x = data$x, y = data$y, name = name, linetype = linetype)
  return(data_extract)
}
mround <- function(x,base){
  base*round(x/base)
}

find_best_wet_dry = function(x){
  dry = lapply(x, dplyr::filter, drv_cond == "Drying") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  wet = lapply(x, dplyr::filter, drv_cond == "Wetting") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  best_dry = find_best(dry)
  best_wet = find_best(wet)
  
  return(c(best_wet,best_dry))
}

find_best_wet_dry_neg = function(x){
  dry = lapply(x, dplyr::filter, drv_cond == "Drying") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  wet = lapply(x, dplyr::filter, drv_cond == "Wetting") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  best_dry = find_best_neg(dry)
  best_wet = find_best_neg(wet)
  
  return(c(best_wet,best_dry))
}



find_best_wet_dry_mesonet = function(x){
  dry = lapply(x, dplyr::filter, drv_cond == "Drying") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  wet = lapply(x, dplyr::filter, drv_cond == "Wetting") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  best_dry = find_best_mesonet(dry)
  best_wet = find_best_mesonet(wet)
  
  return(c(best_wet,best_dry))
}


find_best_wet_dry_mesonet_neg = function(x){
  dry = lapply(x, dplyr::filter, drv_cond == "Drying") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  wet = lapply(x, dplyr::filter, drv_cond == "Wetting") %>%
    do.call(rbind,.) %>%
    data.frame() %>%
    select(-drv_cond)%>%
    t()
  
  best_dry = find_best_mesonet_neg(dry)
  best_wet = find_best_mesonet_neg(wet)
  
  return(c(best_wet,best_dry))
}
