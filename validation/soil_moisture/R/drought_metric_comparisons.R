load('/home/zhoylman/drought_indicators_data/mesonet/mesonet_eddi.RData')
load('/home/zhoylman/drought_indicators_data/mesonet/mesonet_spei.RData')

eddi = mesonet_eddi[[57]][[5]] %>%
  dplyr::filter(time > as.Date('2010-01-01')) %>%
  mutate(yday = lubridate::yday(time)) 

eddi = snotel_eddi[[57]][[5]] %>%
  dplyr::filter(time > as.Date('2010-01-01')) %>%
  mutate(yday = lubridate::yday(time)) 

spei = mesonet_spei[[1]][[10]] %>%
  dplyr::filter(time > as.Date('2010-01-01')) %>%
  mutate(yday = lubridate::yday(time)) 

plot(eddi$time, eddi$eddi, type = 'l')

eddi %>%
  dplyr::filter(yday == 200)

spei %>%
  dplyr::filter(yday == 200)
