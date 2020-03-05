library(tidyverse)
library(DBI)
library(magrittr)

#import credentials
credentials = read.table("/home/zhoylman/env_var/mco_db.txt")

#connect to sql database
mesonet_db <- DBI::dbConnect(odbc::odbc(),
                             Driver = "ODBC Driver 17 for SQL Server",
                             Server = "cfcsql.cfc.umt.edu",
                             Database = "MCOMesonet",
                             UID = (as.character(credentials$V1[1])),
                             PWD = (as.character(credentials$V1[2])),
                             Port = 1433)

#DBI::dbDisconnect(mesonet_db)

# import needed tables for joins
# logger deployment 
logger_deployment = dplyr::tbl(mesonet_db,
           dbplyr::in_schema("public_data","logger_deployment")) %>%
  select(logger_sn, station_key, date_start, date_end) 

# sensor deployment 
sensor_deployment = dplyr::tbl(mesonet_db,
          dbplyr::in_schema("public_data","sensor_deployment")) %>%
  select(logger_sn, sensor_sn, port, date_start, date_end, height_depth) 


#sensor inventory
sensor_inventory = dplyr::tbl(mesonet_db,
           dbplyr::in_schema("private_data","sensor_inventory")) 

#sensor elements
elements = dplyr::tbl(mesonet_db,
                      dbplyr::in_schema("meta","elements")) %>%
  select(name, height_depth, sensor_measurement)




data = dplyr::tbl(mesonet_db,
                  dbplyr::in_schema("observations","raw")) %>%
  dplyr::select(logger_sn, datetime, port, measurement, units, value)%>%
  #filter by hour and minute to get first reading of the day
  dplyr::mutate(date = as.Date(datetime),
                hour = DATEPART(sql("hour"), datetime),
                minute = DATEPART(sql("minute"), datetime)) %>% 
  # dplyr::filter(hour == 7 & minute == 0) %>%
  # dplyr::filter(date > as.Date ('2019-06-01') & date < as.Date ('2019-06-03')) %>%
  head() %>%
  dplyr::collect() %>%
  dplyr::mutate(datetime = lubridate::as_datetime(datetime) %>% lubridate::force_tz("US/Mountain")) %$%
  {value/hour}

  lubridate::tz(datetime)



%>% #get midnight value in UTC
  #join by logger name
  dplyr::left_join(logger_deployment,by=c("logger_sn")) %>%
  #filter for date end
  dplyr::filter(date > date_start & is.na(date_end) | date < date_end)%>%
  #select important collumns to spead up querry and remove date_start and date_end
  # becasue we need it for the next join but for sensors, not loggers
  dplyr::select(logger_sn, datetime, port, measurement, units, value, date, station_key) %>%
  #join by sensor sn and port
  dplyr::left_join(sensor_deployment,by=c("logger_sn", "port")) %>%
  #filter for date end
  dplyr::filter(date > date_start & is.na(date_end) | date < date_end)%>%
  # join to get element names
  dplyr::left_join(elements, by=c("measurement" = "sensor_measurement", "height_depth")) %>%
  dplyr::select(station_key, name, date, value, units) %>%
  #pull soil variables of interest
  dplyr::filter(name %in% c("soilwc00","soilwc04","soilwc08",
                     "soilwc20","soilwc36",
                     "soilt_00", "soilt_04", "soilt_08",
                     "soilt_20", "soilt_36")) %>%
  dplyr::collect()

# arrange it for clarity and remove duplicate rows
data = data %>%
  arrange(station_key, date, name) %>%
  distinct()

#flat file export if needed
# station_location = dplyr::tbl(mesonet_db,
#                               dbplyr::in_schema("private_data","stations"))%>%
#   select(station_key, latitude, longitude) %>%
#   collect()
# 
# station_location = station_location %>%
#   filter(station_key %in% data$station_key)
# 
# write.csv(data, "/mnt/ScratchDrive/data/holden_hoylman_shared/mesonet/mesonet_vwc_soilT_data.csv")
# write.csv(station_location, "/mnt/ScratchDrive/data/holden_hoylman_shared/mesonet/station_location_private_accuracy.csv")

#site specific check
load('/home/zhoylman/drought_indicators_data/mesonet/depreciated/mesonet_soil_moisture.RData')

data_new = data %>%
  dplyr::filter(station_key == "arskeogh")%>%
  dplyr::filter(name %in% c("soilwc04"))

data_old = mesonet_soil_moisture %>%
  dplyr::filter(station_key == "arskeogh")%>%
  dplyr::filter(element %in% "soilwc04")

plot(data_new$date, data_new$value)
points(data_old$datetime, data_old$value, col = 'red')

#compute a factor variable that describes timeperiods that are unfrozen
#will be used to filter soil moisture data below  
unfrozen_time = data %>%
  dplyr::filter(name == "soilt_00" | name == "soilt_04" |
                  name == "soilt_08" | name == "soilt_20" |
                  name == "soilt_36") %>%
  group_by(date, station_key) %>%
  mutate(min_temp = min(value, na.rm = T)) %>%
  dplyr::filter(min_temp > 0) %>%
  select(station_key, date) %>%
  unite("unfrozen", station_key, date) %>%
  distinct()

#filter soil moisture data from unfrozen time periods
mesonet_soil_moisture_unfrozen = data %>%
  unite("unfrozen", station_key,date, remove = F) %>% 
  dplyr::filter(name == "soilwc00" | name == "soilwc04" |
                  name == "soilwc08" | name == "soilwc20" |
                  name == "soilwc36") %>%
  dplyr::filter(unfrozen %in% unfrozen_time$unfrozen) %>%
  select(-unfrozen)

#read in master mesonet list
stations = read_csv('/home/zhoylman/drought_indicators_data/mesonet/station_data_clean.csv')

#filter for the stations of interest
mesonet_soil_moisture_unfrozen = mesonet_soil_moisture_unfrozen %>%
  dplyr::filter(station_key %in% stations$station_key)

#rename
mesonet_soil_moisture = mesonet_soil_moisture_unfrozen

#save
save(mesonet_soil_moisture, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_soil_moisture_unfrozen.RData")



######################## Trouble shooting dublicate obs ####################


# make the sql querry using dplyr (start of day values of all soil vars)
# data = dplyr::tbl(mesonet_db,
#            dbplyr::in_schema("observations","raw")) %>%
#   dplyr::select(logger_sn, datetime, port, measurement, units, value)%>%
#   #join by logger name
#   dplyr::left_join(logger_deployment,
#                    by=c("logger_sn")
#   ) %>%
#   dplyr::filter(station_key == "arskeogh") %>%
#   dplyr::filter(measurement == "Precipitation") %>%
#   dplyr::mutate(date = as.Date(datetime)) %>%
#   dplyr::filter(date > as.Date("2019-05-10") & date < as.Date("2019-05-20"))%>%
#   collect()
# 
# data = dplyr::tbl(mesonet_db,
#                   dbplyr::in_schema("observations","raw")) %>%
#   dplyr::filter(logger_sn == "06-00761") %>%
#   dplyr::filter(measurement == "Precipitation") %>%
#   dplyr::mutate(date = as.Date(datetime)) %>%
#   dplyr::filter(date > as.Date("2019-05-10") & date < as.Date("2019-05-20"))%>%
#   collect()
# 

dplyr::tbl(mesonet_db,
                  dbplyr::in_schema("observations","raw")) %>%
  dplyr::select(logger_sn, datetime, port, measurement, units, value)%>%
  dplyr::filter(logger_sn == "06-00761") %>%
  dplyr::filter(measurement == "Precipitation") %>%
  dplyr::mutate(date = as.Date(datetime),
                hour = DATEPART(sql("hour"), datetime),
                minute = DATEPART(sql("minute"), datetime)) %>%
  dplyr::filter(hour == 7 & minute == 0) %>% #manually convert to mountian
  dplyr::filter(date > as.Date("2019-05-10") & date < as.Date("2019-05-20"))%>%
  dplyr::filter(hour == 0 & minute == 0) %>%
  dplyr::filter(is.na(date_end) | date < date_end)%>%
  dplyr::filter(station_key == "arskeogh") %>%
  dplyr::filter(measurement == "Precipitation") %>%
  dplyr::filter(date > as.Date("2019-05-10") & date < as.Date("2019-05-20")) %>%
  collect()