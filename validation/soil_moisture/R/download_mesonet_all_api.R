library(dplyr)

start = Sys.time()

soil_moisture = RCurl::getURL("https://mesonet.climate.umt.edu/api/observations?elements=soilt_00,soilt_04,soilt_08,soilt_20,soilt_36,soilwc00,soilwc04,soilwc08,soilwc20,soilwc36&latest=false&tz=US%2FMountain&wide=false&simple_datetime=false&type=csv") %>%
  read_csv()%>%
  mutate(datetime = datetime %>%
           lubridate::with_tz("America/Denver"))

print(Sys.time() - start)

unfrozen_time = soil_moisture %>%
  group_by(datetime) %>%
  dplyr::filter(name == "soilt_00" | name == "soilt_04" |
                  name == "soilt_08" | name == "soilt_20" |
                  name == "soilt_36") %>%
  group_by(datetime, station_key) %>%
  mutate(min_temp = min(value, na.rm = T)) %>%
  dplyr::filter(min_temp > 0)%>%
  select(station_key, datetime) %>%
  unite("unfrozen", station_key:datetime) %>%
  distinct()

mesonet_soil_moisture_unfrozen = soil_moisture %>%
  unite("unfrozen", station_key:datetime, remove = F) %>% 
  dplyr::filter(name == "soilwc00" | name == "soilwc04" |
                  name == "soilwc08" | name == "soilwc20" |
                  name == "soilwc36") %>%
  dplyr::filter(unfrozen %in% unfrozen_time$unfrozen) %>%
  select(-unfrozen)

