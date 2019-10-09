library(tidyverse)
library(DBI)
library(magrittr)

credentials = read.table("/home/zhoylman/env_var/mco_db.txt")

mesonet_db <- DBI::dbConnect(odbc::odbc(),
                             Driver = "ODBC Driver 17 for SQL Server",
                             Server = "cfcsql17.gs.umt.edu",
                             Database = "MCOMesonet",
                             UID = (as.character(credentials$V1[1])),
                             PWD = (as.character(credentials$V1[2])),
                             Port = 1433)

q <- "
SET NOCOUNT ON
DECLARE @raw observations_raw
INSERT INTO @raw
SELECT *
FROM observations.raw
WHERE (
    DATEPART(hour, datetime AT TIME ZONE 'Mountain Standard Time') = 0 AND
    DATEPART(minute, datetime AT TIME ZONE 'Mountain Standard Time') = 0 AND
    (measurement = 'Water Content' OR
    measurement = 'Soil Temperature')
)

DECLARE @l0 observations_level_0
INSERT INTO @l0
SELECT *
FROM api.get_L0(@raw)

SELECT *
FROM api.get_L1(@l0)
"

mesonet_soil_moisture_raw <-
  DBI::dbGetQuery(mesonet_db, q) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(datetime =
                  datetime %>%
                  lubridate::as_datetime() %>%
                  lubridate::with_tz("America/Denver") %>%
                  lubridate::as_date()) %>%
  dplyr::left_join(dplyr::tbl(mesonet_db,
                              dbplyr::in_schema("meta","elements")) %>%
                     dplyr::select(name,description_short) %>%
                     dplyr::collect(),
                   by = c("element" = "name")) %>%
  dplyr::mutate_at(.vars = dplyr::vars(station_key,
                                       element,
                                       description_short,
                                       units),
                   .funs = ~factor(.))

unfrozen_time = mesonet_soil_moisture_raw %>%
  group_by(datetime) %>%
  dplyr::filter(element == "soilt_00" | element == "soilt_04" |
                element == "soilt_08" | element == "soilt_20" |
                element == "soilt_36") %>%
  group_by(datetime, station_key) %>%
  mutate(min_temp = min(value, na.rm = T)) %>%
  dplyr::filter(min_temp > 0) %>%
  select(station_key, datetime) %>%
  unite("unfrozen", station_key:datetime) %>%
  distinct()

mesonet_soil_moisture_unfrozen = mesonet_soil_moisture_raw %>%
  unite("unfrozen", station_key:datetime, remove = F) %>% 
  dplyr::filter(element == "soilwc00" | element == "soilwc04" |
                element == "soilwc08" | element == "soilwc20" |
                element == "soilwc36") %>%
  dplyr::filter(unfrozen %in% unfrozen_time$unfrozen) %>%
  select(-unfrozen)

mesonet_soil_moisture = mesonet_soil_moisture_unfrozen

save(mesonet_soil_moisture, file = "/home/zhoylman/drought_indicators_data/mesonet/mesonet_soil_moisture_unfrozen.RData")

