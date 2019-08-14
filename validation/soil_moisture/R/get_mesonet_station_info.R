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
q = "SELECT station_key, latitude, longitude FROM private_data.stations"
station_data = DBI::dbGetQuery(mesonet_db, q)%>%
  tibble::as_tibble() 
