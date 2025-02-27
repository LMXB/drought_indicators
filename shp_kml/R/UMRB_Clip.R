library(rgdal)
library(sf)
library(dplyr)
library(stringr)

states = read_sf("/home/zhoylman/Downloads/tl_2017_us_state/tl_2017_us_state.shp")

county = read_sf("/home/zhoylman/Downloads/UScounties/UScounties.shp")

watersheds = read_sf("/home/zhoylman/Downloads/huc8_conus/HUC8_CONUS/HUC8_US.shp")

states_umrb = states %>% 
  dplyr::filter(NAME == "Montana"|NAME == "Idaho"| NAME == "Wyoming"|NAME == "South Dakota"|NAME == "North Dakota")%>%
  sf::st_simplify()

county_umrb = county %>%
  dplyr::filter(STATE_NAME == "Montana"|STATE_NAME == "Idaho"| STATE_NAME == "Wyoming"|STATE_NAME == "South Dakota"|STATE_NAME == "North Dakota")%>%
  sf::st_simplify()

outline_umrb = st_combine(states_umrb) %>%
  sf::st_simplify()

st_crs(outline_umrb) = 4326

watersheds_umrb = watersheds %>%
  st_intersection(outline_umrb) 

watersheds_umrb_simple = rmapshaper::ms_simplify(watersheds_umrb, keep = 0.005)

st_write(county_umrb, "/home/zhoylman/drought_indicators/shp_kml/larger_extent/county_umrb.shp", "county_umrb", driver = "ESRI Shapefile")
st_write(outline_umrb, "/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp", "outline_umrb", driver = "ESRI Shapefile")
st_write(watersheds_umrb_simple, "/home/zhoylman/drought_indicators/shp_kml/larger_extent/watersheds_umrb.shp", "watersheds_umrb", driver = "ESRI Shapefile")
