library(raster)
library(rgdal)
library(dplyr)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(scales)
library(shinycssloaders)
library(sf)
library(raster)
library(htmltools)
library(rgdal)
library(dplyr)
library(zoo)
library(rowr)
library(precintcon)
library(gridExtra)
library(fitdistrplus)
library(tictoc)
library(ncdf4) 
library(lubridate)
library(plotly)
library(glogis)
library(PearsonDS)
library(gsl)
library(lmomco)
library(mapview)
library(parallel)
library(foreach)
library(doParallel)
library(htmlwidgets)

setwd('/home/zhoylman/drought_indicators/soil_moisture')

counties_shp = st_read("../shp_kml/larger_extent/county_umrb.shp")

#load custom functions
source("../mapping_functions/base_map.R")

download.file("https://www.cpc.ncep.noaa.gov//products/Drought/Figures/index/SM_perc.ens.web.tif", 
              destfile = "/home/zhoylman/drought_indicators/soil_moisture/maps/current.tif")

soil_moisture = raster("/home/zhoylman/drought_indicators/soil_moisture/maps/current.tif")%>%
  mask(., readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp"))

soil_moisture[soil_moisture >= 100] = 100
soil_moisture[soil_moisture < 0] = 0

pal <- colorBin(colorRamp(c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66"), interpolate = "spline"), 
                     domain = 0:101, bins = seq(0,101,10), na.color = "transparent")
m_raster = base_map() %>%
    addRasterImage(soil_moisture, colors = pal, opacity = 0.8, group = "Soil Moisture", project = TRUE)

m_raster = m_raster %>%
  addPolygons(data = counties_shp, group = "Counties", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  addLayersControl(position = "topleft",
                   baseGroups = timescale_names,
                   overlayGroups = c("USDM", "States", "Weather", "Streets", "Counties"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  leaflet::hideGroup(c("Counties", "Streets"))%>%
  addLegend(pal = pal, values = seq(0,101,10),
            title = paste0("Soil Moisture<br>Percentile<br>", as.Date(Sys.time())), 
            position = "bottomleft")

saveWidget(m_raster, "/home/zhoylman/drought_indicators/widgets/m_raster_soil_moisture.html", 
           selfcontained = F, libdir = "/home/zhoylman/drought_indicators/widgets/libs/")

