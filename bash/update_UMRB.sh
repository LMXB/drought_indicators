#!/bin/bash

# Compute Current Drought Conditions
Rscript /home/zhoylman/drought_indicators/spi_app/R/SPI_Map.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R
Rscript /home/zhoylman/drought_indicators/precipitation/R/precipitation_map.R
Rscript /home/zhoylman/drought_indicators/temperature/R/temperature_map.R

# Update Snotel Data
Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1
Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1

# Update SNODAS data
Rscript /home/zhoylman/drought_indicators/snodas/R/snodas_maps.R >/home/zhoylman/drought_indicators/snodas/log 2>&1

# Build HTML widget files
Rscript /home/zhoylman/drought_indicators/spi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/snotel/R/build_html.R
Rscript /home/zhoylman/drought_indicators/precipitation/R/build_html.R
Rscript /home/zhoylman/drought_indicators/soil_moisture/R/build_html.R
Rscript /home/zhoylman/drought_indicators/temperature/R/build_html.R

# Copy widget files to nginx folder
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1

# Copy plots 
cp -r /home/zhoylman/drought_indicators/snotel/plots /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1