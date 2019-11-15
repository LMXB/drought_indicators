#!/bin/bash

# Compute Current Drought Conditions
Rscript /home/zhoylman/drought_indicators/spi_app/R/SPI_Map.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R
Rscript /home/zhoylman/drought_indicators/precipitation/R/precipitation_map.R

# Update Snotel Data
Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1
Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1

# Build HTML files for Rmd
Rscript /home/zhoylman/drought_indicators/spi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/snotel/R/build_html.R
Rscript /home/zhoylman/drought_indicators/precipitation/R/build_html.R
Rscript /home/zhoylman/drought_indicators/soil_moisture/R/build_html.R

# Convert Rmd to HTML with Rmarkdown render
Rscript /home/zhoylman/drought_indicators/flex_new/R/knit_rmd.R >/home/zhoylman/cron-log/cron-log-knitt 2>&1

# Copy files to shiny server  
cp -r "/home/zhoylman/drought_indicators" "/srv/shiny-server/"
