#!/bin/bash -i

# Compute Current Drought Conditions
Rscript /home/zhoylman/drought_indicators/spi_app/R/SPI_Map.R >/home/zhoylman/cron-log/cron-log-spi 2>&1
Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R >/home/zhoylman/cron-log/cron-log-spei 2>&1
Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R >/home/zhoylman/cron-log/cron-log-eddi 2>&1

# Update Snotel Data
Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1
Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R

# Build HTML files for Rmd
Rscript /home/zhoylman/drought_indicators/spi_app/R/build_html.R >/home/zhoylman/cron-log/cron-log-spi 2>&1
Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R >/home/zhoylman/cron-log/cron-log-spei 2>&1
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R >/home/zhoylman/cron-log/cron-log-eddi 2>&1

# Copy files to shiny server  
cp -r "/home/zhoylman/drought_indicators" "/srv/shiny-server/"
