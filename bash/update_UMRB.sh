#!/bin/bash

# Compute Current Drought Conditions
Rscript /home/zhoylman/drought_indicators/spi_app/R/SPI_Map.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R

# Update Snotel Data
Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R
Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R

# Build HTML files for Rmd
Rscript /home/zhoylman/drought_indicators/spi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R

# Copy files to shiny server  
cp -r "/home/zhoylman/drought_indicators" "/srv/shiny-server/"
