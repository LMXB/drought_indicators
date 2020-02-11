#!/bin/bash

Rscript /home/zhoylman/drought_indicators/spi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/snotel/R/build_html.R
Rscript /home/zhoylman/drought_indicators/precipitation/R/build_html.R
Rscript /home/zhoylman/drought_indicators/sedi_app/R/build_html.R

# Copy widget files to nginx folder
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
