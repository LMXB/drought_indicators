#!/bin/bash

# Compute Current SEDI Conditions and Build SEDI Widget
Rscript /home/zhoylman/drought_indicators/sedi_app/R/SEDI_Map.R
Rscript /home/zhoylman/drought_indicators/sedi_app/R/build_html.R

# Copy widget files to nginx folder
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
