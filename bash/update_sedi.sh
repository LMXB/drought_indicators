#!/bin/bash

# Compute Current SEDI Conditions
Rscript /home/zhoylman/drought_indicators/sedi_app/R/SEDI_Map.R
Rscript /home/zhoylman/drought_indicators/sedi_app/R/build_html.R

# Copy files to shiny server  
cp -r "/home/zhoylman/drought_indicators" "/srv/shiny-server/"
