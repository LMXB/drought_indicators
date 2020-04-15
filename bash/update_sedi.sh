#!/bin/bash

# Compute Current SEDI Conditions and Build SEDI Widget
Rscript /home/zhoylman/drought_indicators/sedi_app/R/SEDI_Map.R
Rscript /home/zhoylman/drought_indicators/sedi_app/R/build_html.R

# Copy widget files to nginx and git deployment folders
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1

cp -r /home/zhoylman/drought_indicators/widgets /home/zhoylman/drought_indicators/docs/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /home/zhoylman/drought_indicators/docs/ 

/home/zhoylman/drought_indicators/bash/git_update.sh