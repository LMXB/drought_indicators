#!/bin/bash

# Update Snotel Data
Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1
Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1

# Update SNODAS data
Rscript /home/zhoylman/drought_indicators/snodas/R/snodas_maps.R >/home/zhoylman/drought_indicators/snodas/log 2>&1

Rscript /home/zhoylman/drought_indicators/snotel/R/build_html.R

cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/bash/log 2>&1

cp -r /home/zhoylman/drought_indicators/widgets /home/zhoylman/drought_indicators/docs/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /home/zhoylman/drought_indicators/docs/ 