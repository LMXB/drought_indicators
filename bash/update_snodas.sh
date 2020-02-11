#!/bin/bash

Rscript /home/zhoylman/drought_indicators/snodas/R/snodas_maps.R >/home/zhoylman/drought_indicators/snodas/log 2>&1
Rscript /home/zhoylman/drought_indicators/snotel/R/build_html.R >/home/zhoylman/drought_indicators/snodas/html-log 2>&1
cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ >/home/zhoylman/drought_indicators/snodas/transfer-log 2>&1