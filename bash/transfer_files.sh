#!/bin/bash

cp -r /home/zhoylman/drought_indicators/widgets /var/www/shiny.cfc.umt.edu/drought_indicators/ 
cp -r /home/zhoylman/drought_indicators/snotel/plots /var/www/shiny.cfc.umt.edu/drought_indicators/ 

cp -r /home/zhoylman/drought_indicators/widgets /home/zhoylman/drought_indicators/docs/ >/home/zhoylman/bash/log 2>&1
cp -r /home/zhoylman/drought_indicators/snotel/plots /home/zhoylman/drought_indicators/docs/ 