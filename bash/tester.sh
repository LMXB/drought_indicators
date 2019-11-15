#!/bin/bash

Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R

Rscript /home/zhoylman/drought_indicators/spei_app/R/build_html.R
Rscript /home/zhoylman/drought_indicators/eddi_app/R/build_html.R