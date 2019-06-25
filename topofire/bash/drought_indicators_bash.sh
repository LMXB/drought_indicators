#!/bin/bash -i
#load the gcc library
module load gcc/5.1

# Update Gridmet data
Rscript /home/zhoylman/drought_indicators/topofire/R/gridmet_update.R
# calcualte SPI
Rscript /home/zhoylman/drought_indicators/topofire/R/SPI_Map_Cpp.R
# reproject forecast grids
Rscript /home/zhoylman/drought_indicators/topofire/R/ingest_forecast.R
# calcualte SPI Forecasts
Rscript /home/zhoylman/drought_indicators/topofire/R/SPI_Map_Cpp_Forecast.R
# calcualte SPEI
Rscript /home/zhoylman/drought_indicators/topofire/R/SPEI_Map_Cpp.R
# calcualte SPEI Forecasts (also ingests pet forecasts)
Rscript /home/zhoylman/drought_indicators/topofire/R/SPEI_Map_Cpp_Forecast.R
# calcualte EDDI
Rscript /home/zhoylman/drought_indicators/topofire/R/EDDI_Map_Cpp.R
# calcualte EDDI Forecasts (also ingests pet forecasts, just incase)
Rscript /home/zhoylman/drought_indicators/topofire/R/EDDI_Map_Cpp_Forecast.R
# calculate SEDI
Rscript /home/zhoylman/drought_indicators/topofire/R/SEDI_Map_Cpp.R
# calculate SEDI Forecasts (ingests def forecasts)
Rscript /home/zhoylman/drought_indicators/topofire/R/SEDI_Map_Cpp_Forecast.R
