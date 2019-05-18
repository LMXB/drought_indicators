#!/usr/bin/env Rscript

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


