# drought_indicators
Upper Missouri River Basin (UMRB) Drought Indicators Document. This repo includes functions to calculate drought indices, R Markdown scripts to generate HTML and several plotting functions. 

All Shiny Apps are in the "apps folder".

spi_app is the first itteration of calculating spi, it is limited to monthly calculations that are not updated until a FULL MONTH of data has been imported.. This app uses functions developed by others. 

spi_app_v2 is the second itteration of the spi_app, this app calculates SPI on a moving window method which will update every day there is new data included in the gridMET dataset. This app has custom SPI calcualtions authored by myself. 

