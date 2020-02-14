library(ncdf4)
library(raster)

# Monthly Global = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cpcsoil/soilw.mon.mean.v2.nc"


url = "http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/cpcsoil/soilw.mon.mean.v2.nc"
nc <- brick(url)

