# USDM years
years = 2000:2019
# location to unzip files (each shp file here is zipped too)
unzip.dir = "/home/zhoylman/drought_indicators_data/usdm_data/unzipped/"
# location to extract zipped shp files
extracted.dir = "/home/zhoylman/drought_indicators_data/usdm_data/extracted_shp/"
# loop through years to extract
for(i in 1:length(years)){
  # define url to download
  url = paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/", years[i],"_USDM_M.zip")
  # define dest files
  destfile = paste0("/home/zhoylman/drought_indicators_data/usdm_data/raw/", years[i], "_USDM_M.zip")
  # download raw data
  download.file(url, destfile, method = "curl") 
  # unzip the raw data for the year (each map is still zipped)
  unzip(destfile, exdir = unzip.dir)
}
# find all unzipped maps (each USDM shp)
unzipped.files = list.files(unzip.dir, full.names = T)
# unzip all the maps
lapply(unzipped.files, unzip, exdir = extracted.dir)