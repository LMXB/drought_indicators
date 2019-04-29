library(ncdf4)
library(raster)
library(doParallel)
library(foreach)
library(rgdal)
library(parallel)
library(XML)
library(RCurl)

#update gridmet data
climatology.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_precip_raw"

files = list.files(climatology.dir, pattern = ".tif$", full.names = T)

#compute time from files
time_fdates = fdates(files)
time = data.frame(datetime = as.Date(fdates(files), format = "%Y%m%d"))
time$day = strftime(time$datetime,"%m-%d")

#bring in netcdf
url = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
netcdf = nc_open(url)
test = ncvar_get(netcdf, "status")



gridmet_precip = brick(url, var= "precipitation_amount")
crs(gridmet_precip) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

url2 = paste0("https://www.northwestknowledge.net/metdata/uncompressed/provisional/",2018,"/")

result <- getURL(url2,verbose=F,ftp.use.epsv=T, dirlistonly = T)
fnames <- getHTMLLinks(result,relative=F)
fnames <- fnames[grep("gridmet_",fnames)]
download_links <- paste0(url,fnames)
