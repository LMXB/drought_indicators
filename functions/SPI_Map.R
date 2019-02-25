## LOAD THE REQUIRED LIBRARYS
library(ncdf4) # Downlaoded from https://github.com/pmjherman/r-ncdf4-build-opendap-windows
library(lubridate)
library(dplyr)
library(zoo)
library(plyr)
library(rowr)
library(precintcon)
library(gridExtra)
library(raster)


## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)

time_scale = 30

time = data.frame(datetime = as.POSIXct(as.Date(as.numeric(substring(names(raster_precip),2)), origin="1900-01-01")))
time$day = yday(time$datetime)

first_date_breaks = which(time$day == time$day[length(time$datetime)])
second_date_breaks = first_date_breaks-(time_scale-1)

#if there are negative indexes remove last year (incomplete data range)
#change this to remove all indexes from both vectors that are negative

# if(!all(second_date_breaks < 0)){
#   first_date_breaks = first_date_breaks[-1]
#   second_date_breaks = second_date_breaks[-1]
# }
if(!all(second_date_breaks < 0)){
  pos_index = which(second_date_breaks > 0)
  first_date_breaks = first_date_breaks[c(pos_index)]
  second_date_breaks = second_date_breaks[c(pos_index)]
}

#create slice vectors and group by vectors
for(j in 1:length(first_date_breaks)){
  if(j == 1){
    slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
    group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
  }
  else{
    slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
    group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
  }
}

library(doParallel)
library(foreach)

#summing first here

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

tic()

raster_precip_clipped = foreach(i=1:41) %dopar% {
  library(raster)
  temp = sum(raster_precip[[slice_vec[group_by_vec == i]]])
  crop(temp, extent(montana))
}

toc()

stopCluster(cl)

#243 on 7 cores seconds

test = data.frame(matrix(nrow = length(values(raster_precip_clipped[[1]])), ncol = 41))
for(i in 1:41){
  test[,i] = values(raster_precip_clipped[[i]])
}


spi_fun <- function(x) { 

  fit.gamma = tryCatch(fitdist(x, distr = "gamma", method = "mle"), error=function(e) NA)
  if(is.na(fit.gamma)){
    return(NA)
  }
  else{
    fit.cdf = pgamma(x, fit.gamma$estimate[1], fit.gamma$estimate[2])
    standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
    return(standard_norm[length(standard_norm)]) 
  }
}

test_spi = apply(test, 1, spi_fun)

test_spi_map = raster_precip_clipped[[1]]

values(test_spi_map) = test_spi





x = as.numeric(test[1,])

fit.gamma = fitdist(x, distr = "gamma", method = "mle")
fit.cdf = pgamma(x, fit.gamma$estimate[1], fit.gamma$estimate[2])
standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)








x1 <- calc(s, fun)



fit.gamma <- fitdist(data_time_filter$sum, distr = "gamma", method = "mle")

#calculate modeled cdf
fit.cdf = pgamma(data_time_filter$sum, fit.gamma$estimate[1], fit.gamma$estimate[2])

par(mfrow = c(1,2))
cdfcomp(fit.gamma)
plot(data_time_filter$sum,fit.cdf)

#CLOSE but do this for the gamma fit!!!
#standard_norm raw data
standard_norm = qnorm(ecdf(data_time_filter$sum)(data_time_filter$sum), mean = 0, sd = 1)

#standard_norm modelled data
#this is it!!!
standard_norm2 = qnorm(fit.cdf, mean = 0, sd = 1)
















# try cropping all maps first

cl = makeCluster(detectCores())
registerDoParallel(cl)

tic()

raster_precip_clipped = foreach(i=1:41) %dopar% {
  library(raster)
  temp = sum(raster_precip[[slice_vec[group_by_vec == i]]])
  crop(temp, extent(montana))
}

toc()

stopCluster(cl)






#try cropping first here. 

cl = makeCluster(detectCores())
registerDoParallel(cl)

tic()

raster_precip_clipped = foreach(i=1:41) %dopar% {
  library(raster)
  temp = sum(raster_precip[[slice_vec[group_by_vec == i]]])
  crop(temp, extent(montana))
}

toc()

stopCluster(cl)




















test_clipped = crop(test, extent(montana))


time_clipped_raster = raster[[slice_vec]]


montana = rgdal::readOGR("D:\\Git_Repo\\drought_indicators\\montana_outline.kml")

tic()
clipped = crop(raster, extent(montana))
toc()

#example code of how I will agregate monthly data?
# precip_test = time_clipped_raster %>% group_by(group_by_vec) %>%
#   summarize(sum = sum(time_clipped_raster, na.rm = TRUE))
