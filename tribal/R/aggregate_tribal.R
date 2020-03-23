tribal = st_read('/home/zhoylman/drought_indicators/shp_kml/UMRB_tribal_lands_simple.geojson') 

#simple = st_simplify(tribal, dTolerance = 0.001)

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

aggregate_tribal = function(x){
  # Extract raster values for each tribe 
  r.vals <- extract(x, tribal)
  
  # Use list apply to calculate median for each polygon
  r.median <- lapply(r.vals, FUN=median,na.rm=TRUE)
  
  r.median = nullToNA(r.median)
  
  tribal_export = tribal
  tribal_export$average = as.vector(unlist(r.median))
  
  return(tribal_export)
}

aggregate_tribal_precip = function(anomoly, percentile){
  # Extract raster values for each tribe 
  r.vals.anom <- extract(anomoly, tribal)
  r.vals.perc <- extract(percentile, tribal)
  
  # Use list apply to calculate median for each polygon
  r.median.anom <- lapply(r.vals.anom, FUN=median,na.rm=TRUE)
  r.median.perc <- lapply(r.vals.perc, FUN=median,na.rm=TRUE)
  
  r.median.anom = nullToNA(r.median.anom)
  r.median.perc = nullToNA(r.median.perc)
  
  tribal_export = tribal
  tribal_export$anomaly = as.vector(unlist(r.median.anom))
  tribal_export$percentile = as.vector(unlist(r.median.perc))
  
  return(tribal_export)
}
