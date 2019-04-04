---
title: "Upper Missouri River Basin <br> Drought Guide"
author: "Montana Climate Office"
resource_files:
- current_spi_county_30.shx
- current_spi_county_60.shx
- current_spi_county_90.shx
- current_spi_county_180.shx
- current_spi_county_300.shx
- current_spi_watershed_30.shx
- current_spi_watershed_60.shx
- current_spi_watershed_90.shx
- current_spi_watershed_180.shx
- current_spi_watershed_300.shx
- current_spi_county_30.dbf
- current_spi_county_60.dbf
- current_spi_county_90.dbf
- current_spi_county_180.dbf
- current_spi_county_300.dbf
- current_spi_watershed_30.dbf
- current_spi_watershed_60.dbf
- current_spi_watershed_90.dbf
- current_spi_watershed_180.dbf
- current_spi_watershed_300.dbf
output:
  html_document:
    theme: cerulean
    includes:
      after_body: footer.html
runtime: shiny_prerendered
---



preserve0baeefa1107c6b67

<br>

# Introduction

Welcome to the Upper Missouri River Basin (UMRB) drought indicators document. This document provides an overview of several drought metrics commonly used across the globe. In this document We focus on: 

  + 1. Describing general overview and theoretical basis for a metric 
  + 2. Describe the data required for calculation 
  + 3. Compute and display current conditions
  + 4. Briefly discussing the mathematical derivation of the metric 
  + 5. Discuss the relative strengths and weaknesses of a metric 
  + 6. Describe historical validation of a metric 
  + 7. Show validation of a metric using data collected within the UMRB
  + 8. Provide a simple recommendation of its usage across the UMRB for describing differing forms of drought

<br>

# Drought Indicator {.tabset .tabset-fade .tabset-dropdown}

<br>

## **Standardized Precipitation Index (SPI)**

### Overview:
The SPI is a commonly used metric which quantifies precipitation anomalies at various timescales. This metric is often used to approximate soil moisture anomalies when calculated over short time scales (days to weeks) and is related to groundwater and reservoir storage over longer timescales. The values of SPI can be interpreted as a number of standard deviations away from the average (mean) precipitation depth probability for a given time period. 

### Data Requirements: 
  + Precipitation data

### Current Conditions:

preserve239388277734d4b7



### Derivation:  
The SPI quantifies precipitation as a standardized departure from a selected probability distribution function that models the raw precipitation data. The raw precipitation data are typically fitted to a gamma or a Pearson Type III distribution, and then transformed to a normal distribution (Keyantash and NCAR staff, 2018). Normalization of data is important because precipitation data is heavily right hand skewed. This is because smaller precipitation events are much more probable than large events. 

### Key Strengths:
  + Easily calculated, uses only precipitation data
  + Can be used to estimate effects of drought or water abundance on differing hydrologic reservoirs (e.g. soil moisture, groundwater, etc) using different time scales
  + Comparable in across regions (within reason) due to normalization of data
  + Can account for changes in climatology as probability distributions are updated through time
 
### Key Weaknesses: 
  + Doesn't account for atmospheric demand of moisture, this limits identification of flash drought due to high temperature and large vapor pressure deficits
  + Sensitive to biases in precipitation records over time
  + Does not account for the capacity of the landscape to capture precipitation or generate runoff 


### Historical Validation: 
There has been extensive validation of the SPI across the globe. In general, results have shown that the SPI provides similar results to different standardized precipitation indices. 

https://www.sciencedirect.com/science/article/pii/S0168192318303708

https://link.springer.com/article/10.1007/s10584-005-5358-9

https://www.hydrol-earth-syst-sci.net/17/2359/2013/hess-17-2359-2013.html

https://journals.ametsoc.org/doi/abs/10.1175/JHM-D-13-0190.1

https://journals.ametsoc.org/doi/abs/10.1175/JAMC-D-10-05015.1

### UMRB Validation:
Validation in progress

### UMRB Recommendation:
UMRB specific recommendations will be appended to this document as validation is completed

Much of the background information regarding this metric was contributed by NCAR/UCAR Climate Data Guide



## **Standardized Precipitation Evapotranspiration Index (SPEI)**

### Overview: 
SPEI takes into account both precipitation and potential evapotranspiration to describe the wetness/dryness of a time period. Similar to the SPI, SPEI can be calculated at various timescales to represent different drought timescales. As such, the SPEI can approximate different impacts of drought on hydrological conditions and processes depending on the timescale used. Although similar to the SPI, SPEI incorporates the import effect of atmospheric demand on drought which can cause significant impacts over short time scales (flash drought). 

### Data Requirements: 
Precipitation Data
Potential Evapotranspiration Data

### Derivation: 
SPEI is an extension of the SPI in the sense that it uses a normalized probability distribution approximation of raw values to calculate deviation from normals. Similar to SPI, SPEI values are reported in units of standard deviation or z-score (Vicente-Serrano and NCAR staff, 2015). Although, the raw values for this metric are P-PET. 

### Key Strengths:
  + Combines information about water availability (precipitation) and atmospheric demand for moisture (potential evapotranspiration)
  + Relatively simple to calculate and only requires climatological data and statistical models
  + Does not incorporate assumptions about the behavior of the underlying system
  + Can be calculated where only T and P exist (if using Thornthwaite based PET)
  + Can be calculated with more sophisticated PET algorithms if data is available 

### Key Weaknesses:  
  + Sensitive to differences in PET calculations 
  + Requires climatology of data to have accurate statistical reference distributions
  + Sensitive to probability distribution used to normalize data distribution

### Current Conditions:



### Historical Validation:
The SPEI has been used in many studies to understand the effects of drought on hydrologic resource availability, including reservoir, stream discharge and groundwater. In general, SPEI calculated at longer timescales (>12 months) has shown greater correlation with water levels in lakes and reservoirs (McEvoy et al., 2012). 

### UMRB Validation:
Validation in progress

### UMRB Recommendation:
UMRB specific recommendations will be appended to this document as validation is completed

Much of the background information regarding this metric was adapted from the NCAR/UCAR Climate Data Guide (here)


## Palmer Drought Severity Index (PDSI)

## Effective Drought Index (EDI)




preserve0b8ea6537618f460
<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="dependencies">
{"type":"list","attributes":{},"value":[{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","version","src","meta","script","stylesheet","head","attachment","package","all_files","pkgVersion"]},"class":{"type":"character","attributes":{},"value":["html_dependency"]}},"value":[{"type":"character","attributes":{},"value":["jquery"]},{"type":"character","attributes":{},"value":["1.11.3"]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["file"]}},"value":[{"type":"character","attributes":{},"value":["rmd/h/jquery"]}]},{"type":"NULL"},{"type":"character","attributes":{},"value":["jquery.min.js"]},{"type":"NULL"},{"type":"NULL"},{"type":"NULL"},{"type":"character","attributes":{},"value":["rmarkdown"]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["1.11"]}]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","version","src","meta","script","stylesheet","head","attachment","package","all_files","pkgVersion"]},"class":{"type":"character","attributes":{},"value":["html_dependency"]}},"value":[{"type":"character","attributes":{},"value":["bootstrap"]},{"type":"character","attributes":{},"value":["3.3.5"]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["file"]}},"value":[{"type":"character","attributes":{},"value":["rmd/h/bootstrap"]}]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["viewport"]}},"value":[{"type":"character","attributes":{},"value":["width=device-width, initial-scale=1"]}]},{"type":"character","attributes":{},"value":["js/bootstrap.min.js","shim/html5shiv.min.js","shim/respond.min.js"]},{"type":"character","attributes":{},"value":["css/cerulean.min.css"]},{"type":"NULL"},{"type":"NULL"},{"type":"character","attributes":{},"value":["rmarkdown"]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["1.11"]}]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","version","src","meta","script","stylesheet","head","attachment","package","all_files","pkgVersion"]},"class":{"type":"character","attributes":{},"value":["html_dependency"]}},"value":[{"type":"character","attributes":{},"value":["jquery"]},{"type":"character","attributes":{},"value":["1.11.3"]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["file"]}},"value":[{"type":"character","attributes":{},"value":["rmd/h/jquery"]}]},{"type":"NULL"},{"type":"character","attributes":{},"value":["jquery.min.js"]},{"type":"NULL"},{"type":"NULL"},{"type":"NULL"},{"type":"character","attributes":{},"value":["rmarkdown"]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["1.11"]}]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","version","src","meta","script","stylesheet","head","attachment","package","all_files","pkgVersion"]},"class":{"type":"character","attributes":{},"value":["html_dependency"]}},"value":[{"type":"character","attributes":{},"value":["navigation"]},{"type":"character","attributes":{},"value":["1.1"]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["file"]}},"value":[{"type":"character","attributes":{},"value":["rmd/h/navigation-1.1"]}]},{"type":"NULL"},{"type":"character","attributes":{},"value":["tabsets.js"]},{"type":"NULL"},{"type":"NULL"},{"type":"NULL"},{"type":"character","attributes":{},"value":["rmarkdown"]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["1.11"]}]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","version","src","meta","script","stylesheet","head","attachment","package","all_files","pkgVersion"]},"class":{"type":"character","attributes":{},"value":["html_dependency"]}},"value":[{"type":"character","attributes":{},"value":["highlightjs"]},{"type":"character","attributes":{},"value":["9.12.0"]},{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["file"]}},"value":[{"type":"character","attributes":{},"value":["rmd/h/highlightjs"]}]},{"type":"NULL"},{"type":"character","attributes":{},"value":["highlight.js"]},{"type":"character","attributes":{},"value":["default.css"]},{"type":"NULL"},{"type":"NULL"},{"type":"character","attributes":{},"value":["rmarkdown"]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["1.11"]}]}]}
</script>
<!--/html_preserve-->
<!--html_preserve-->
<script type="application/shiny-prerendered" data-context="execution_dependencies">
{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["packages"]}},"value":[{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["packages","version"]},"class":{"type":"character","attributes":{},"value":["data.frame"]},"row.names":{"type":"integer","attributes":{},"value":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81]}},"value":[{"type":"character","attributes":{},"value":["assertthat","base","bindr","bindrcpp","class","classInt","codetools","colorspace","compiler","crayon","crosstalk","datasets","DBI","digest","dplyr","e1071","evaluate","fitdistrplus","ggplot2","glue","graphics","grDevices","grid","gridExtra","gtable","htmltools","htmlwidgets","httpuv","knitr","later","lattice","lazyeval","leaflet","leaflet.extras","lsei","lubridate","magrittr","markdown","MASS","Matrix","methods","mime","munsell","ncdf4","npsurv","pillar","pkgconfig","plyr","precintcon","prettydoc","promises","purrr","R6","raster","Rcpp","rgdal","rlang","rmarkdown","rowr","scales","sf","shiny","shinycssloaders","sp","splines","stats","stringi","stringr","survival","tibble","tictoc","tidyselect","tools","units","utils","webshot","withr","xfun","xtable","yaml","zoo"]},{"type":"character","attributes":{},"value":["0.2.0","3.5.2","0.1.1","0.2.2","7.3-15","0.3-1","0.2-16","1.3-2","3.5.2","1.3.4","1.0.0","3.5.2","1.0.0","0.6.18","0.7.8","1.7-0","0.12","1.0-14","3.1.0","1.3.0","3.5.2","3.5.2","3.5.2","2.3","0.2.0","0.3.6","1.3","1.4.5.1","1.21","0.7.5","0.20-38","0.2.1","2.0.2","1.0.0","1.2-0","1.7.4","1.5","0.9","7.3-51.1","1.2-15","3.5.2","0.6","0.5.0","1.15","0.4-0","1.3.1","2.0.2","1.8.4","2.3.0","0.2.1","1.0.1","0.2.5","2.3.0","2.8-4","1.0.0","1.3-6","0.3.0.1","1.11","1.1.3","1.0.0","0.7-2","1.2.0","0.2.0","1.3-1","3.5.2","3.5.2","1.2.4","1.3.1","2.43-3","1.4.2","1.0","0.2.5","3.5.2","0.6-2","3.5.2","0.5.1","2.1.2","0.4","1.8-3","2.2.0","1.8-4"]}]}]}
</script>
<!--/html_preserve-->
