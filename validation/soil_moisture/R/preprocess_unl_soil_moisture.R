files = list.files("/home/zhoylman/drought_indicators_data/unl_mesonet/raw", full.names = T)
files_short = list.files("/home/zhoylman/drought_indicators_data/unl_mesonet/raw", full.names = F)
extracted.dir = "/home/zhoylman/drought_indicators_data/unl_mesonet/unzipped/"

x = files[[1]]
x_short = files_short[[1]]

library(stringr)
library(dplyr)
library(openxlsx)

process_unl = function(x, x_short){
  unzip(x, exdir = extracted.dir)
  internal_files = list.files(paste0(extracted.dir, str_remove(x_short, ".zip")), full.names = T) %>%
    lapply(., openxlsx::read.xlsx, sheet = 1)
  
}



lapply(files, unzip, exdir = extracted.dir)