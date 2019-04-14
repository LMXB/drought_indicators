#Test for gridmet continuity

climatology.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_Climatology/gridMET_precip_raw/"
git.dir = '/home/zhoylman/drought_indicators/zoran/R/'
work.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_test/"
write.dir = "/mnt/ScratchDrive/data/Hoylman/gridMET_test/annual_precip_sum/"

files = list.files(climatology.dir, pattern = ".tif$", full.names = T)

years = list(y2019 = files[(length(files)-30):length(files)],
             y2018 = files[(length(files)-365 - 30):(length(files)-365)],
                     files[(length(files)-365*2 - 30):(length(files)-365*2)])

source(paste0(git.dir, "fdates.R"))

time = lapply(years, FUN = fdates)

for(i in 1:length(years)){
  tmp.dir <- paste0(work.dir, "tmp_dir/", "temp_sum_",time[[i]][1],'-',time[[i]][length(time[[i]])],'/')
  dir.create(tmp.dir)
  
  
  flist = years[[i]]
  datetime_char = time[[i]]
  
  txt.filename <- paste0(tmp.dir, "do_sum_group_", time[[i]][1],'-',time[[i]][length(time[[i]])],".txt")
  
  write.table(flist, file=txt.filename, quote=F, row.names=F, col.names=F, append=F)
  out.file <- paste0(write.dir, "sum_raster_", time[[i]][1],'-',time[[i]][length(time[[i]])], ".tif")
  
  # call C++ sum program here
  # aruments are: 1. text file which lists geotiffs; 2. name of the output file; 3. NoData value 
  system(paste0("/mnt/ScratchDrive/data/Hoylman/drought_anomaly/drought_anomaly_sum ", txt.filename, " ", out.file, " ", -9999  ))
}
