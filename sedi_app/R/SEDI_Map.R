vars = read.table("/home/zhoylman/env_var/var1")

call = paste0("scp -r ", vars$V1[1], ":", "/mnt/DataDrive2/data/drought_indices/sedi/current /home/zhoylman/drought_indicators/sedi_app/maps/")

system(call)
