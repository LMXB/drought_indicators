#!/usr/bin/Rscript

#update gridmet data
#this script runs 2 main operations
# 1. fill in any missing data in the on disk climatology
# 2. remove and recalulate the last 4 months of data (cant find information in meta data about provisional)

library(raster)
library(ncdf4)
library(rgdal)
library(parallel)
library(foreach)
library(doParallel)

#set climatology dir and git dirs
git.dir = "/home/zhoylman/drought_indicators/topofire/R/"
climatology.dir = "/mnt/DataDrive2/data/drought_indices/maca/precip_2.5km/"

#import master grid (needed for gridmet update function)
master_grid = raster("/home/zhoylman/drought_indicators/topofire/master_warp_grid/master_grid_clipped_2.5km.tif")

#import gridmet update function
source(paste0(git.dir,"gridmet_update_fun.R"))

#first fill in any missing data
suppressWarnings(update_gridMET(climatology.dir, git.dir, master_grid))

# now we will delete the last 4 months of data from the climatology.dir and re-fill
# this is a nieve way of updating gridmet data once data is updated from provisional 
# to perminant. From gridMET docs: 

# "At first, assets are ingested with status='early'. After several days, they are 
# replaced by assets with status='provisional'. After about 2 months, they are replaced 
# by the final assets with status='permanent'."

#import file names
files_updated = list.files(climatology.dir, pattern = ".tif$", full.names = T)

#index files to delete (4 months = 120 days)
files_to_remove = files_updated[(length(files_updated)-119):length(files_updated)]

#remove files
file.remove(files_to_remove)

#fill in dataset with up to date data
suppressWarnings(update_gridMET(climatology.dir, git.dir, master_grid))
