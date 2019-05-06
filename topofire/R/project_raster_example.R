# reproject one MACA geotiff to match the NWS precip grids. 

work.dir <- "/mnt/ScratchDrive/data/holden_hoylman_shared/project_raster_example/"

source(paste0(work.dir, "ProjectRaster_function.R"))

input.file <- paste0(work.dir, "maca_tmin_2011-01-01.tif")
target.file <- paste0(work.dir, "nws_precip_mm_20170112.tif")

out.file <- paste0(work.dir, "test_warp_MACA_to_NWS.tif")

gdalProjRaster(input.file, target.file, out.file)
