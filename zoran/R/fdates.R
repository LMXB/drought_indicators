

fdates  = function(filenames,dateformat=F,n=NULL){
  # this function extracts dates from filenames that contain a date in YYYYMMDD format. 
  # the date portion of the filename must be separated by underscores from the rest of the 
  # filename.  the function searches each filename for numeric elements with 8 digits.  
  # the filenane extension is automatically removed.  
  #
  # the 'filenames' argument is any vector of filenames
  # 'dateformat' causes the returned dates to be in R date format
  # 'n' overrides the automatic search and instead returns the nth element separated by underscores.
  if(length(filenames)==0) {cat("filenames supplied have length 0\n");return(NULL)}
  
  # strip extension ~~~~~~~~~~~~~~~~~~~~
  filenames <- sapply(strsplit(basename(filenames),"\\."), function(x) paste(x[1:(length(x)-1)],collapse="."))
  if(is.null(n)){
    dts<-sapply(strsplit(filenames,"_"),function(x) {y<-x[!is.na(suppressWarnings(as.numeric(x))) & nchar(x)==8];if(length(y)==1) y else NA})
  } else  dts<-sapply(strsplit(basename(filenames),"_"),function(x) x[n])
  if(dateformat) dts <- as.Date(dts,"%Y%m%d")
  dts
}


