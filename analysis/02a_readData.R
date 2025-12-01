rm(list=ls())
library(oce)
library(sp) # for point.in.polygon
source('00_setupFile.R')
# define and read in file that documents mission names
arcFile <- 'arcList.dat'

arcMissions <- read.table(arcFile, header = TRUE,
                          stringsAsFactors = FALSE)
# get filenames
files <- NULL
for(i in 1:dim(arcMissions)[1]){
  cat(paste("Getting files for mission", arcMissions[['mission']][i], "year", arcMissions[['year']][i]), sep = '\n')
  path <- paste(arcPath, arcMissions[['year']][i], sep = '/')
  arcfiles <- as.vector(unlist(apply(arcMissions[i,], 1, function(k) list.files(path = path,
                                                                                pattern = paste0('^CTD_', k[['mission']],'.*DN\\.ODF'),
                                                                                full.names = TRUE))))
  # check for bad files defined in 00_setup.R
  bf <- arcfiles %in% badfiles
  if(is.null(files)){
    files <- arcfiles[!bf]
  } else {
    files <- c(files, arcfiles[!bf])
  }
}

# read in ctd data
ctd <- lapply(files, read.ctd.odf)
# trim the data
ctd <- lapply(ctd, ctdTrim)
# handle flags, if there are any
ctd <- lapply(ctd, function(k) if(length(k[['flags']]) != 0){handleFlags(k, flags = 2:4)} else {k})
# save data
save(ctd, file = paste(destDirData, 'ctd.rda', sep = '/'))