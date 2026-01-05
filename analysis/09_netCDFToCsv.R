rm(list=ls())
library(ncdf4)
# set directory
indir <- './netCDFclimatology' # where files live
outdir <- './csvClimatology' # where files go
## check if outdir exists, if not create
if(!exists(outdir)) dir.create(outdir, recursive = TRUE)
# get list of all files
files <- list.files(path = indir,
                    pattern = '*.\\.nc$',
                    full.names = TRUE)
# iterate through files
for(f in files){
  # open file
  nc <- nc_open(f)
  # get variable data
  ncvars <- names(nc$var)
  # define output variable
  variableData <- NULL
  # iterate through each variable
  for(iv in 1:length(ncvars)){
    var <- ncvars[iv]
    varinfo <- nc$var[[which(ncvars == var)]]
    # get variable dimensions and names
    vardim <- unlist(lapply(varinfo$dim, '[[', 'len'))
    vardimname <- unlist(lapply(varinfo$dim, '[[', 'name'))
    ## check that dimension is a dimension and that it's been created
    vardimcheck <- unlist(lapply(varinfo$dim, function(k) {k$dimvarid$isdimvar & k$create_dimvar}))
    ## retain valid dimensions
    vardim <- vardim[vardimcheck]
    vardimname <- vardimname[vardimcheck]
    ## extract dimension data
    dimnames <- lapply(vardimname, function(k) ncvar_get(nc = nc, varid = k))
    names(dimnames) <- vardimname
    # extract data
    vardata <- array(data = ncvar_get(nc = nc,
                                      varid = var),
                     dim = vardim,
                     dimnames = dimnames)
    # convert it to a data.frame
    vardatadf <- as.data.frame.table(x = vardata,
                                     responseName = var)
    ## remove rows where data value is NA
    isna <- is.na(vardatadf[[var]])
    vardatadf <- vardatadf[!isna, ]
    # merge with previous iteration
    if(iv == 1){
      variableData <- vardatadf
    } else {
      # get names from variableData and vardatadf
      dfvarnames <- c(names(variableData), names(vardatadf))
      # get variables to merge by, which is going to be any name that is repeated
      dfnametab <- table(unlist(dfvarnames))
      dfnametab <- dfnametab[order(dfnametab, decreasing = TRUE)] # order from most common to least
      mergevars <- names(dfnametab[which(dfnametab > 1)])
      # merge
      out <- merge(x = variableData, y = vardatadf, by = mergevars, all = TRUE)
      # re-name merged data frame 'out' to be used in next iteration
      variableData <- out
    }
  }
  # close the netCDF file
  nc_close(nc)
  # output data to .csv
  ## get filename
  filename <- basename(f)
  ### replace extension with '.csv'
  filename <- gsub('(.*\\.)nc', '\\1csv', filename)
  write.csv(x = variableData,
            file = paste(outdir, filename, sep = '/'),
            row.names = FALSE)
}
