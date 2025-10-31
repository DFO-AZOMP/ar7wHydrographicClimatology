rm(list=ls())
library(oce)
library(csasAtlPhys)
library(ncdf4)
library(sp) # for point.in.polygon
source('00_setupFile.R')
library(ocedata)
data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'bisque2'
load('ar7wPolygon.rda')
polygons <- list(ar7w = ar7wPolygon) # re-name for ease
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease
lonlim <- range(unlist(lapply(stations, '[[', 'longitude')))
latlim <- range(unlist(lapply(stations, '[[', 'latitude')))

files <- list.files(path = destDirData,
                    pattern = '.*\\.nc',
                    full.names = TRUE)
ctd <- NULL
for(file in files){
  nc <- nc_open(filename = file)
  # nothing useful in global attributes (ncatt_get(nc, 0))
  # get all data
  ncdimvars <- names(nc$dim)
  ncvars <- names(nc$var)
  ncallvars <- c(ncdimvars, ncvars)
  av <- vector(mode = 'list', length = length(ncallvars))
  for(ia in 1:length(av)){
    lookvar <- ncallvars[ia]
    # dimension variable
    isdimvar <- any(lookvar %in% ncdimvars)
    if(isdimvar) cat(paste(paste0("'", lookvar, "'"),
                           'is a dim var'), sep = '\n')
    # variable
    isvar <- any(lookvar %in% ncvars)
    if(isvar) cat(paste(paste0("'", lookvar, "'"),
                        'is a var'), sep = '\n')
    # neither, no data for that variable
    if(!isdimvar & !isvar) cat(paste(paste0("'", lookvar, "'"),
                                     'variable does not exist in file.'), sep = '\n')
    # extract data
    if(isdimvar){ # if it's a dim variable, this is easy, just extract it
      ncvar <- lookvar
      ncdimidx <- which(ncdimvars == ncvar)
      av[[ia]][['variableName']] <- lookvar
      av[[ia]][['data']] <- nc[['dim']][[ncdimidx]][['vals']]
      av[[ia]][['variableType']] <- 'dimension'
      # get units for later use
      av[[ia]][['units']] <- nc[['dim']][[ncdimidx]][['units']]
      cat(paste('Units of dimension', ncvar, 'are', nc[['dim']][[ncdimidx]][['units']]), sep = '\n')
    } else if(isvar){ # if it's a variable, have to extract it in it's dimensions
      ncvar <- lookvar
      var <- nc$var[[which(ncvars == ncvar)]]
      # get dimension and dimension names
      vardim <- unlist(lapply(var$dim, function(k) k[['len']]))
      vardimname <- lapply(var$dim, function(k) k[['name']])
      # find which dims has length one,
      # what to do with vars that have dimension length of 1 ? metadata ?
      dimlenone <- vardim == 1
      if(length(which(dimlenone)) != 0){
        cat(paste('Found', length(which(dimlenone)), 'dimensions with length 1'), sep = '\n')
        cat(paste('Excluding these in final array, the names of these dimensions are', paste(vardimname[dimlenone], collapse = ', ')), sep = '\n')
        vardim <- vardim[!dimlenone]
        vardimname <- vardimname[!dimlenone]
      }
      if(length(ncvar) != 0 & length(vardim) != 0){ # variable found with more than one dimension, and it's not a dimvar
        # not sure if I should retain the lon, lat dimension since it's usually 1,1 ?
        cat(paste('Found data for', lookvar, 'as variable', ncvar), sep = '\n')
        cat(paste(ncvar, 'has units', var[['units']]), sep = '\n')
        # want units as m/s
        if(var[['units']] == "cm/s"){
          unitadj <- 1/100 # 1m = 100cm
        } else {
          unitadj <- 1
        }
        av[[ia]][['variableName']] <- lookvar
        av[[ia]][['data']] <- array(data = ncvar_get(nc, ncvar),
                                    dim = vardim)
        if(!is.character(av[[ia]][['data']])) av[[ia]][['data']] <- av[[ia]][['data']] * unitadj
        av[[ia]][['variableType']] <- 'data'
        av[[ia]][['dimnames']] <- unlist(vardimname)
        av[[ia]][['units']] <- var[['units']]
      } else if(length(ncvar) != 0 & length(vardim) == 0 & !isdimvar){ # variable found with one dimension, and it's not a dimvar
        cat(paste('Dimension for variable', lookvar, 'is of length one.'), sep = '\n')
        av[[ia]][['data']] <- array(data = ncvar_get(nc, ncvar),
                                    dim = 1)
        av[[ia]][['variableType']] <- 'data'
        av[[ia]][['units']] <- var[['units']]
      } else {
        cat(paste('No data found for', paste0("'", lookvar, "'")), sep = '\n')
        av[[ia]][['data']] <- NULL
        av[[ia]][['variableType']] <- 'data'
      }
    } else {
      ncvar <- NULL
      cat(paste('No data found for', paste0("'", lookvar, "'")), sep = '\n')
      av[[ia]][['data']] <- NULL
      av[[ia]][['variableType']] <- 'data'
    }
  }
  nc_close(nc)
  # make a matrix of 'ctd' variables, not including the 'qc' variables
  ctdvars <- ncvars[grepl('^ctd', ncvars) & !grepl('^ctd.*\\_qc', ncvars)]
  avvars <- unlist(lapply(av, '[[', 'variableName'))
  nrow <- length(av[[which(avvars == ctdvars[1])]][['data']])
  ctddata <- ctdflags <- matrix(data = NA, nrow = nrow, ncol = length(ctdvars))
  ctdunits <- NULL
  for(iv in 1:length(ctdvars)){
    okavvars <- which(avvars == ctdvars[iv])
    ctddata[,iv] <- av[[okavvars]][['data']]
    ctdunits <- c(ctdunits, av[[okavvars]][['units']])
  }
  colnames(ctddata) <- ctdvars
  # get ctd qc variables
  ctdqcvars <- ncvars[grepl('^ctd.*\\_qc', ncvars)]
  ctdflags <- matrix(data = NA, nrow = nrow, ncol = length(ctdqcvars))
  for(iv in 1:length(ctdvars)){
    okavvars <- which(avvars == ctdqcvars[iv])
    ctdflags[,iv] <- av[[okavvars]][['data']]
  }
  colnames(ctdflags) <- ctdqcvars
  # look for variables that aren't all NA, this will filter out the temperature and oxygen vars.
  omitvars <- apply(ctddata, 2, function(k) all(is.na(k)))
  keepvarnames <- colnames(ctddata)[!omitvars] # for flags
  ctddata <- as.data.frame(ctddata[, !omitvars]) # to help retain str below
  ctdunits <- as.data.frame(ctdunits[!omitvars])
  keepflags <- unlist(lapply(keepvarnames, function(k) which(grepl(pattern = paste(k, 'qc', sep = '_'), colnames(ctdflags)))))
  ctdflags <- as.data.frame(ctdflags[, keepflags]) # for easier conversion to list below
  # add relevant metadata
  ## i'm not sure what the 'profile_id' and 'expocode' dimensions mean
  ##  retain only one row of both
  ##  and denote expocode as cruiseNumber (OR cruise)
  profileId <- av[[which(ncallvars == 'profile_id')]][['data']][1, ]
  cruiseNumber <- av[[which(ncallvars == 'expocode')]][['data']][1,]
  ## easy ones
  metavars <- c('time', 'pressure', 'latitude', 'longitude')
  metadata <- matrix(data = NA, nrow = nrow, ncol = length(metavars))
  for(iv in 1:length(metavars)){
    okavvars <- which(avvars == metavars[iv])
    metadata[,iv] <- av[[okavvars]][['data']]
  }
  colnames(metadata) <- metavars
  metadata <- as.data.frame(metadata)
  ctddata <- cbind(ctddata, metadata, profileId = profileId, cruise = cruiseNumber)
  ctdflags <- cbind(ctdflags, metadata)
  # remove 'ctd' from names for easy use of as.ctd
  names(ctddata) <- gsub('^ctd\\_(.*)', '\\1', names(ctddata))
  # remove 'ctd' and 'qc' from ctdflags
  names(ctdflags) <- gsub('^ctd\\_(.*)\\_qc', '\\1', names(ctdflags))
  # split by unique lon/lat
  ## get lon and lat as a data.frame
  lonlatdf <- metadata[, names(metadata) %in% c('longitude', 'latitude')]
  ulonlatdf <- unique(lonlatdf)
  ## add castNumber
  ulonlatdf <- data.frame(ulonlatdf,
                          index = 1:dim(ulonlatdf)[1])
  ## merge with ctddata and ctdflags
  ctddata <- merge(ctddata, ulonlatdf, by = c('longitude', 'latitude'),
                   all.x = TRUE)
  ctdflags <- merge(ctdflags, ulonlatdf, by = c('longitude', 'latitude'),
                    all.x = TRUE)
  # split by index
  ctddatas <- split(ctddata, ctddata[['index']])
  ctdflagss <- split(ctdflags, ctdflags[['index']])
  uindex <- names(ctddatas)
  ctdout <- vector(mode = 'list', length = length(uindex))
  # define flagScheme... unable to pull it from a qc variable, I don't know why
  flagScheme <- list("no_flag_assigned" = 0,
                     "not_calibrated" = 1,
                     "acceptable_measurement" = 2,
                     "questionable_measurement" = 3,
                     "bad_measurement" = 4,
                     "not_reported" = 5,
                     "interpolated_over_a_pressure_interval_larger_than_2_dbar" = 6,
                     "despiked" = 7,
                     "not_sampled" = 9)
  for(ip in 1:length(uindex)){
    lookid <- uindex[ip]
    okdata <- which(names(ctddatas) == lookid)
    okflags <- which(names(ctdflagss) == lookid)
    # remove profileId from flags
    addflags <- ctdflagss[[okflags]]
    addflags <- as.list(addflags[, !names(addflags) %in% c(names(metadata), 'index')])
    # create ctd
    ctdadd <- as.ctd(ctddatas[[okdata]],
                     flags = addflags)
    # add flag scheme
    ctdadd <- initializeFlagScheme(object = ctdadd,
                                   name = 'WOCECTD',
                                   mapping = flagScheme)
    ctdout[[ip]] <- ctdadd
  }
  lon <- unlist(lapply(ctdout, function(k) k[['longitude']][1]))
  lat <- unlist(lapply(ctdout, function(k) k[['latitude']][1]))
  par(mar = c(3.5, 3.5, 1, 1))
  mapPlot(coastlineWorldFine,
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol,
          proj = proj,
          grid = c(2,1))
  mapPoints(lon, lat, pch = 20, col = 'black')
  lapply(polygons, function(k) mapPolygon(k[['longitude']], k[['latitude']], border = 'red'))
  ctd <- c(ctd, ctdout)
}
# add cruiseNumber
ctd <- lapply(ctd, function(k) oceSetMetadata(k, 'cruiseNumber', k[['cruise']][1]))
# add startTime
ctd <- lapply(ctd, function(k) oceSetMetadata(k, 'startTime', as.POSIXct(k[['time']][[1]], origin = '1970-01-01', tz = 'UTC')))
# change time to POSIX
ctd <- lapply(ctd, function(k) oceSetData(k, 'time', as.POSIXct(k[['time']], origin = '1970-01-01', tz = 'UTC')))
# handle flags
ctd <- lapply(ctd, handleFlags, flags = list(3:4))
# save data
ctdgo <- ctd
save(ctdgo, file = paste(destDirData, 'ctdgo.rda', sep = '/'))
