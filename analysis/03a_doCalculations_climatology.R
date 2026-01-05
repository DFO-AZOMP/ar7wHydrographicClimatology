rm(list=ls())
library(oce)
data("ctd")
ghostctd <- ctd
plotProfile <- oce::plotProfile
topoFile <- download.topo(west = -70, east = -40,
                          south = 38, north = 65,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
# for rounding to get stations spacing
mround <- function(x,base, type = 'round'){
  if(type == 'round'){
    return(base*round(x/base))
  }
  if(type == 'ceiling'){
    return(base*ceiling(x/base))
  }
}
source('00_setupFile.R')
# load station polygons
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease
# load transectDefinition
load("ar7wTransectDefinition.rda")
transectDefinitions <- list(ar7w = ar7wTransectDefinition)
# load data
load(paste(destDirData, 'ctdFiltered.rda', sep = '/'))
####
# define minimum number of profiles
####
minN <- 10
####
# define depth bins
####
## 10m
# bin <- seq(10, 4000, 10)
# tolerance <- rep(5, length(bin))
## woa
bin <- list(seq(15, 100, 5),
         seq(125, 500, 25),
         seq(550, 2000, 50),
         seq(2100, 4000, 100))
tolerance <- lapply(bin, function(k) {d <- diff(k)/2; c(d[1], d)})
bin <- do.call('c', bin)
tolerance <- do.call('c', tolerance)
transectDepthBins <- data.frame(bin = bin,
                                tolerance = tolerance)
####
# subset to only core stations, aka not 'half' stations ?
# decision : keep all stations, implement min number of occupations (see above)
####
# stnName <- lapply(stations, '[[', 'stationName')
# stnNumber <- lapply(stnName, function(k) as.numeric(gsub('\\w+_(\\w+)', '\\1', k)))
# iscore <- unlist(lapply(stnNumber, function(k) k == round(k)))
# stations <- stations[iscore]

####
# define some values for smoothing the section
####
# set xr based on region
ar7wStns <- unlist(lapply(ar7wStationPolygons, '[[', 'stationName'))
ar7wLon <- unlist(lapply(ar7wStationPolygons, '[[', 'longitude'))
ar7wLat <- unlist(lapply(ar7wStationPolygons, '[[', 'latitude'))
xradd <- NULL
for(rs in 1:length(regionStations)){
  ok <- ar7wStns %in% regionStations[[rs]]
  regdist <- geodDist(longitude1 = ar7wLon[ok],
                      latitude1 = ar7wLat[ok])
  regxr <- mround(median(diff(regdist)), 5, type = 'ceiling') * 2.0
  xradd <- c(xradd, regxr)
}
dfxr <- data.frame(region = names(regionStations),
                   xr = xradd)
# set yr based on region
yradd <- rep(50, dim(dfxr)[1])
yradd[dfxr[['region']] %in% c('labradorShelf', 'greenlandShelf')] <- 15
dfsmoothparam <- data.frame(dfxr,
                            yr = yradd)

# get some important meta for knowing which profile belongs to which
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startYear <- as.POSIXlt(startTime)$year + 1900
startMonth <- as.POSIXlt(startTime)$mon + 1
transects <- unlist(lapply(ctd, function(k) k[['transect']]))
dataType <- unlist(lapply(ctd, function(k) k[['dataType']]))
station <- unlist(lapply(ctd, function(k) k[['stationName']]))

df <- data.frame(transect = transects,
                 station = station,
                 dataType = dataType,
                 startTime = startTime,
                 year = startYear,
                 month = startMonth)
# remove instances where no transect was detected
df <- df[!is.na(df[['transect']]), ]
df <- df[df[['transect']] != FALSE, ]
# remove instances where no station was detected
df <- df[df[['station']] != FALSE, ]
###
# 'season'
###
# only want data collected between May 01 to July 01, so we'll look for data in months 5 and 6
df <- df[df[['month']] %in% c(5, 6), ]
# create lookup table for climatology calculations
coreTransects <- c('ar7w')
lookdf <- data.frame(transect = coreTransects)

start <- c(1991)
end <- c(2020)
lookdfnew <- NULL
for(ic in 1:length(start)){
  for(i in 1:dim(lookdf)[1]){
    if(dim(lookdf)[1] == 1){
      add <- lookdf
    } else {
      add <- lookdf[i, ]
    }
    newdf <- data.frame(add,
                        start = start[ic],
                        end = end[ic])
    if(is.null(lookdfnew)){
      lookdfnew <- newdf
    } else {
      lookdfnew <- rbind(lookdfnew, newdf)
    }
  }
}
lookdf <- lookdfnew

dfStationNames <- data.frame(transect = unlist(lapply(stations, function(k) k[['transectName']])),
                             station = unlist(lapply(stations, function(k) k[['stationName']])))
climatology <- vector(mode = 'list', length = dim(lookdf)[1])
for(it in 1:dim(lookdf)[1]){
  transectlook <- lookdf[['transect']][it]
  climatologyYears <- lookdf[['start']][it]:lookdf[['end']][it]
  ok <- df[['transect']] == transectlook & df[['year']] %in% climatologyYears
  d <- df[ok, ]
  transectStations <- dfStationNames[dfStationNames[['transect']] %in% transectlook ,]
  cat(paste('Calculating a climatology for', transectlook,
            'for climatology period', paste(range(climatologyYears), collapse = ' to ')),
      sep = '\n')
  # some plot output, commenting out for now
  # pdf(file = paste(destDirSuppFigures, paste0(paste('transectProfiles',
  #                                             transectlook,
  #                                             programlook,
  #                                             seasonlook,
  #                                             paste(min(climatologyYears), max(climatologyYears), sep = 'to'), sep = '_'),
  #                                             '.pdf'), sep = '/'))
  tranCtd <- vector(mode = 'list')
  origCtd <- vector(mode = 'list', length = dim(transectStations)[1])
  names(origCtd) <- transectStations[['station']]
  missingStn <- missingStnClimMinYear <- NULL
  cnt <- 1
  for(ts in 1:dim(transectStations)[1]){
    stationlook <- transectStations[['station']][ts]
    okstn <- d[['station']] %in% stationlook
    dd <- d[okstn, ]
    # the row names of the data frame will give us the index of which ctd it is
    idx <- as.numeric(rownames(dd))
    stnctd <- ctd[idx]
    origCtd[[ts]] <- stnctd
    if(length(unique(dd[['year']])) < minN){
      cat(paste('      Unable to calculate climatology for station', stationlook, 'due to insufficient number of profiles'), sep = '\n')
      missingStn <- c(missingStn, stationlook)
      missingStnClimMinYear <- c(missingStnClimMinYear, lookdf[['start']][it])
    } else {
      stnStartTime <- as.POSIXct(unlist(lapply(stnctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
      pressure <- unlist(lapply(stnctd, function(k) k[['pressure']])) # want it separate from the data for averaging
      stndata <- data.frame(temperature = unlist(lapply(stnctd, function(k) k[['temperature']])),
                            salinity = unlist(lapply(stnctd, function(k) k[['salinity']])),
                            sigmaTheta = unlist(lapply(stnctd, function(k) k[['sigmaTheta']])))
      # might have to figure out something smarter to subset the to max depth bin for each station.
      # update : added the tolerance to get the lower bound of the bins
      # update : use the median max depth to omit using profiles that are deeper than most
      medianMaxDepth <- median(unlist(lapply(stnctd, function(k) max(k[['pressure']]))))
      maxDepthBinIdx <- which.min(abs(medianMaxDepth - (transectDepthBins[['bin']] + transectDepthBins[['tolerance']])))
      stnDepthBins <- transectDepthBins[1:maxDepthBinIdx, ]
      avgProfile <- apply(stndata, 2, function(k) mapply(function(bin, tolerance) mean(k[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                                                         stnDepthBins$bin,
                                                         stnDepthBins$tolerance))
      sdProfile <- apply(stndata, 2, function(k) mapply(function(bin, tolerance) sd(k[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                                                        stnDepthBins$bin,
                                                        stnDepthBins$tolerance))
      avgProfile <- as.data.frame(avgProfile)
      sdProfile <- as.data.frame(sdProfile)
      Tlim <- range(stndata[['temperature']], na.rm = TRUE)
      Slim <- range(stndata[['salinity']], na.rm = TRUE)
      STlim <- range(stndata[['sigmaTheta']], na.rm = TRUE)
      plim <- rev(range(pressure, na.rm = TRUE))
      par(mfrow=c(2,2), oma = c(0,0,1,0))

      plotProfile(ghostctd, xtype = 'temperature', Tlim = Tlim, plim = plim, col = 'white')
      polygon(c(avgProfile[['temperature']] - sdProfile[['temperature']],
                rev(avgProfile[['temperature']] + sdProfile[['temperature']])),
              c(stnDepthBins[['bin']],
                rev(stnDepthBins[['bin']])),
              border = NA, col = 'grey')
      lapply(stnctd, function(k) lines(k[['temperature']], k[['pressure']]))
      lines(avgProfile[['temperature']], stnDepthBins[['bin']], col = 'red', lwd = 2)
      # add station name
      mtext(stationlook, outer = TRUE)
      plotProfile(ghostctd, xtype = 'salinity', Slim = Slim, plim = plim, col = 'white')
      polygon(c(avgProfile[['salinity']] - sdProfile[['salinity']],
                rev(avgProfile[['salinity']] + sdProfile[['salinity']])),
              c(stnDepthBins[['bin']],
                rev(stnDepthBins[['bin']])),
              border = NA, col = 'grey')
      lapply(stnctd, function(k) lines(k[['salinity']], k[['pressure']]))
      lines(avgProfile[['salinity']], stnDepthBins[['bin']], col = 'red', lwd = 2)

      plotProfile(ghostctd, xtype = 'sigmaTheta', densitylim = STlim, plim = plim, col = 'white')
      polygon(c(avgProfile[['sigmaTheta']] - sdProfile[['sigmaTheta']],
                rev(avgProfile[['sigmaTheta']] + sdProfile[['sigmaTheta']])),
              c(stnDepthBins[['bin']],
                rev(stnDepthBins[['bin']])),
              border = NA, col = 'grey')
      lapply(stnctd, function(k) lines(k[['sigmaTheta']], k[['pressure']]))
      lines(avgProfile[['sigmaTheta']], stnDepthBins[['bin']], col = 'red', lwd = 2)

      plotTS(ghostctd, Tlim = Tlim, Slim = Slim, col = 'white')
      # polygon in T-S space is kind of weird.
      polygon(c(avgProfile[['salinity']] - sdProfile[['salinity']],
                rev(avgProfile[['salinity']] + sdProfile[['salinity']])),
              c(avgProfile[['temperature']] - sdProfile[['temperature']],
                rev(avgProfile[['temperature']] + sdProfile[['temperature']])),
              border = NA, col = 'grey')
      lapply(stnctd, function(k) points(k[['salinity']], k[['temperature']]))
      lines(avgProfile[['salinity']], avgProfile[['temperature']], col = 'red', lwd = 2)

      # make each average profile a ctd object
      fakeMonth <- '05'
      okstation <- which(unlist(lapply(stations, function(k) k[['stationName']])) == stationlook)
      stnmeta <- stations[[okstation[1]]]
      tranCtd[[cnt]] <- as.ctd(salinity = avgProfile[['salinity']],
                               temperature = avgProfile[['temperature']],
                               pressure = stnDepthBins[['bin']],
                               startTime = paste(max(climatologyYears), fakeMonth, 15, sep = '-'),
                               longitude = stnmeta[['longitude']],
                               latitude = stnmeta[['latitude']])
      # have to set sigmaTheta based on averaging
      tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                   'sigmaThetaAvg', # have to use 'sigmaThetaAvg' since oce will internally calculate 'sigmaTheta'
                                   avgProfile[['sigmaTheta']],
                                   unit = expression(kg/m^3))
      # add standard deviation
      tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                   'temperatureSd',
                                   sdProfile[['temperature']])
      tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                   'salinitySd',
                                   sdProfile[['salinity']])
      tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                   'sigmaThetaAvgSd',
                                   sdProfile[['sigmaTheta']])
      # add some metadata
      tranCtd[[cnt]] <- oceSetMetadata(tranCtd[[cnt]],
                                       'stationName',
                                       stnmeta[['stationName']])
      tranCtd[[cnt]] <- oceSetMetadata(tranCtd[[cnt]],
                                       'transect',
                                       stnmeta[['transectName']])
      cnt <- cnt + 1
    }
  }
  #dev.off()
  if(length(tranCtd) > dim(transectStations)[1]/2){ # require that there be more than half of the stations present
    # order the stations
    ddlon <- unlist(lapply(tranCtd, function(k) k[['longitude']]))
    ddlat <- unlist(lapply(tranCtd, function(k) k[['latitude']]))
    o <- order(ddlon)
    tranCtd <- tranCtd[o]
    # define values for smoothing section (need it for creating fake profile)
    factor <- 2.0 # guess
    distance <- geodDist(longitude1 = ddlon[o],
                         latitude1 = ddlat[o],
                         longitude2 = ddlon[o][1],
                         latitude2 = ddlat[o][1])
    xr <- mround(median(diff(distance))/2, 5, type = 'ceiling') * factor
    yr <- 10
    # add fake station at the end for nicer interpolation
    a <- getTransectAngle(longitude = unlist(lapply(tranCtd[(length(tranCtd)-1):length(tranCtd)], function(k) k[['longitude']][1])),
                          latitude = unlist(lapply(tranCtd[(length(tranCtd)-1):length(tranCtd)], function(k) k[['latitude']][1])))
    angle <- a$angle
    eastingadd <- ((xr/3) * 1000) * cos(angle * pi/180) # not sure of factor in front of xr, changed xr to fixed distance, xr/2
    northingadd <- ((xr/3) * 1000) * sin(angle * pi/180) # not sure of factor in front of xr, changed xr to fixed distance, xr/2
    faked <- tranCtd[[length(tranCtd)]]
    zone <- lonlat2utm(longitude = faked[['longitude']][1],
                       latitude = faked[['latitude']][1])$zone
    utm <- lonlat2utm(longitude = faked[['longitude']][1],
                      latitude = faked[['latitude']], zone = zone)
    fakelonlat <- utm2lonlat(easting = utm$easting + eastingadd,
                             northing = utm$northing + northingadd,
                             zone = zone)
    faked[['longitude']] <- fakelonlat$longitude
    faked[['latitude']] <- fakelonlat$latitude
    faked[['stationName']] <- 'fakeStn'
    tranCtd <- c(tranCtd, faked)
    # # create section and barnes interpolate
    s <- as.section(tranCtd)
    # set pressure levels, minimum across dd and max
    allp <- unlist(lapply(tranCtd, '[[', 'pressure'))
    okmin <- which.min(abs(min(allp) - transectDepthBins[['bin']]))
    okmax <- which.min(abs(max(allp) - transectDepthBins[['bin']]))
    plevels <- transectDepthBins[['bin']][okmin:okmax]
    sg <- sectionGrid(s, p = plevels)
    ## do separate smoothed sections for each region
    if(transectlook == 'ar7w'){
      ss <- vector(mode = 'list', length = dim(dfsmoothparam)[1])
      for(ir in 1:dim(dfsmoothparam)[1]){
        lookparam <- dfsmoothparam[ir, ]
        xgrid <- seq(0, ceiling(max(sg[['distance', 'byStation']])) + xr, by = xr/2)
        ygrid <- seq(min(plevels), max(plevels), by = lookparam[['yr']])
        ss[[ir]] <- sectionSmooth(sg, method = 'barnes',
                                  xg = xgrid, yg = ygrid,
                                  xr = lookparam[['xr']], yr = lookparam[['yr']])
      }
      names(ss) <- dfsmoothparam[['region']]
    } else {
      # set parameters for interpolation
      xgrid <- seq(0, ceiling(max(sg[['distance', 'byStation']])) + xr, by = xr/2)
      # have to add the is.na part to not interpolate farther down than measurements
      ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr)
      ss <- sectionSmooth(sg, method = 'barnes', xg = xgrid, yg = ygrid, xr = xr, yr = yr)
    }
    # get some meta data things for plotting that was defined previously
    # get bottom polygon
    okdef <- which(names(transectDefinitions) %in% transectlook)
    longitude0 <- transectDefinitions[[okdef]][['info']][['start_longitude']]
    latitude0 <- transectDefinitions[[okdef]][['info']][['start_latitude']]
    ylim <- c(0, transectDefinitions[[okdef]][['info']][['yaxis_max']])
    # get xlim for section plots by using the defined stations
    okstations <- lapply(transectStations[['station']], function(kk) which(unlist(lapply(stations, function(k) k[['stationName']])) == kk))
    transectLongitude <- unlist(lapply(okstations, function(k) stations[[k[1]]][['longitude']]))
    transectLatitude <- unlist(lapply(okstations, function(k) stations[[k[1]]][['latitude']]))
    transectDistance <- geodDist(longitude1 = transectLongitude,
                                 latitude1 = transectLatitude,
                                 longitude2 = longitude0,
                                 latitude2 = latitude0)
    # create missing station data.frame
    if(is.null(missingStn)){
      missingDf <- NULL
    } else {
      missingDf <- data.frame(station = missingStn,
                              climYear = missingStnClimMinYear,
                              transect = transectlook)
    } # closes create missing station data.frame
    # save info
    climatology[[it]][['transect']] <- transectlook
    climatology[[it]][['longitude0']] <- longitude0
    climatology[[it]][['latitude0']] <- latitude0
    climatology[[it]][['ylim']] <- ylim
    climatology[[it]][['xlim']] <- c(0, max(transectDistance))
    climatology[[it]][['avgProfiles']] <- tranCtd[-length(tranCtd)]
    climatology[[it]][['section']] <- s
    climatology[[it]][['sectionGrid']] <- sg
    climatology[[it]][['sectionSmooth']] <- ss
    climatology[[it]][['climatologyYears']] <- climatologyYears
    climatology[[it]][['missingStations']] <- missingDf
    climatology[[it]][['originalStations']] <- origCtd
  }  else {# closes if length(tranCtd) != 0
    cat(paste('   Unable to create a climatology for', transectlook,
              'for climatology period', paste(range(climatologyYears), collapse = ' to '),
              'for', programlook), sep = '\n')
    missingDf <- data.frame(station = missingStn,
                            climYear = missingStnClimMinYear,
                            transect = transectlook)
    climatology[[it]][['transect']] <- transectlook
    climatology[[it]][['longitude0']] <- NA
    climatology[[it]][['latitude0']] <- NA
    climatology[[it]][['ylim']] <- NA
    climatology[[it]][['xlim']] <- NA
    climatology[[it]][['avgProfiles']] <- NA
    climatology[[it]][['section']] <- NA
    climatology[[it]][['sectionGrid']] <- NA
    climatology[[it]][['sectionSmooth']] <- NA
    climatology[[it]][['climatologyYears']] <- climatologyYears
    climatology[[it]][['missingStations']] <- missingDf
    climatology[[it]][['originalStations']] <- origCtd
  }
}

save(climatology, file = paste(destDirData, 'climatology.rda', sep = '/'))
