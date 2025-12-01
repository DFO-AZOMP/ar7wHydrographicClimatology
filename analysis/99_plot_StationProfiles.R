rm(list=ls())
# toggle colormapType
colormapType <- 'year'
#colormapType <- 'date'
library(oce)
data('ctd')
ghostctd <- ctd # for plotting
library(fields)
topoFile <- download.topo(west = -70, east = -40,
                          south = 38, north = 65,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')
# load nominal stations
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease
# load data
load(paste(destDirData, 'ctdFiltered.rda', sep = '/'))
# infer bottom depths at station locations
zs <- abs(interp.surface(obj = list(x = ocetopo[['longitude']],
                                y = ocetopo[['latitude']],
                                z = ocetopo[['z']]),
                     loc = cbind(unlist(lapply(stations, '[[', 'longitude')),
                                 unlist(lapply(stations, '[[', 'latitude')))))
# get stationName from ctd objects
stnctd <- unlist(lapply(ctd, '[[', 'stationName'))
# define breaks for colormap
## year
yearbreaks <- c(1991, seq(1995, 2020, 5))
## date
fakeYear <- 1990
timectd <- as.POSIXct(unlist(lapply(ctd, '[[', 'startTime')), origin = '1970-01-01', tz = 'UTC')
minMonth <- min(as.POSIXlt(timectd)$mon + 1)
maxMonth <- max(as.POSIXlt(timectd)$mon + 1)
fakeTimeBreaks <- as.POSIXct(paste(fakeYear, minMonth:(maxMonth+1), '01', sep = '-'), tz = 'UTC') # add 1 to max to get full
## now appoint based on toggle at beginning of script
breaks <- switch(colormapType,
                 'year'= yearbreaks,
                 'date' = fakeTimeBreaks)
paletteLabel <- switch(colormapType,
                        'year' = 'Year',
                        'date' = 'Date')
# define depth to subset
pbreak <- 400
# define vars to plot
vars <- c('temperature', 'salinity', 'sigmaTheta')
for(is in 1:length(stations)){
  stnlook <- stations[[is]]
  stnname <- stnlook[['stationName']]
  ok <- stnctd == stnname
  d <- ctd[ok]
  dtime <- as.POSIXct(unlist(lapply(d, '[[', 'startTime')), origin = '1970-01-01', tz = 'UTC')
  dyear <- as.POSIXlt(dtime)$year + 1900
  fakeTime <- as.POSIXct(paste(fakeYear, as.POSIXlt(dtime)$mon + 1, as.POSIXlt(dtime)$mday, sep = '-'), tz = 'UTC')
  # define what to enter into colormap
  cmz <- switch(colormapType,
                'year' = dyear,
                'date' = fakeTime)
  png(filename = paste(destDirSuppFigures,
                       paste0(paste('99_profilePlot',
                                    is,
                                    paste0('by', colormapType),
                                    sep = '_'),
                              '.png'),
                       sep = '/'),
      height = ifelse(zs[is] > pbreak, 7, 7*0.55), width = 7, units = 'in',
      pointsize = 8, res = 250)
  # set up plot window
  ## if depth is greater than 400, plot full depth and then top 400 m
  palwidth <- 0.15
  if (zs[is] > pbreak){
    # 3x2
    # mlay <- matrix(c(
    #                  1,1, 2,2, # temperature
    #                  3,3, 4, 4, # salinity
    #                  5,5, 6,6,# density
    #                  0,7,7, 0 # color palette
    #                  ),
    #                nrow = 4, ncol = 4, byrow = TRUE)
    # 2x3
    mlay <- rbind(t(matrix(c(1, 2, # temperature
                             3, 4, # salinity
                             5, 6 # density
                             ),
                           nrow = 3, ncol = 2, byrow = TRUE)),
                  matrix(c(0,7,0), nrow = 1))
  } else {
    # 3x1
    # mlay <- matrix(c(1,
    #                  2,
    #                  3,
    #                  4),
    #                nrow = 4, ncol = 1, byrow = TRUE)
    # 1x3
    mlay <- matrix(c(1, 2, 3,
                     0, 4, 0),
                   nrow = 2, byrow = TRUE)

  }
  #layout(mat = mlay, heights = c(rep(1-palwidth, 3), palwidth)) #3byx
  layout(mat = mlay, heights = c(rep(1-palwidth, ifelse(zs[is] > pbreak, 2, 1)), palwidth)) # xby3
  par(cex = 0.8) # for nice palette labels
  par(oma = c(0, 0, 1.5, 0)) # to label stationName
  # get some limits
  ## not sure if we should use limit from all data, or just the station
  ## for now, station
  lims <- lapply(vars, function(k) range(unlist(lapply(d, '[[', k)), na.rm = TRUE))
  names(lims) <- vars
  # define colormap
  cm <- colormap(z = cmz, breaks = breaks)
  for(var in vars){
    plim <- rev(range(unlist(lapply(d, '[[', 'pressure')), na.rm = TRUE))
    plotProfile(x = ghostctd,
                xtype = var,
                Tlim = lims[['temperature']],
                Slim = lims[['salinity']],
                densitylim = lims[['sigmaTheta']],
                plim = plim,
                col = 'white')
    for(id in 1:length(d)){
      dd <- d[[id]]
      lines(dd[[var]], dd[['pressure']], col = cm$zcol[id])
    }
    if(zs[is] > pbreak){
      plim[1] <- pbreak
      plotProfile(x = ghostctd,
                  xtype = var,
                  Tlim = lims[['temperature']],
                  Slim = lims[['salinity']],
                  densitylim = lims[['sigmaTheta']],
                  plim = plim,
                  col = 'white')
      for(id in 1:length(d)){
        dd <- d[[id]]
        lines(dd[[var]], dd[['pressure']], col = cm$zcol[id])
      }
    }
  }
  paletteAxisLabels <- switch(colormapType,
                              'year'= breaks,
                              'date' = format.POSIXct(breaks, format = '%b'))
  drawPalette(breaks = breaks, zlab = ' ', pos = 1, labels = paletteAxisLabels, at = breaks)
  mtext(text = paletteLabel, side = 3, line = -1.7)
  # label stationName
  mtext(text = stnname, side = 3, outer = TRUE)
  dev.off()
}

