rm(list=ls())
library(csasAtlPhys)
library(oce)
library(ocedata)
data(coastlineWorldFine)
topoFile <- download.topo(west = -70, east = -40,
                          south = 38, north = 65,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease

load(paste(destDirData, 'ctdFiltered.rda', sep = '/'))

# get some important meta for knowing which profile belongs to which
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startYear <- as.POSIXlt(startTime)$year + 1900
transects <- unlist(lapply(ctd, function(k) k[['transect']]))
dataType <- unlist(lapply(ctd, function(k) k[['dataType']]))
station <- unlist(lapply(ctd, function(k) k[['stationName']]))


df <- data.frame(transect = transects,
                 station = station,
                 dataType = dataType,
                 startTime = startTime,
                 year = startYear)
df <- df[df[['year']] %in% 1991:2020, ]
# remove instances where no transect was detected
df <- df[!is.na(df[['transect']]), ]
df <- df[df[['transect']] != FALSE, ]
# remove instances where a profile was not associated with a station
df <- df[df[['station']] != FALSE, ]

# split by transect, then station
# this is just for a visual check
# this helped identify that profiles that were not associated with a station needed to be omitted
#   from the data frame.
sdf <- lapply(split(df, df[['transect']]), function(k) split(k, k[['station']]))

# make a table by transect, and station
tbl <- lapply(split(df, df[['transect']]), function(k) {totable <- data.frame(station = k[['station']], year = factor(k[['year']], 1991:2020));
table(totable)})

# now plot number of stations per season, per transect, per station
fakeYear <- 2020
# some things for map
proj <- '+proj=merc'
fillcol <- 'bisque2'
lonlim <- range(unlist(lapply(stations, '[[', 'longitude')))
latlim <- range(unlist(lapply(stations, '[[', 'latitude')))
mapmar <- c(1.25, 10, 0, 10)
plotmar <- c(1.5, 1, 1.5, 0)
mapmar <- plotmar
palettemar <- c(3.5, 0, 1, 6)
plotmar <- c(3.5, 3.5, 1, 0)
palettemar <- c(3.5, 0, 1, 0)
mapmar <- c(3, 3, 1, 1)
for(i in 1:length(tbl)){
  png(paste(destDirFigures,
            paste0(paste('09_stationMap',
                         names(tbl)[i],
                         sep = '_'),
                   '.png'),
            sep = '/'),
      width = 5, height = 6, units = 'in', # portrait
      #width = 8, height = 4.5, units = 'in', # landscape
      pointsize = 9, res = 250)
  # layout for plot
  # palwidth <- 0.23
  # mlay <- matrix(data = c(1, 1, 2, 2, 0,
  #                         1, 1, 3, 3, 4),
  #                nrow = 2, byrow = TRUE)
  # mlay <- matrix(data = c(2, 2, 0, 1, 1,
  #                         3, 3, 4, 1, 1),
  #                nrow = 2, byrow = TRUE)
  # layout(mat = mlay,
  #        #widths = c(1-palwidth, 1-palwidth, 1-palwidth, 1-palwidth, palwidth))
  #        widths = c(1-palwidth, 1-palwidth, palwidth, 1-palwidth, 1-palwidth))
  # oma <- c(1.5, 4, 1, 1)
  # par(cex = 0.8) # for nice labels in palette
  # par(oma = oma)
  # map
  # plot transect stations with labels
  transect <- names(tbl)[i]
  stnlab <- unlist(lapply(stations, '[[', 'stationName'))
  stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', stnlab))
  o <- order(stnnum)
  stnlab <- stnlab[o]
  stnlon <- unlist(lapply(stations, '[[', 'longitude'))[o]
  stnlat <- unlist(lapply(stations, '[[', 'latitude'))[o]
  ## set limits based on stn coordinates
  tranlonlim <- range(stnlon) + c(-0.5, 0.5)
  if(diff(tranlonlim) < 1.5) tranlonlim <- tranlonlim + c(-0.5, 0.5)
  tranlatlim <- range(stnlat) + c(-0.5, 0.5)
  if(diff(tranlatlim) < 1.5) tranlatlim <- tranlatlim + c(-0.5, 0.5)
  ## increase limits if tight
  par(mar = mapmar)
  mapPlot(coastlineWorldFine,
          longitudelim = tranlonlim,
          latitudelim = tranlatlim,
          col = fillcol,
          proj = proj,
          grid = TRUE)
  bathylevels <- c(-3000, -2000, -1000, -200)
  bathycol <- 'lightgrey'
  mapContour(longitude = ocetopo[['longitude']],
             latitude = ocetopo[['latitude']],
             z = ocetopo[['z']],
             levels = bathylevels,
             lwd = 0.8, col = bathycol)
  ## plot transect stations with labels
  mapPoints(longitude = stnlon, latitude = stnlat, pch = 20)
  ### get indicies for labelling on left and right side of point
  labelidx <- 1:length(stnlab)
  okidx <- labelidx %in% seq(1, max(labelidx), 2)
  ### right side
  mapText(longitude = stnlon[okidx],
          latitude = stnlat[okidx],
          labels = stnlab[okidx],
          pos = 4,
          cex = 4/5)
  ### left side
  mapText(longitude = stnlon[!okidx],
          latitude = stnlat[!okidx],
          labels = stnlab[!okidx],
          pos = 2,
          cex = 4/5)
  dev.off()
  png(paste(destDirFigures,
            paste0(paste('09_samplingFrequency',
                         names(tbl)[i],
                         sep = '_'),
                   '.png'),
            sep = '/'),
      width = 6, height = 6, units = 'in', # portrait
      #width = 8, height = 4.5, units = 'in', # landscape
      pointsize = 8, res = 250)
  par(oma = c(0, 3, 0, 1.5))
  d <- tbl[[i]]
  dstn <- sdf[[which(names(sdf) == names(tbl)[i])]]
  dd <- t(d)
  stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', colnames(dd)))
  o <- order(stnnum)
  dd <- dd[, o]
  cm <- colormap(z = dd, breaks = c(0, 0.9, 1.9, 2), col = hcl.colors(n=5, palette = 'Red-Green')[c(2, 4, 5)])
  x <- as.numeric(rownames(dd))
  y <- 1:dim(dd)[2]
  par(mar = plotmar)
  imagep(x = x,
         y = y,
         z = dd,
         colormap = cm,
         drawPalette = FALSE,
         axes = FALSE,
         mar = plotmar)
  textlab <- expand.grid(x = x,
                         y = y)
  text(x = textlab$x,
       y = textlab$y,
       labels = as.vector(dd),
       col = ifelse(as.vector(dd) > 1, 'white', 'black'),
       cex = 4/5)
  # get total number of profiles for 1991 to 2020 climatology
  ddtotyear <- dd
  ddtotyear[ddtotyear > 1] <- 1
  climtotal <- apply(ddtotyear[x %in% 1991:2020, ], 2, sum)
  mtext(text = paste(climtotal, sep = ','), side = 4, at = y, las = 1, line = 0.25)
  box()
  abline(h = y + 0.5)
  abline(v = x + 0.5)
  axis(1)
  axis(2, at = y, labels = FALSE)
  mtext(text = colnames(dd), side = 2, at = y, las = 1, font = ifelse(climtotal < 10, 2, 1), line = 0.85)
  # x-axis label
  mtext(side = 1, text = 'Year', line = 1.8)
  dev.off()
  # plot sampling timing
  png(paste(destDirFigures,
            paste0(paste('09_samplingTiming',
                         names(tbl)[i],
                         sep = '_'),
                   '.png'),
            sep = '/'),
      width = 6, height = 4, units = 'in', # portrait
      #width = 8, height = 4.5, units = 'in', # landscape
      pointsize = 6, res = 250)
  breaks <- c(1991, seq(1995, 2020, 5))
  plotmar <- c(3.5, 1, 0, 1)
  palwidth <- 0.15
  mlay <- matrix(c(2,1), nrow = 2)
  layout(mlay, heights = c(palwidth, 1-palwidth))
  par(oma = c(0, 5, 0, 5))
  ## order data based on station number
  stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', names(dstn)))
  o <- order(stnnum)
  dstn <- dstn[o]
  ## define limits
  fakeTime <- as.POSIXct(paste(fakeYear, as.POSIXlt(df[['startTime']])$mon + 1, as.POSIXlt(df[['startTime']])$mday, sep = '-'), tz = 'UTC')
  xlim <- range(fakeTime)
  ylim <- c(1,max(length(dstn)))
  zlim <- c(1990, 2020)
  # initiate plot
  par(mar = plotmar)
  plot(x = xlim, y = 1:2, col = 'white',
       xlim = xlim, ylim = ylim,
       xlab = '', ylab = '',
       yaxs = 'i',
       xaxt = 'n', yaxt = 'n')
  # add x-axis
  xlabs <- axis.POSIXct(1)
  ## add grid
  abline(v = xlabs, lty = 3, col = 'lightgrey')
  ## x-label
  mtext(text = 'Date', side = 1, line = 1.8)
  # y-axis
  ### get indicies for labelling on left and right side of point
  labelidx <- 1:length(names(dstn))
  okidx <- labelidx %in% seq(1, max(labelidx), 2)
  yat <- ylim[1]:ylim[2]
  axis(side = 2, at = yat[okidx], labels = names(dstn)[okidx], las = 1)
  axis(side = 4, at = yat[!okidx], labels = names(dstn)[!okidx], las = 1)
  ## add grid
  abline(h = ylim[1]:ylim[2], lty = 3, col = 'lightgrey')
  box()
  cat('after ts plot', sep = '\n')
  print(par('mfg'))
  # go through each station
  for(istn in 1:length(dstn)){
    stn <- dstn[[istn]]
    timelt <- as.POSIXlt(stn[['startTime']])
    fakeTime <- as.POSIXct(paste(fakeYear, timelt$mon + 1, timelt$mday, sep = '-'), tz = 'UTC')
    cm <- colormap(z = stn[['year']], breaks = breaks)
    points(x = fakeTime,
           y = rep(istn, length(fakeTime)),
           pch = 21, col = 'lightgrey',
           bg = cm$zcol,
           cex = 1.4)
  }
  # draw palette
  drawPalette(breaks = breaks, zlab = ' ', pos = 3, labels = breaks, at = breaks)
  mtext(text = 'Year', side = 3, line = 2.8)
  dev.off()
}