rm(list=ls())
fillWithSmooth <- FALSE # logical that decides if missing stations filled with smoothed climatology profiles should be used
library(csasAtlPhys)
data("transectPlotLimits")
library(oce)
plotProfile <- oce::plotProfile
library(ocedata)
data('coastlineWorldFine')
data("ctd")
ghostctd <- ctd
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')
# get all stations names
# load station polygons
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease
allStationNames <- unlist(lapply(stations, function(k) k[['stationName']]))
# for rounding to get stations spacing
mround <- function(x,base){
  base*round(x/base)
}

if(fillWithSmooth){
  load(paste(destDirData, 'climatologyFilled.rda', sep = '/'))
} else {
  load(paste(destDirData, 'climatology.rda', sep = '/'))
}

climdf <- do.call('rbind', lapply(climatology, function(k) data.frame(transect = k[['transect']],
                                                                      climatology = k[['climatologyYears']][1])))
utran <- unique(climdf[!names(climdf) %in% 'climatology'])

for(i in 1:dim(utran)[1]){
  if(dim(utran)[1] == 1){
    look <- utran
  } else {
    look <- utran[i, ]
  }
  ok <- which(climdf[['transect']] %in% look[['transect']])
  d <- climatology[ok]
  # need to check if 'avgProfiles' exists in each 'climatology' (for the case of only one climatology for the transect)
  keep <- NULL
  for(ii in 1:length(d)){
    k <- d[[ii]]
    if(!all(is.na(k[['avgProfiles']]))){
      keep <- c(keep, ii)
    }
  }
  d <- d[keep]
  if(length(d) == 0){
    cat(paste('skipping', look[['transect']], 'no data'))
    next
  }
  # define limits based on region
  ## get all data out of d
  ### avgProfiles (climatology)
  aall <- unlist(lapply(d, function(k) k[['avgProfiles']]))
  astnname <- unlist(lapply(aall, '[[', 'stationName'))
  astnnum <- as.numeric(gsub('^\\w+\\_(\\d+)', '\\1', astnname))
  ### original data
  oall <- unlist(lapply(d, function(k) unlist(unname(k[['originalStations']]))))
  ostnname <- unlist(lapply(oall, '[[', 'stationName'))
  ostnnum <- as.numeric(gsub('^\\w+\\_(\\d+)', '\\1', ostnname))
  ## define region station number limits
  regionNumbers <- list('w' = c(1, 10),
                        'c' = c(10.5, 23.5),
                        'e' = c(24, 28))
  ### which index belong to each region
  okaregstn <- lapply(regionNumbers, function(k) which(astnnum >= k[1] & astnnum <= k[2]))
  okoregstn <- lapply(regionNumbers, function(k) which(ostnnum >= k[1] & ostnnum <= k[2]))
  ## define variables for a (avgProfiles) and o (originalStations)
  avariables <- c('temperature', 'salinity', 'sigmaThetaAvg')
  ovariables <- c('temperature', 'salinity', 'sigmaTheta')
  # define variable for output
  varlimits <- vector(mode = 'list', length = length(regionStations))
  for(irr in 1:length(regionStations)){
    # define depth to subset data to get good near surface limits
    zsurf <- ifelse(irr == 1, 20, 50)
    # avgProfiles
    oka <- okaregstn[[irr]]
    ra <- aall[oka]
    allavar <- lapply(avariables, function(var) unlist(lapply(ra, function(k) c(k[[var]] + k[[paste0(var,'Sd')]],
                                                                                k[[var]] - k[[paste0(var,'Sd')]]))))
    surfavar <- lapply(avariables, function(var) unlist(lapply(ra, function(k) {k <- subset(k, pressure < zsurf) ;c(k[[var]] + k[[paste0(var,'Sd')]],
                                                                                 k[[var]] - k[[paste0(var,'Sd')]])})))
    # originalStations
    oko <- okoregstn[[irr]]
    ro <- oall[oko]
    allovar <- lapply(ovariables, function(var) unlist(lapply(ro, function(k) subset(k, pressure >= 15)[[var]])))
    surfovar <- lapply(ovariables, function(var) unlist(lapply(ro, function(k) subset(k, pressure >= 15 & pressure < zsurf)[[var]])))
    # combine and get limits for variables
    ## since variables are in the same order i'm not going to worry about matching up based on name
    ##  going to do it by index
    limits <- vector(mode = 'list')
    for(ivv in 1:length(allavar)){
      limname <- switch(ovariables[ivv],
                        'temperature' = 'Tlim',
                        'salinity' = 'Slim',
                        'sigmaTheta' = 'STlim')
      allvar <- c(allavar[[ivv]], allovar[[ivv]])
      surfvar <- c(surfavar[[ivv]], surfovar[[ivv]])
      limits[[limname]] <- range(allvar, na.rm = TRUE)
      #limits[[limname]] <- c(quantile(x = surfvar, probs = ifelse(irr == 1, 0.0005, 0.005), na.rm = TRUE), quantile(x = allvar, probs = 0.995, na.rm = TRUE))
    }
    varlimits[[irr]] <- limits
  }
  # function to plot line and polygon to keep code DRY
  plotLineAndPolygon <- function(var){
    for(p in 1:length(dd)){
      polyCol <- switch(as.character(climYear[p]),
                        '1981' = hcl.colors(n=5, palette = 'Burg')[3],
                        '1991' = hcl.colors(n=5, palette = 'Blues')[3])
      alpha <- switch(as.character(climYear[p]),
                      '1981' = 150,
                      '1991' = 100)
      profile <- dd[[p]]
      ok <- !is.na(profile[[var]])
      polygon(c(profile[[var]][ok] - profile[[paste0(var,'Sd')]][ok],
                rev(profile[[var]][ok] + profile[[paste0(var,'Sd')]][ok])),
              c(profile[['pressure']][ok],
                rev(profile[['pressure']][ok])),
              border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
    }
    for(p in 1:length(dd)){
      linCol <- switch(as.character(climYear[p]),
                       '1981' = hcl.colors(n=5, palette = 'Burg')[1],
                       '1991' = hcl.colors(n=5, palette = 'Blues 2')[1])
      profile <- dd[[p]]
      ok <- !is.na(profile[[var]])
      lines(profile[[var]], profile[['pressure']], col = linCol, lwd = 2)
    }
  }

  # define plot limits
  mar <- c(3.5, 3.5, 3.5, 2.0)
  # mar[1,3] must be the same for profileMar and tsMar
  xmar <- 1.5
  profileMar <- c(xmar, 0.5, xmar, 1.5)
  tsMar <- c(xmar, 3.0, xmar, 0)
  # get station and other information
  dStations <- unique(unlist(lapply(d, function(k) unlist(lapply(k[['avgProfiles']], function(kk) kk[['stationName']])))))
  ## match up which station belongs to which region
  dstnnum <- as.numeric(gsub('^\\w+\\_(\\d+)', '\\1', dStations))
  ### which index belong to each region
  okdregstn <- as.data.frame(lapply(regionNumbers, function(k) dstnnum >= k[1] & dstnnum <= k[2]))
  # get station lon and lat
  okStations <- unlist(lapply(dStations, function(k) which(allStationNames == k)))
  stnLon <- unlist(lapply(stations[okStations], function(k) k[['longitude']]))
  stnLat <- unlist(lapply(stations[okStations], function(k) k[['latitude']]))
  for(istn in 1:length(dStations)){
    stn <- dStations[istn]
    dd <- lapply(d, function(k) k[['avgProfiles']][[which(unlist(lapply(k[['avgProfiles']], '[[', 'stationName')) == stn)]])
    # get original profiles
    ddorig <- unname(unlist(lapply(d, function(k) k[['originalStations']][names(k[['originalStations']]) == stn])))
    ## remove duplicates used in both climatologies
    ddorigStartTime <- as.POSIXct(unlist(lapply(ddorig, '[[', 'startTime')), origin = '1970-01-01', tz = 'UTC')
    ust <- unique(ddorigStartTime)
    okorig <- unlist(lapply(ust, function(k) which(ddorigStartTime == k)[1]))
    ddorig <- ddorig[okorig]
    ddorigStartTime <- ddorigStartTime[okorig]
    ## get year and define colormap
    ddorigYear <- as.POSIXlt(ddorigStartTime)$year + 1900
    datacm <- colormap(z = ddorigYear, breaks = c(1991, 2000, 2010, 2020), col = hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)])
    # some definitions for plotting
    climYear <- unlist(lapply(d, function(k) k[['climatologyYears']][1]))
    ## limits for variables
    oklim <- which(as.vector(as.matrix(okdregstn[istn, ])))
    vlim <- varlimits[[oklim]]
    ## pressure limits
    allP <- unlist(lapply(dd, '[[', 'pressure'))
    plim <- rev(range(allP, na.rm = TRUE))
    filename <- paste0(paste('06_profileAndDifference',
                             look[['transect']],
                             gsub('_', '', stn),
                             'wData',
                             sep = '_'),
                       '.png')
    png(filename = paste(destDirFigures,
                         filename, sep = '/'),
        width = 7, height = ifelse(length(dd)==1, 4.5, 7), units = 'in',
        res = 200, pointsize = 13)
    stnIdx <- which(dStations == stn)
    # now plot
    ## set up plotting scheme
    palwidth <- 0.22
    if(length(dd) == 2){
      mlay <- matrix(c(1, 2, 3, 7,
                       4, 5, 6, 0),
                     nrow = 2,
                     byrow = TRUE)
      layout(mat = mlay, widths = c(rep(1-palwidth, 3), palwidth))
    } else {
      mlay <- matrix(c(1, 2, 3, 4),
                     nrow = 1,
                     byrow = TRUE)
      layout(mat = mlay, widths = c(rep(1-palwidth, 3), palwidth))
    }
    par(oma = c(0, 3, 3, 1))
    # temperature
    plotProfile(ghostctd, xtype = 'temperature', Tlim = vlim[['Tlim']], plim = plim, col = 'white', mar = profileMar)
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['temperature']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'temperature')
    # add stn label
    #mtext(text = stn, side = 3, line = 3.2, adj=0, cex = 0.9)
    #cat(paste('Temp profile measurements', paste(par('fin'), collapse = ', ')), sep = '\n')
    #fin <- par('fin')
    # salinity
    plotProfile(ghostctd, xtype = 'salinity', Slim = vlim[['Slim']], plim = plim, col = 'white', mar = profileMar, ylab = '')
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['salinity']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'salinity')
    # sigmaTheta
    plotProfile(ghostctd, xtype = 'sigmaTheta', densitylim = vlim[['STlim']], plim = plim, col = 'white', mar = profileMar, ylab = '')
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['sigmaTheta']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'sigmaThetaAvg')
    # calculate and plot climatology profile differences
    ## but go to next index if only one climatology period exists
    if(length(dd) == 1){
      par(cex = 0.8)
      drawPalette(colormap = datacm, zlab = '', mar = profileMar)
      mtext(text = 'Year', side = 4, line = 4.8, cex = 4/5)
      dev.off()
      next
    }
    ## construct data.frame
    dfclim <- vector(mode = 'list', length = length(dd))
    for(id in 1:length(dd)){
      ddd <- dd[[id]]
      p <- ddd[['pressure']]
      data <- as.data.frame(ddd@data)
      keep <- grepl(pattern = '^(?:salinity|temperature|sigmaTheta)', names(data))
      data <- data[,keep]
      names(data) <- paste0(names(data), climYear[id])
      dfclim[[id]] <- data.frame(pressure = p,
                                 data)
    }
    climm <- merge(dfclim[[1]], dfclim[[2]], by = 'pressure', all = TRUE)
    # calculate difference
    vars <- c('temperature', 'salinity', 'sigmaThetaAvg')
    varsanomaly <- vector(mode = 'list', length = length(vars))
    varssd <- vector(mode = 'list', length = length(vars))
    for(iv in 1:length(vars)){
      var <- vars[iv]
      # 1991 minus 1981
      vardiff <- climm[[paste0(var, '1991')]] - climm[[paste0(var, '1981')]]
      vardiffsumsqsd <- climm[[paste0(var, 'Sd1991')]]^2 + climm[[paste0(var, 'Sd1981')]]^2
      vardiffsd <- sqrt(vardiffsumsqsd)
      varsanomaly[[iv]] <- vardiff
      varssd[[iv]] <- vardiffsd
    }
    names(varsanomaly) <- paste0(vars, 'Anomaly')
    names(varssd) <- paste0(vars, 'Sd')
    Talim <- range(c(varsanomaly[['temperatureAnomaly']] - (varssd[['temperatureSd']]/2),
                     varsanomaly[['temperatureAnomaly']] + (varssd[['temperatureSd']]/2)),
                   na.rm = TRUE)
    Salim <- range(c(varsanomaly[['salinityAnomaly']]- (varssd[['salinitySd']]/2),
                     varsanomaly[['salinityAnomaly']] + (varssd[['salinitySd']]/2)),
                   na.rm = TRUE)
    STalim <- range(c(varsanomaly[['sigmaThetaAvgAnomaly']]- (varssd[['sigmaThetaAvgSd']]/2),
                      varsanomaly[['sigmaThetaAvgAnomaly']] + (varssd[['sigmaThetaAvgSd']]/2)),
                    na.rm = TRUE)
    for(var in vars){
      # initiate plot
      ylab <- switch(var,
                     'temperature' = NULL,
                     'salinity' = '',
                     'sigmaThetaAvg' = '')
      yaxt <- switch(var,
                     'temperature' = 's',
                     'salinity' = 'n',
                     'sigmaThetaAvg' = 'n')
      plotProfile(ghostctd, xtype = ifelse(var == 'sigmaThetaAvg', 'sigmaTheta', var),
                  Tlim = Talim,
                  Slim = Salim,
                  densitylim = STalim,
                  plim = plim,
                  col = 'white',
                  mar = profileMar,
                  xlab = '',
                  ylab = ylab,
                  yaxt = yaxt)
      polyCol <- hcl.colors(n=5, palette = 'Grays')[3]
      linCol <- hcl.colors(n=5, palette = 'Grays')[1]
      alpha <- 100
      # +/- 0.5 sd
      varanomaly <- paste0(var, 'Anomaly')
      varsd <- paste0(var, 'Sd')
      x <- varsanomaly[[varanomaly]]
      xsd <- varssd[[varsd]]
      ok <- !is.na(x)
      polygon(c(x[ok] - (xsd[ok]/2),
                rev(x[ok] + (xsd[ok]/2))),
              c(climm[['pressure']][ok],
                rev(climm[['pressure']][ok])),
              border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
      # difference
      lines(x, climm[['pressure']], col = linCol, lwd = 2)
      # vertical line at 0
      abline(v = 0)
    }
    par(cex = 0.8)
    par(mar = profileMar)
    drawPalette(colormap = datacm, zlab = '', mar = profileMar)
    mtext(text = 'Year', side = 4, line = 4.8, cex = 4/5)
    dev.off()
  }
}