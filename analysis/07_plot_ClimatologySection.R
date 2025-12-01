rm(list=ls())
library(oce)
library(csasAtlPhys)
library(cmocean)
plotProfile <- oce::plotProfile
topoFile <- download.topo(west = -70, east = -40,
                          south = 38, north = 65,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
ocetopo[['z']] <- ocetopo[['z']]*-1 # to work with `plot,section-method` `showBottom = ocetopo` param
source('00_setupFile.R')
# fn for making transect polygon nice
transectPoly <- function(x, y, yadj = 50){
  xpoly <- c(x[1], x, max(x), max(x), x[1], x[1])
  ypoly <- c(y[1], y, y[length(y)], max(y) + yadj, max(y) + yadj, y[1])
  return(list(xpoly = xpoly, ypoly = ypoly))
}
# load climatology
load(paste(destDirData, 'climatology.rda', sep = '/'))
# load transectDefinition
load("ar7wTransectDefinition.rda")
transectDefinitions <- list(ar7w = ar7wTransectDefinition)
# load stations
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease
stationNames <- unlist(lapply(stations, '[[', 'stationName'))
stationLongitude <- unlist(lapply(stations, '[[', 'longitude'))
stationLatitude <- unlist(lapply(stations, '[[', 'latitude'))

df <- data.frame(transect = unlist(lapply(climatology, function(k) k[['transect']])),
                 climStartYear = unlist(lapply(climatology, function(k) min(k[['climatologyYears']]))),
                 climEndYear = unlist(lapply(climatology, function(k) max(k[['climatologyYears']]))))
# add region for transect == ar7w, if not ar7w, let region be null
ar7wregions <- c('labradorShelf', 'full', 'greenlandShelf')
dfreg <- NULL
for(i in 1:dim(df)[1]){
  transect <- df[['transect']][i]
  if(transect == 'ar7w'){
    dfadd <- data.frame(df[rep(i, times = length(ar7wregions)), ],
                       region = ar7wregions,
                       idx = i)
  } else {
    dfadd <- data.frame(df[i, ],
                        region = NA,
                        idx = i)
  }
  dfreg <- rbind(dfreg,
                 dfadd)
}
# set rownames to null for new df
row.names(dfreg) <- NULL
# re-define df
df <- dfreg
dftran <- unique(df[ , names(df) %in% c('transect', 'region')])

vars <- list(c('temperature', 'salinity', 'sigmaThetaAvg'),
             c('temperature', 'salinity', 'sigmaThetaAvg'),
             c('temperatureAnomaly', 'salinityAnomaly', 'sigmaThetaAnomaly'))

#for(it in 1:dim(dftran)[1]){
for(it in 1:dim(dftran)[1]){
  ok <- df[['transect']] %in% dftran[['transect']][it] & df[['region']] %in% dftran[['region']][it]
  d <- climatology[df[['idx']][ok]]
  keep <- unlist(lapply(d, function(k) !is.na(k[['longitude0']])))
  d <- d[keep]
  okdef <- names(transectDefinitions) == dftran[['transect']][it]
  if(any(okdef)){
    bottom <- transectDefinitions[[okdef]][['bottom_outline']]
    bottomPoly <- transectPoly(x = bottom[['distance_km']],
                               y = bottom[['elevation_m']]*-1,
                               yadj = 100)
  }
  if(length(d) == 0){
    cat(paste('No climatology created for the', dftran[['transect']][it], 'transect.'), sep = '\n')
    next
  } else {
    height <- 3.875 * 2
    width <- 3.25 * 1.5
    mfrow <- c(3,1)
    region <- dftran[['region']][it]
    png(filename = paste(destDirFigures,
                         paste0(paste('07_climatologyComparison',
                                      dftran[['transect']][it],
                                      region,
                                      sep = '_'), '.png'), sep = '/'),
        #width = 11, height = ifelse(length(d) == 3, 6, 2.8), units = 'in',
        width = width, height = height, units = 'in',
        res = 200, pointsize = 10)
    oma <- c(2, 2, 2, 0)
    mar <- c(1.5, 3.5, 1.5, 2)
    par(mfrow = mfrow, mar = mar, oma = oma)
    for(i in 1:ifelse(length(d) == 3, 2, length(d))){
      s <- d[[i]][['sectionSmooth']]
      sctd <- d[[i]][['avgProfiles']]
      lon0 <- d[[i]][['longitude0']]
      lat0 <- d[[i]][['latitude0']]
      # get ylim based on region
      if(region == 'full'){ # pre-defined xlim
        ylims <- d[[i]][['ylim']]
      } else {
        ylims <- regionylim[[region]]
      }
      # get xlim based on region
      if(region == 'full'){ # pre-defined xlim
        xlims <- d[[i]][['xlim']]
        if(any(okdef)){
          xlims <- range(bottomPoly$xpoly)
        }
        #
      } else {
        dist1stns <- regionStations[[region]]
        ok <- stationNames %in% dist1stns
        xlims <- range(geodDist(longitude1 = stationLongitude[ok],
                                latitude1 = stationLatitude[ok],
                                longitude2 = lon0,
                                latitude2 = lat0))
      }

      transectName <- d[[i]][['transect']]
      climYear <- d[[i]][['climatologyYears']][[1]] # use min year, but special case for difference
      # for use by contour, resulting barnes interp 'stations'
      clx <- geodDist(longitude1 = s[['longitude', 'byStation']],
                      latitude1 = s[['latitude', 'byStation']],
                      longitude2 = lon0 , latitude2 = lat0)
      clxd <- geodDist(longitude1 = unlist(lapply(sctd, function(k) k[['longitude']][1])),
                       latitude1 = unlist(lapply(sctd, function(k) k[['latitude']][1])),
                       longitude2 = lon0,
                       latitude2 = lat0)
      cly <- s[['station',1]][['pressure']]
      # for anomaly scorecard plot, add half of the diff between last two stations
      #xlims[2] <- xlims[2] + diff(clxd[(length(clxd)-1):length(clxd)])/2
      #xlim <- c(0, max(clxd))
      # set up for plotting
      dataNames <- names(sctd[[1]]@data)

      for(var in vars[[i]]){
        if(!(var %in% dataNames) & var != 'sigmaTheta'){
          next
        } else {
          # set up various plotting parameters
          zlim <- transectPlotLimits[['limits']][[region]][[gsub('Avg', '', var)]]
          levels <- transectPlotLimits[['contourLevels']][[region]][[gsub('Avg', '', var)]]
          levelLimits <- transectPlotLimits[['contourLevelLimits']][[region]][[gsub('Avg', '', var)]]
          if(var == 'temperatureAnomaly'){
            zlim <- range(seq(-7,7,1)/8)
            levels <- levels/8
            levelLimits <- levelLimits/8
          }
          if(var == 'salinityAnomaly' | var == 'sigmaThetaAnomaly'){
            zlim <- range(anomalyColors$breaks[anomalyColors$breaks < 4])/4
            levels <- levels/4
            levelLimits <- levelLimits/4
          }
          if(var == 'temperature'){
            zlim <- transectPlotLimits[['limits']][[region]][['theta']]
            levels <- transectPlotLimits[['contourLevels']][[region]][['theta']]
            levelLimits <- transectPlotLimits[['contourLevelLimits']][[region]][['theta']]
          }
          if(var == 'sigmaThetaAvg'){
            levels <- transectPlotLimits[['contourLevels']][[region]][['sigmaTheta']]
            levelLimits <- transectPlotLimits[['contourLevelLimits']][[region]][['sigmaTheta']]
          }
          if(var == 'sigmaThetaAnomaly'){
            zlim <- range(anomalyColors$breaks[anomalyColors$breaks < 4])/4
          }
          axes <- switch(var,
                         'temperature' = FALSE,
                         'salinity' = FALSE,
                         'sigmaThetaAvg' = TRUE)
          #axes <- ifelse(i == length(d), TRUE, FALSE)
          title <- switch(var,
                          'temperature' = TRUE,
                          'salinity' = FALSE,
                          'sigmaThetaAvg' = FALSE)
          R <- ']'
          L <- '['
          zlab <- switch(var,
                         'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                         'temperatureAnomaly' = getAnomalyLabel('temperatureAnomaly', bold = TRUE),
                         'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                         'salinityAnomaly' = getAnomalyLabel('salinityAnomaly', bold = TRUE),
                         'sigmaThetaAvg' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))),
                         'sigmaThetaAnomaly' = getAnomalyLabel('sigmaThetaAnomaly', bold = TRUE))
          zcol <- switch(var,
                         'temperature' = cmocean::cmocean("thermal"),
                         'temperatureAnomaly'= head(anomalyColors$colors, -4),
                         'salinity' = cmocean::cmocean("haline"),
                         'salinityAnomaly' = head(anomalyColors$colors, -4),
                         'sigmaThetaAvg' = cmocean::cmocean("dense"),
                         'sigmaThetaAnomaly' = head(anomalyColors$colors, -4))
          zbreaks <- switch(var,
                            'temperature'= NULL, #seq(Tlim[1], Tlim[2],1),
                            'temperatureAnomaly'= seq(-7,7,1)/8,
                            'salinity'= NULL, #seq(Slim[1], Slim[2]),
                            'salinityAnomaly' = head(anomalyColors$breaks, -4)/4,
                            'sigmaThetaAvg' = NULL, #seq(STlim[1], STlim[2],1),
                            'sigmaThetaAnomaly' = head(anomalyColors$breaks, -4)/4)

          par(cex = 0.8)
          {if(!axes){
            plot(s, which = var, ztype = 'image',
                 ylim = rev(ylims), zlim = zlim, xlim = xlims,
                 zcol = zcol, zbreaks = zbreaks,
                 xaxs = 'i',
                 showBottom = FALSE,
                 legend.loc = '',
                 axes = axes, xlab = '', mar = mar,
                 longitude0 = lon0, latitude0 = lat0)
          } else{
            plot(s, which = var, ztype = 'image',
                 ylim = rev(ylims), zlim = zlim, xlim = xlims,
                 zcol = zcol, zbreaks = zbreaks,
                 xaxs = 'i',
                 showBottom = FALSE,
                 legend.loc = '',
                 mar = mar, stationTicks = FALSE,
                 longitude0 = lon0, latitude0 = lat0)
          }
          }
          clz <- matrix(s[[var]], byrow = TRUE, nrow = length(s[['station']]))
          contour(clx, cly, clz, levels = levels[levels > levelLimits[1] & levels < levelLimits[2] ],
                  col = 'black', add = TRUE, #labcex = 1,
                  vfont = c('sans serif', 'bold'),
                  xlim = xlims, ylim = ylim)
          contour(clx, cly, clz, levels = levels[levels <= levelLimits[1] | levels >= levelLimits[2]],
                  col = 'white', add = TRUE, #labcex = 1,
                  vfont = c('sans serif', 'bold'),
                  xlim = xlims, ylim = ylim)
          # get the bottom that `plot, section-method` created (pulled from oce/sections.R)
          # some modifications were made to work with my code/variable names
          # topoResolution <- geodDist(0, 0, 0, diff(ocetopo[["latitude"]][1:2]))
          # slon <- c(lon0, s[["longitude", "byStation"]])
          # slat <- c(lat0, s[["latitude", "byStation"]])
          # sectionSpan <- geodDist(min(slon, na.rm=TRUE), min(slat, na.rm=TRUE),
          #                         max(slon, na.rm=TRUE), max(slat, na.rm=TRUE))
          # nin <- length(slon)
          # ## double up on resolution, although perhaps not needed
          # nout <- as.integer(1 + 2 * sectionSpan / topoResolution)
          # blon <- approx(1:nin, slon, n=nout)$y
          # blat <- approx(1:nin, slat, n=nout)$y
          # bottom.y <- topoInterpolate(blon, blat, ocetopo) * -1
          # bottom.x <- approx(1:nin, c(0, clx), n=nout)$y
          # bottom.x <- c(bottom.x[1], bottom.x, tail(bottom.x, 1))
          # usr3 <- par('usr')[3] * -1
          # bottom.y <- c(usr3, bottom.y, usr3)
          # polygon(bottom.x, bottom.y * -1, col="grey49")
          polygon(bottomPoly$xpoly, bottomPoly$ypoly, col = 'grey49')
          #polygon(bottom$xpoly, bottom$ypoly, col = 'grey49')
          plabel <- ifelse(region == 'full', -100, -10)
          plotStationLocations(distance = clxd[clxd >= xlims[1] & clxd <= xlims[2]], plabel = plabel)
          legend(ifelse(region == 'greenlandShelf', 'bottomright', 'bottomleft'), legend = zlab,
                 bg = 'n', bty = 'n',
                 text.col = 'white', cex = 1)
          # add x and y-axis
          {if(!axes){
            axis(side = 1, at = pretty(xlims), labels = FALSE) # not working for browns bank
            axis(side = 2, at = pretty(ylims))

          } else{
            axis(side = 1, at = pretty(xlims), labels = FALSE, tcl = -0.01)
            mtext(text = 'Distance [km]', side = 1, line = 2, cex = 4/5)
            axis(side = 2, at = pretty(ylims), labels = FALSE)
          }
          }
          #axis(side = 3, at = clx, labels = FALSE, tcl = -0.01)
          # add y-axis on other side
          axis(side = 4, at = pretty(ylims), labels = FALSE, line = -3.45)
          # # add title
          # if(title & i == 1){
          #   titlet <- paste(getLocationName(dftran[['transect']][it]), dftran[['season']][it], sep = ' , ')
          #   mtext(titlet, side = 3, font = 2, cex = 1, outer = TRUE, line = 0)
          # }
          # add station number on top of triangles
          if(var == vars[[i]][1]){
            if(region == 'full'){
              labelline <- 1
              sctdname <- unlist(lapply(sctd, '[[', 'stationName'))
              # define stations that split up the section
              splitStn <- c('AR7W_10', 'AR7W_24')
              oksctdstn <- sctdname %in% splitStn
              labelctd <- c(sctd[[1]], sctd[oksctdstn], sctd[[length(sctd)]])
              labelat <- geodDist(longitude1 = unlist(lapply(labelctd, function(k) k[['longitude']][1])),
                                  latitude1 = unlist(lapply(labelctd, function(k) k[['latitude']][1])),
                                  alongPath = TRUE)
              stnlabels <- gsub('^\\w+\\_(\\d+)', '\\1', unlist(lapply(labelctd, '[[', 'stationName')))
              axis(side = 3, at = labelat, line = labelline, labels = stnlabels, cex.axis = 4/5)
              rlabelat <- labelat[1:(length(labelat)-1)] + diff(labelat)/2
              mtext(text = paste('AR7W', c('W', 'C', 'E'), sep = '-'), at = rlabelat, side = 3, line = labelline + 1.4, cex = 4/5)
            } else {
              line <- 0.5
            }
            if(region != 'full') mtext(at = clxd[clxd >= xlims[1] & clxd <= xlims[2]], text = gsub('^\\w+\\_(\\d+)', '\\1', unlist(lapply(sctd, '[[', 'stationName'))[clxd >= xlims[1] & clxd <= xlims[2]]),
                  line = line, cex = 4/5)
          }
          if(var %in% c('temperature', 'temperatureAnomaly')){
            # mtext(text = ifelse(climYear == 'difference', '1991 - 1981', climYear),
            #       side = 2, line = 3.5, font = 2)
          }
        } # closes else part of if statement
      } # closes variable
    } # closes i
    # construct scorecard of differences
    if(length(d) != 3){
      dev.off()
      next
    }
    i <- 3
    s <- d[[i]][['sectionSmooth']]
    sctd <- d[[i]][['avgProfiles']]
    lon0 <- d[[i]][['longitude0']]
    lat0 <- d[[i]][['latitude0']]

    # calculate x, distance along section
    x <- geodDist(longitude1 = unlist(lapply(sctd, function(k) k[['longitude']][1])),
                  latitude1 = unlist(lapply(sctd, function(k) k[['latitude']][1])),
                  longitude2 = lon0 , latitude2 = lat0)
    # define y, unique pressure values
    allP <- unlist(lapply(sctd, '[[', 'pressure'))
    y <- unique(allP)
    # now iterate through each variable
    for(var in vars[[i]]){
      # define z and fill matrix with data
      z <- matrix(data = NA, nrow = length(y), ncol = length(x))
      for(is in 1:length(sctd)){
        pidx <- unlist(lapply(sctd[[is]][['pressure']], function(k) which(y == k)))
        z[pidx, is] <- sctd[[is]][[var]]
      }
      cm <- colormap(z = z,
                     zlim = max(abs(z), na.rm = TRUE) * c(-1, 1),
                     col = cmocean('balance'),
                     missingColor = 'grey49')
      imagep(x = x, y = y, z = t(z),
             colormap = cm,
             ylim = rev(ylims), xlim = xlims,
             axes = FALSE, drawPalette=TRUE, mar = mar)
      box()
      # add station locations
      plotStationLocations(distance = clxd, plabel = -10)
      # add y-axis
      axis(2)
      mtext(text = resizableLabel('depth'), side = 2, line = 2, cex = 4/5)
      axis(side = 4, labels = FALSE)
      # add x-axis
      axis(1)
      mtext(text = 'Distance [km]', side = 1, line = 2, cex = 4/5)
      #axis(1, at = x, labels = gsub('^\\w+\\_(\\d+)', '\\1', unlist(lapply(sctd, '[[', 'stationName'))))

    }
    dev.off()
  }
}
