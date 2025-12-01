rm(list=ls())
library(oce)
library(sp) # for point.in.polygon
source('00_setupFile.R') # has polygons and stations
load(paste(destDirData, 'ctd.rda', sep = '/'))
load(paste(destDirData, 'ctdgo.rda', sep = '/'))
ctd <- c(ctd, ctdgo)
# only concerned with ar7w
load('ar7wPolygon.rda')
polygons <- list(ar7w = ar7wPolygon) # re-name for ease
load("ar7wStationPolygons.rda")
stations <- ar7wStationPolygons # re-name for ease

# 1. Classify transect for each profile
lon <- unlist(lapply(ctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(ctd, function(k) k[['latitude']][1]))
ctdTransect <- lapply(polygons, function(k) mapply(function(longitude, latitude) point.in.polygon(point.x = longitude,
                                                                                                  point.y = latitude,
                                                                                                  pol.x = k[['longitude']],
                                                                                                  pol.y = k[['latitude']]),
                                                   lon,
                                                   lat))
transect <- vector(mode = 'logical', length = length(ctd))
for (i in 1:length(ctdTransect)){
  ok <- which(ctdTransect[[i]] == 1)
  transect[ok] <- names(polygons)[i]
}

# 2. Classify the station name for each profile
ctdStation <- lapply(stations, function(k) mapply(function(longitude, latitude) point.in.polygon(point.x = longitude,
                                                                                                 point.y = latitude,
                                                                                                 pol.x = k[['polyLongitude']],
                                                                                                 pol.y = k[['polyLatitude']]),
                                                  lon,
                                                  lat))
stationName <- vector(mode = 'logical', length = length(ctd))
for(i in 1:length(ctdStation)){
  ok <- which(ctdStation[[i]] == 1)
  stationName[ok] <- stations[[i]][['stationName']]
}

##
# for debugging purposes to double check which stations weren't assigned a station/transect
##
library(ocedata)
data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'bisque2'
lonlim <- range(unlist(lapply(stations, '[[', 'longitude')))
latlim <- range(unlist(lapply(stations, '[[', 'latitude')))

nostn <- which(stationName == "FALSE")
notransect <- which(transect == "FALSE")
both <- intersect(nostn, notransect)
par(mar = c(3.5, 3.5, 1, 1))
mapPlot(coastlineWorldFine,
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol,
        proj = proj,
        grid = c(2,1))
mapPoints(lon[notransect[!notransect %in% both]], lat[notransect[!notransect %in% both]], pch = 20, col = 'blue')
mapPoints(lon[nostn[!nostn %in% both]], lat[nostn[!nostn %in% both]], pch = 20, col = 'red')
mapPoints(lon[both], lat[both], pch = 20, col = 'black')
lapply(polygons, function(k) mapPolygon(k[['longitude']], k[['latitude']], border = 'red'))
legend('topleft', pch = c(20, 20, NA), lty = c(NA, NA, 1), col = c('black', 'red', 'red'), legend = c('no stn or tran', 'no stn', 'polygons'))

# add the transect, station name, and season to the metadata of each ctd station

for (i in 1:length(ctd)){
  tran <- transect[i]
  stn <- stationName[i]
  #seas <- season[i]
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'transect',
                             tran)
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'stationName',
                             stn)
  # ctd[[i]] <- oceSetMetadata(ctd[[i]],
  #                            'season',
  #                            seas)
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'program',
                             'azomp');
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'dataType',
                             'CD')
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'dataSource',
                             'azompMission')
}

# save
save(ctd, file = paste(destDirData, 'ctdFiltered.rda', sep = '/'))