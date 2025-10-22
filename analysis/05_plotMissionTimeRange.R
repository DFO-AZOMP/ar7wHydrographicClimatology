rm(list=ls())
library(oce)
source('00_setupFile.R')

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


load(paste(destDirData, 'ctdFiltered.rda', sep = '/'))

df <- data.frame(time = as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC'),
                 cruiseNumber = unlist(lapply(ctd, function(k) k[['cruiseNumber']])))

sdf <- split(df, df[['cruiseNumber']])
minTime <- as.POSIXct(unlist(lapply(sdf, function(k) min(k[['time']]))), origin = '1970-01-01')
maxTime <- as.POSIXct(unlist(lapply(sdf, function(k) max(k[['time']]))), origin = '1970-01-01')
mission <- names(sdf)

d <- data.frame(mission = mission,
                startTime = minTime,
                endTime = maxTime)

fakeYear <- 1990
filename <- '05_missionTimeRange.png'
png(paste(destDirFigures, filename, sep = '/'), width = 5, height = 6, units = 'in', res = 250, pointsize = 9)
ylim <- range(as.POSIXlt(d[['startTime']])$year + 1900)
dd <- d[1, ]
start <- as.POSIXlt(d[['startTime']], tz = 'UTC')
end <- as.POSIXlt(d[['endTime']], tz = 'UTC')
fakeStart <- as.POSIXct(paste(fakeYear, start$mon + 1, start$mday, sep = '-'), tz = 'UTC')
fakeEnd <- as.POSIXct(paste(fakeYear, end$mon + 1, end$mday, sep = '-'), tz = 'UTC')
xlim <- range(c(fakeStart, fakeEnd))
par(mar = c(3.5, 3.5, 1, 1))
plot(y = c(as.POSIXlt(dd[['startTime']])$year + 1900, as.POSIXlt(dd[['endTime']])$year + 1900),
     x = c(fakeStart[1], fakeEnd[1]),
     ylim = ylim, xlim = xlim,
     ylab = '', xlab = '',
     type = 'l')
abline(v= pretty(xlim), col = 'grey', lty = 3)
abline(h = pretty(ylim), col = 'grey', lty = 3)
for(id in 1:dim(d)[1]){
  dd <- d[id, ]
  lines(y = c(as.POSIXlt(dd[['startTime']])$year + 1900, as.POSIXlt(dd[['endTime']])$year + 1900),
        x = c(fakeStart[id], fakeEnd[id]),
        lwd = 1.4)
}
# x-axis label
mtext(text = 'Date', side = 1, line = 2.3)
# y-axis label
mtext(text = 'Year', side = 2, line = 2.3)
dev.off()
