rm(list=ls())
library(oce)
library(csasAtlPhys)
library(sp) # for point.in.polygon
source('00_setupFile.R')

expocodes <- c('18HU92014_1')
baseurl <- "https://data.pmel.noaa.gov/"
url <- paste0(baseurl,
              "generic/erddap/tabledap/cchdo_ctd.nc",
              "?profile_id%2C",
              "expocode%2C",
              "station%2C",
              "cast%2C",
              "sample%2C",
              "time%2C",
              "latitude%2C",
              "longitude%2C",
              "pressure%2C",
              "ctd_temperature_unk%2C",
              "ctd_temperature_unk_qc%2C",
              "ctd_temperature_68%2C",
              "ctd_temperature_68_qc%2C",
              "ctd_temperature%2C",
              "ctd_temperature_qc%2C",
              "ctd_salinity%2C",
              "ctd_salinity_qc%2C",
              "ctd_oxygen_ml_l%2C",
              "ctd_oxygen_ml_l_qc%2C",
              "ctd_oxygen%2C",
              "ctd_oxygen_qc%2C",
              "ctd_oxygen_umol_l%2C",
              "ctd_oxygen_umol_l_qc%2C",
              "bottle_time%2C",
              "profile_type%2C",
              "geometry_container",
              "&expocode=%22",
              expocodes,"%22")

# download data
for(iu in 1:length(url)){
  # construct filename output
  destfile <- paste0(paste('cchdoData', expocodes[iu], sep = '_'), '.nc')
  destFilename <- paste(destDirData, destfile, sep = '/')
  download.file(url = url[iu],
                destfile = destFilename,
                mode = 'wb')
}