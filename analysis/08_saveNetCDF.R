rm(list=ls())
library(ncdf4)
library(oce)
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')
# 1. load the climatology
load(paste(destDirData, 'climatology.rda', sep = '/'))

#outputType <- c('station', 'interpolated')
outputType <- 'station' # only provide station climatology
for(i in 1:length(climatology)){
  d <- climatology[[i]]
  for(type in outputType){
    if(type == 'station'){
      s <- d[['section']]
      if(is.na(s)){ # an warning will be saved for a valid section grid. this is fine.
        cat(paste('No climatology for', d[['transect']], paste(range(d[['climatologyYears']]), collapse = ' to ')), sep = '\n')
        next
      }
    }
    if(type == 'interpolated'){
      s <- d[['sectionSmooth']]
    }
    # set up the metadata
    # set up the dimensions
    pressure <- s[['pressure', 'byStation']]
    pressure <- pressure[[which.max(unlist(lapply(pressure, length)))]]
    ctd <- s[['station']]
    if(type == 'station'){
      station <- unlist(lapply(ctd, function(k) k[['stationName']]))
      # have to omit the fake station added to make plot look nice
      keep <- station != 'fakeStn'
      ctd <- ctd[keep]
      station <- station[keep]
      ## remove from 's' as well
      s <- subset(s, indices = which(keep))
      stationNumber <- as.numeric(gsub('\\w+\\_(\\w+)', '\\1', station))
    }
    if(type == 'interpolated'){
      stationNumber <- 1:length(ctd)
      station <- stationNumber # not great, but to avoid errors ?
    }
    # define dimensions
    dimpress <- ncdim_def(#name = 'PRESPR01', # BODC
                          name = 'sea_water_pressure', # CF
                          units = 'dbar',
                          vals = pressure,
                          longname = 'Pressure (spatial coordinate) exerted by the water body by profiling pressure sensor and correction to read zero at sea level')
    dimstn <- ncdim_def(name = 'station', units = 'none', vals = stationNumber) # has to be numeric
    # define variables
    varlon <- ncvar_def(name = 'longitude', # CF
                        #name = 'ALONZZ01', # BODC
                        units = 'degree_east',
                        dim = list(dimstn))
    varlat <- ncvar_def(name = 'latitude', # CF
                        #name = 'ALATZZ01',
                        units = 'degree_north',
                        dim = list(dimstn))
    # define number of characters for station output (see `ncvar_put` for example)
    dimnchar <- ncdim_def(name = 'nchar', units = "", vals = 1:length(stationNumber), create_dimvar = FALSE)
    varstn <- ncvar_def(name = 'stationName',
                        units = '',
                        dim = list(dimnchar, dimstn),
                        prec = 'char')
    varT <- ncvar_def(name = 'sea_water_temperature', # CF
                      #name = 'TEMPP901', # BODC
                      units = 'degree_C',
                      longname = "Temperature (ITS-90) of the water body",
                      missval = NA,
                      prec = 'double',
                      dim = list(dimpress, dimstn))
    varTSD <- ncvar_def(name = 'sea_water_temperature_standard_deviation',
                        #name = "TEMPSD01",
                        units = 'degree_C',
                        longname = "Temperature (ITS-90) standard deviation of the water body",
                        missval = NA,
                        prec = 'double',
                        dim = list(dimpress, dimstn))
    varS <- ncvar_def(name = 'sea_water_practical_salinity', # CF
                      #name = 'PSLTZZ01', # BODC
                      units = 'none',
                      longname = "Practical salinity of the water body",
                      missval = NA,
                      prec = 'double',
                      dim = list(dimpress, dimstn))
    varSSD <- ncvar_def(name = 'sea_water_practical_salinity_standard_deviation',
                        #name = "SDALPR01",
                        units = 'none',
                        longname = 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm',
                        missval = NA,
                        prec = 'double',
                        dim = list(dimpress, dimstn))
    varST <- ncvar_def(name = 'sea_water_sigma_theta',
                       #name = 'SIGTPR01',
                       units = 'kg m-3',
                       longname = "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                       missval = NA,
                       prec = 'double',
                       dim = list(dimpress, dimstn))
    varSTSD <- ncvar_def(name = 'sea_water_sigma_theta_standard_deviation',
                         units = 'kg m-3',
                         longname = "Sigma-theta standard deviation of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm",
                         missval = NA,
                         prec = 'double',
                         dim = list(dimpress, dimstn))
    # list all variables together to load them together
    varlist <- list(varlon, varlat, varstn,
                    varT, varTSD,
                    varS, varSSD,
                    varST, varSTSD)
    # output data
    outputfile <- paste0(paste(d[['transect']],
                               paste0(min(d[['climatologyYears']]),
                                      'to',
                                      max(d[['climatologyYears']]),
                                      'climatology'),
                               type,
                               sep = '_'),
                         '.nc')
    # create file
    con <- nc_create(paste(destDirNetCDF, outputfile, sep = '/'), varlist)
    # add standard name, BODC parameter, and uom attributes
    # latitude
    ncatt_put(con, varlat, 'standard_name', 'latitude')
    ncatt_put(con, varlat, 'sdn_parameter_name', 'Latitude north')
    ncatt_put(con, varlat, 'sdn_parameter_urn', 'ALATZZ01')
    ncatt_put(con, varlat, 'sdn_uom_name', 'degree_north')
    ncatt_put(con, varlat, 'sdn_uom_urn', 'DEGN')
    # longitude
    ncatt_put(con, varlon, 'standard_name', 'longitude')
    ncatt_put(con, varlon, 'sdn_parameter_name', 'Longitude east')
    ncatt_put(con, varlon, 'sdn_parameter_urn', 'ALONZZ01')
    ncatt_put(con, varlon, 'sdn_uom_name', 'degree_east')
    ncatt_put(con, varlon, 'sdn_uom_urn', 'DEGE')
    # pressure
    # ncatt_put(con, varpress, 'standard_name', 'sea_water_pressure')
    # ncatt_put(con, varpress, 'sdn_parameter_name', 'Pressure (spatial coordinate) exerted by the water body by profiling pressure sensor and correction to read zero at sea level')
    # ncatt_put(con, varpress, 'sdn_parameter_urn', 'PRESPR01')
    # ncatt_put(con, varpress, 'sdn_uom_name', 'dbar')
    # ncatt_put(con, varpress, 'sdn_uom_urn', 'UPDB')
    # temperature
    ncatt_put(con, varT, 'standard_name', 'sea_water_temperature')
    ncatt_put(con, varT, 'sdn_parameter_name', 'Temperature of the water body')
    ncatt_put(con, varT, 'sdn_parameter_urn', 'TEMPPR01')
    ncatt_put(con, varT, 'sdn_uom_name', 'degrees Celcius')
    ncatt_put(con, varT, 'sdn_uom_urn', 'UPAA')
    # temperature standard deviation
    ncatt_put(con, varTSD, 'standard_name', 'sea_water_temperature_standard_deviation')
    ncatt_put(con, varTSD, 'sdn_parameter_name', 'Temperature (ITS-90) standard deviation of the water body')
    ncatt_put(con, varTSD, 'sdn_parameter_urn', 'TEMPSD01')
    ncatt_put(con, varTSD, 'sdn_uom_name', 'degrees Celcius')
    ncatt_put(con, varTSD, 'sdn_uom_urn', 'UPAA')
    # salinity
    ncatt_put(con, varS, 'standard_name', 'sea_water_practical_salinity')
    ncatt_put(con, varS, 'sdn_parameter_name', 'Practical salinity of the water body')
    ncatt_put(con, varS, 'sdn_parameter_urn', 'PSLTZZ01')
    ncatt_put(con, varS, 'sdn_uom_name', '')
    ncatt_put(con, varS, 'sdn_uom_urn', '')
    # salinity standard deviation
    ncatt_put(con, varSSD, 'standard_name', 'sea_water_practical_salinity_standard_deviation')
    ncatt_put(con, varSSD, 'sdn_parameter_name', 'Practical salinity standard deviation of the water body by conductivity cell and computation using UNESCO 1983 algorithm')
    ncatt_put(con, varSSD, 'sdn_parameter_urn', 'SDALPR01')
    ncatt_put(con, varSSD, 'sdn_uom_name', '')
    ncatt_put(con, varSSD, 'sdn_uom_urn', '')
    # sigmaTheta
    ncatt_put(con, varST, 'standard_name', 'sea_water_sigma_theta')
    ncatt_put(con, varST, 'sdn_parameter_name', "Sigma-theta of the water body by CTD and computation from salinity and potential temperature using UNESCO algorithm")
    ncatt_put(con, varST, 'sdn_parameter_urn', 'SIGTPR01')
    ncatt_put(con, varST, 'sdn_uom_name', 'kilograms per cubic meter')
    ncatt_put(con, varST, 'sdn_uom_urn', 'UKMC')
    # store global attributes (indicated by varid = 0 in ncatt_put)
    title <- paste("The Atlantic Repeat Hydrography Line 7 West",
                   'transect',
                   'climatology for the',
                   paste(min(d[['climatologyYears']]), max(d[['climatologyYears']]), sep = ' to '),
                   'reference period.')

    institute <- 'Department of Fisheries and Oceans Canada, Bedford Institute of Oceanography'
    author <- 'Chantelle Layton'
    source <- ''
    history <- ''
    climatologyDateRange <- 'May 01 to June 30'
    comment <- paste('Date ranges to define climatology',
                     'are',
                     climatologyDateRange)
    references <- ''
    ncatt_put(con, varid = 0, attname = 'title', attval = title)
    ncatt_put(con, varid = 0, attname = 'source', attval = source)
    ncatt_put(con, varid = 0, attname = 'history', attval = history)
    ncatt_put(con, varid = 0, attname = 'comment', attval = comment)
    ncatt_put(con, varid = 0, attname = 'Conventions', attval = 'CF-1.8')
    ncatt_put(con, varid = 0, attname = "country_code", attval = 1810)
    ncatt_put(con, varid = 0, attname = "sdn_country_id", attval = "SDN:C18::18")
    ncatt_put(con, varid = 0, attname = "sdn_country_vocabulary", attval = "http://vocab.nerc.ac.uk/collection/C18/current/")
    ncatt_put(con, varid = 0, attname = "institution", attval= "DFO BIO")
    ncatt_put(con, varid = 0, attname = "sdn_institution_id", attval= "SDN:EDMO::1811")
    ncatt_put(con, varid = 0, attname = "sdn_institution_vocabulary", attval = "https://edmo.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ORGANISATIONS (EDMO)")
    ncatt_put(con, varid = 0, attname = "creator_type", attval = "person")
    ncatt_put(con, varid = 0, attname = "creator_name", attval = "Chantelle Layton")
    ncatt_put(con, varid = 0, attname = "creator_country", attval = "Canada")
    ncatt_put(con, varid = 0, attname = "creator_email", attval = "BIO.Datashop@dfo-mpo.gc.ca")
    ncatt_put(con, varid = 0, attname = "creator_institution", attval = "Bedford Institute of Oceanography")
    ncatt_put(con, varid = 0, attname = "creator_address", attval= "P.O. Box 1006, 1 Challenger Dr.")
    ncatt_put(con, varid = 0, attname = "creator_city", attval = "Dartmouth")
    ncatt_put(con, varid = 0, attname = "creator_sector", attval = "gov_federal")
    ncatt_put(con, varid = 0, attname = "creator_url", attval = "https://www.bio.gc.ca/index-en.php")
    ncatt_put(con, varid = 0, attname = "sdn_creator_id", attval = "SDN:EDMO::1811")
    ncatt_put(con, varid = 0, attname = "license", attval = "Open Government Licence – Canada,  https://open.canada.ca/en/open-government-licence-canada")

    # get the data
    lon <- s[['longitude', 'byStation']]
    lat <- s[['latitude', 'byStation']]
    allpressure <- s[['pressure', 'byStation']]
    # define function to fill matrix
    fillMatrix <- function(y = pressure, yall = allpressure, z){
      m <- matrix(data = NA, nrow = length(y), ncol = length(z))
      for(im in 1:length(z)){
        oky <- unlist(lapply(yall[[im]], function(k) which(k == y)))
        m[oky, im] <- z[[im]]
      }
      m
    }
    T <- fillMatrix(y = pressure, yall = allpressure, z = s[['temperature', 'byStation']])
    Tsd <- fillMatrix(y = pressure, yall = allpressure, z = s[['temperatureSd', 'byStation']])
    S <- fillMatrix(y = pressure, yall = allpressure, z = s[['salinity', 'byStation']])
    Ssd <- fillMatrix(y = pressure, yall = allpressure, z = s[['salinitySd', 'byStation']])
    ST <- fillMatrix(y = pressure, yall = allpressure, z = s[['sigmaThetaAvg', 'byStation']])
    STsd <- fillMatrix(y = pressure, yall = allpressure, z = s[['sigmaThetaAvgSd', 'byStation']])
    # if interpolated, add NA values to pressures below the bottom depth
    if(type == 'interpolated'){
      bottomDepth <- abs(topoInterpolate(longitude = lon,
                                         latitude = lat,
                                         topo = ocetopo))
      for(id in 1:length(bottomDepth)){
        ok <- pressure < bottomDepth[id]
        T[!ok, id] <- NA
        Tsd[!ok, id] <- NA
        S[!ok, id] <- NA
        Ssd[!ok, id] <- NA
        ST[!ok, id] <- NA
        STsd[!ok, id] <- NA
      }
    }
    # store the data
    ## global attributes
    ncatt_put(con, varid = 0, attname = "geospatial_lat_min", attval = min(lat))
    ncatt_put(con, varid = 0, attname = "geospatial_lat_max", attval = max(lat))
    ncatt_put(con, varid = 0, attname = "geospatial_lat_units", attval = "degree_north")
    ncatt_put(con, varid = 0, attname = "geospatial_lon_min", attval = min(lon))
    ncatt_put(con, varid = 0, attname = "geospatial_lon_max", attval = max(lon))
    ncatt_put(con, varid = 0, attname = "geospatial_lon_units", attval = "degree_east")
    ## global variables
    ncvar_put(nc = con, varid = varlon, vals = lon)
    ncvar_put(nc = con, varid = varlat, vals = lat)
    ncvar_put(nc = con, varid = varstn, vals = station)
    ncvar_put(nc = con, varid = varT, vals = T)
    ncvar_put(nc = con, varid = varTSD, vals = Tsd)
    ncvar_put(nc = con, varid = varS, vals = S)
    ncvar_put(nc = con, varid = varSSD, vals = Ssd)
    ncvar_put(nc = con, varid = varST, vals = ST)
    ncvar_put(nc = con, varid = varSTSD, vals = STsd)
    # close connection
    (con)
    nc_close(con)
  }
}