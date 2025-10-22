library(csasAtlPhys)
# check if it exists (might be defined in other scripts that are sourcing it)
if(!exists('makeDirs')){
  makeDirs <- TRUE
}
# load file that defines arcPath
## define file
pathToArcFile <- ifelse(basename(getwd()) == 'analysis', './', '../analysis') # to avoid errors when building report
arcPathFile <- paste(pathToArcFile, '00_arcPath.R', sep = '/')
## function to check
checkArcPath <- function(file){
  if(file.exists(file)){
    source(file)
    if(!exists('arcPath')){
      stop(paste("Please define 'arcPath' in", file))
    }
  } else {
    stop(paste(file, 'does not exist.',
               "Please create the file and define 'arcPath'.",
               "See README.md for details."))
  }
}
## check
checkArcPath(file = arcPathFile)

# check if directory for saving various data files has been created
destDirData <- './data'
destDirFigures <- './figures'
destDirNetCDF <- './netCDFclimatology'
destDirSuppFigures <- './supplementaryFigures'

dirsToMake <- c(destDirData,
                destDirFigures,
                destDirNetCDF,
                destDirSuppFigures)
if(makeDirs){
  for(i in 1:length(dirsToMake)){
    if(!dir.exists(dirsToMake[i])) dir.create(dirsToMake[i], recursive = TRUE)
  }
}

# some useful things that will be used in multiple scripts

# these files will cause issues due to their long header
badfiles <- paste(arcPath,
                  c('2006/CTD_TEL2006615_092_288086_DN.ODF',
                    '2003/CTD_NED2003003_000_258853_DN.ODF',
                    '2004/CTD_TEM2004004_088_263653_DN.ODF',
                    '2013/CTD_HUD2013037_042_1_DN.ODF',
                    "2015/CTD_HUD2015030_127_1_DN.ODF",
                    "1996/CTD_96999_003_001_DN.ODF", # variable names bad
                    "1996/CTD_96999_003_002_DN.ODF" # variable names bad
                  ),
                  sep = '/')
# define some information for plotting
limits <- list('full' = list(theta = c(1.5, 5),
                             salinity = c(34.6, 35),
                             sigmaTheta = c(27.5, 27.95)),
               'labradorShelf' = list(theta = c(-2, 5),
                                      salinity = c(32, 35),
                                      sigmaTheta = c(25.5, 27.95))
)

contourLevels <- list('full' = list(theta = seq(1.5, 5, 0.5),
                                    salinity = seq(34.6, 35, 0.05),
                                    sigmaTheta = seq(27.5, 27.95, 0.05)),
                      'labradorShelf' = list(theta = seq(-2, 5, 1),
                                             salinity = seq(32, 35, 0.5),
                                             sigmaTheta = seq(25.5, 27.95, 0.50))
)
contourLevelLimits <- limits # the same for now
transectPlotLimits <- list(limits = limits,
                           contourLevels = contourLevels,
                           contourLevelLimits = contourLevelLimits)