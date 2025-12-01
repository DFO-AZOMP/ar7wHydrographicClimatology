# ar7wHydrographicClimatology

## Overview 

Repository for the "Atlantic Zone Off-shelf Monitoring Program 1991 to 2020 AR7W Hydrographic Climatology" report published in XXXX.
It contains all necessary scripts to run the analysis and create the report. It does not contain any data files or figures.
The directory structure is as follows :

* The directory `analysis` contains scripts, that should be ran sequentially (with the exception of any files with pattern `00_*`), to complete the analysis
and create figures for the report.
* The directory `R` contains functions.
* The directory `report` is for the technical report using [csasdown](https://github.com/pbs-assess/csasdown).

## analysis

Prior to the user running the scripts to complete the analysis, a few files must be created in order to define variables that point 
to the location of the data on the Maritimes regions network drives and variables to run queries from a database. If more information
is needed about both, contact information is provided in the report.

### 00_arcPath.R

A file named `00_arcPath.R` should be created in `analysis/`. Within this file the path to the archived `ctd` directory should be defined
as
```
arcPath <- ''
```
and putting the path in the quotations. If more information about the path and how to gain access is required, please see contact information in the report.




