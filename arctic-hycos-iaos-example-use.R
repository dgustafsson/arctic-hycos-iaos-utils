# /arctic-hycos-iaso-utils/arctic-hycos-iaos-example-use.R
#
# Copyright 2018 SMHI
#
# This file is part of the Arctic-HYCOS iAOS utils, which are open source and distributed 
# under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at your option) 
# any later version. The Arctic-HYCOS iAOS utils are distributed 
# in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU 
# General Public License for more details. You should have received a copy of the Lesser 
# GNU General Public License along with the Arctic-HYCOS iAOS utils. 
# If not, see <http://www.gnu.org/licenses/>.
#
#
# Arctic-HYCOS iAOS utils are a set of R tools for downloading, exploring, and extracting hydrological
# data provided by the Arctic-HYCOS project (https://hydrohub.wmo.int/en/projects/Arctic-HYCOS).
#
# Data are accessed from the web-services of the Global Runoff Data Centre (http://www.bafg.de/GRDC)
# which serves as a data hub for the Arctic-HYCOS project.
#
# Arctic-HYCOS iAOS utils are developed in the EU H2020 project INTAROS - Integrated Arctic
# Observation Systems (www.intaros.eu) by the Swedish Meteorological and Hydrological Institute. 
# 
# File:    arctic-hycos-iaos-example-use.R (example use)
# Author:  David Gustafsson, SMHI (Swedish Meteorological and Hydrological Institute)
# Version: 2018-11-29
#
# Usage:   See example below. The example assumes the application files are stored
#          in a local folder defined in the 
# ---------------------------------------------------------------------------------

### 1. initialization [EDIT HERE] 
{
  ## Define path to application files [EDIT HERE]
  {
    appDir = "D:/arctic-hycos-iaso-utils" # EDIT HERE to fit your system
  }
  
  ## Source the function collections (arctic-hycos-iaos-functions.R) [DONT EDIT]
  {
    source(paste(appDir,"arctic-hycos-iaos-functions.R",sep="/"))
  }
}

### 2. create local working/download/output folders on your system [EDIT HERE]
{
  create.local.working.folders(wdir = "D:/arctic-hycos-iaso-utils")
}

### 3. download/load the station list from GRDC (forcedReload = T will force reload of existing local files)
{
  download.arctic.hycos.station.list.from.GRDC(forcedReload = F)
}

### 4. download archive data from GRDC
{
  download.archive.data.from.GRDC(forcedReload = F)
}

### 5. Example 1: station search by river
{
  # a. search station metadata without timeseries
  search.only = get.Arctic.HYCOS.data(river = "Lena",read.archive.data = F);
  print(search.only)
  
  # b. search station metadata and load archive discharge data 
  search.and.data = get.Arctic.HYCOS.data(river = "Lena",read.archive.data=T);
  print(search.and.data$stn.metadata)
  print(search.and.data$archive.discharge.data)
  
  # c. plot river disharge times series
  x = search.and.data$archive.discharge.data[[1]]$daily.data$date
  y = search.and.data$archive.discharge.data[[1]]$daily.data$value
  main = paste(search.and.data$stn.metadata$river[1]," at ",search.and.data$stn.metadata$station[1])
  plot(x,y,main=main)
}

### 6. Example 2: search station by country
{
  search.by.country = get.Arctic.HYCOS.data(country = "Finland",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "Canada",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "USA",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "Iceland",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "Norway",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "Greenland",read.archive.data=F);print(search.by.country$stn.metadata)
  search.by.country = get.Arctic.HYCOS.data(country = "Russia",read.archive.data=F);print(search.by.country$stn.metadata)
}

### 7. Exemple 3: export search result - Lena river stations
{
  # Search and load Lena river station data
  lena.river.data = get.Arctic.HYCOS.data(river = "Lena",read.archive.data=T);
  
  # Export search result
  export.archive.data(archive.data = lena.river.data,exportMetadata = T,exportOriginal = T,exportCSV = T, exportHYPE = T)
  
}

### 8. Example 4: export search result - all stations
{
  # Search and load Lena river station data
  all.data = get.Arctic.HYCOS.data(read.archive.data=T);
  
  # Export search result
  export.archive.data(archive.data = all.data,exportMetadata = F,exportOriginal = F,exportCSV = F, exportHYPE = T)
  
}




