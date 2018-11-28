# /arctic-hycos-iaso-utils/arctic-hycos-iaos-functions.R
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
# File:    arctic-hycos-iaos-functions.R (collection of general function)
# Usage:   See arctic-hycos-iaos-example-use.R
# Author:  David Gustafsson, SMHI (Swedish Meteorological and Hydrological Institute)
# Version: 2018-11-29
# ---------------------------------------------------------------------------------

### Various R packages needed to read excelfiles, spatial data, etc
{
  library(openxlsx)
  library(rgeos)
  library(sp)
  library(rgdal)
}

### URLs to Arctic-HYCOS discharge data (station metadata and time series data)
#
# - station list (hosted by Global Runoff Data Centre, GRDC)
# - archive data (hosted by GRDC)
# - provisional (near real time) data (provided by national hydrological services) 
{
  # Arctic-HYCOS station list at GRDC (URL, zip, xlsx)
  {
    URL.arctic.hycos.station.list   = "ftp://ftp.bafg.de/pub/REFERATE/GRDC/catalogue/grdc_arctichycos_stations.zip"
    arctic.hycos.station.list.zip   = "grdc_arctichycos_stations.zip"
    arctic.hycos.station.list.xlsx  = "grdc_arctichycos_stations.xlsx"
    arctic.hycos.station.list       = "grdc_arctichycos_stations"
  }
  
  # Archive discharge data at GRDC
  {
    URL.archive.daily = "ftp://ftp.bafg.de/pub/REFERATE/GRDC/ARC_HYCOS/arc_hycos_day.zip"
    URL.archive.monthly = "ftp://ftp.bafg.de/pub/REFERATE/GRDC/ARC_HYCOS/arc_hycos_mon.zip"
    archive.daily.zip = "arc_hycos_day.zip"
    archive.monthly.zip = "arc_hycos_mon.zip"
    archive.daily = "archycos_day"
    archive.monthly = "archycos_month"
  }

}

### Function to set/create working folders on your local system
{
  create.local.working.folders<-function(wdir){
    # working folder
    if(!dir.exists(wdir)){
      dir.create(path = wdir,recursive = T)
    }
    setwd(wdir)
    
    # temporary download folder
    tmpdir = paste(wdir,"/download",sep="")
    if(!dir.exists(tmpdir)){
      dir.create(path = tmpdir,recursive = T)
    }
    
    # station list folder
    stndir = paste(wdir,"/stations",sep="")
    if(!dir.exists(stndir)){
      dir.create(path = stndir,recursive = T)
    }
    # data folders
    datadir = paste(wdir,"/data",sep="")
    if(!dir.exists(datadir)){
      dir.create(path = datadir,recursive = T)
    }
    arcdir = paste(datadir,"/archive",sep="")
    if(!dir.exists(arcdir)){
      dir.create(path = arcdir,recursive = T)
    }
    provdir = paste(datadir,"/provisional",sep="")
    if(!dir.exists(provdir)){
      dir.create(path = provdir,recursive = T)
    }
    
    # export folder
    exportdir = paste(wdir,"/export",sep="")
    if(!dir.exists(exportdir)){
      dir.create(path = exportdir,recursive = T)
    }
    
    # assign folder names to global variables
    assign("wdir",wdir,envir = .GlobalEnv)
    assign("tmpdir",tmpdir,envir = .GlobalEnv)
    assign("stndir",stndir,envir = .GlobalEnv)
    assign("datadir",datadir,envir = .GlobalEnv)
    assign("arcdir",arcdir,envir = .GlobalEnv)
    assign("provdir",provdir,envir = .GlobalEnv)
    assign("exportdir",exportdir,envir = .GlobalEnv)
    
    return(0)
  }
}

### Function to download and load station lists from GRDC
# GRDC Arctic-HYCOS station list:
#  - download station list URL (compressed excel file)
#  - import station list from compressed excel file
#  - transform to and export as spatial layers (KML or ESRI shapefile)
#
{
  download.arctic.hycos.station.list.from.GRDC<-function(forcedReload=F){
    # download file (if needed or if requested)
    if(forcedReload|!file.exists(paste(tmpdir,arctic.hycos.station.list.zip,sep="/"))){
      download.file(url = URL.arctic.hycos.station.list,destfile = paste(tmpdir,arctic.hycos.station.list.zip,sep="/"))
    }
    
    # unzip
    if(forcedReload|!file.exists(paste(stndir,"/",arctic.hycos.station.list.xlsx,sep=""))){
      unzip(zipfile = paste(tmpdir,arctic.hycos.station.list.zip,sep="/"),exdir = stndir)
    }
    
    # read station metadata from excelfile into data frame
    stn.metadata = read.xlsx(xlsxFile = paste(stndir,arctic.hycos.station.list.xlsx,sep="/"),sheet = 1)
   
    # read columns key into separate object  
    stn.metadata.key = read.xlsx(xlsxFile = paste(stndir,arctic.hycos.station.list.xlsx,sep="/"),sheet = 2)
    stn.metadata.key = stn.metadata.key[which(!is.na(stn.metadata.key[,1])),1:3]
    colnames(stn.metadata.key)<-c("Column","Name","Description")
    
    # export metadata and metadata key to textfile tables
    if(forcedReload|!file.exists(paste(stndir,"/",arctic.hycos.station.list,".txt",sep=""))){
      write.table(x = stn.metadata,file = paste(stndir,"/",arctic.hycos.station.list,".txt",sep=""),quote = F,sep = "\t",row.names = F)
    }
    if(forcedReload|!file.exists(paste(stndir,"/",arctic.hycos.station.list,"-column-key.txt",sep=""))){
      write.table(x = stn.metadata.key,file = paste(stndir,"/",arctic.hycos.station.list,"-column-key.txt",sep=""),quote = F,sep = "\t",row.names = F)
    }
    # make spatial points data frame
    stn.metadata.layer = stn.metadata
    coordinates(stn.metadata.layer)<-c("long","lat")
    proj4string(stn.metadata.layer) <- "+proj=longlat +ellps=WGS84"
    
    # export ESRI shapefile (column names are autmatically abbreviated by R)
    if(forcedReload|!file.exists(paste(stndir,"/",arctic.hycos.station.list,".shp",sep=""))){
      writeOGR(obj = stn.metadata.layer,dsn = stndir,layer = arctic.hycos.station.list,driver = "ESRI Shapefile",overwrite_layer = T)
    }
    
    # export KML file
    if(forcedReload|!file.exists(paste(stndir,"/",arctic.hycos.station.list,".kml",sep=""))){
      writeOGR(obj = stn.metadata.layer, dsn = paste(stndir,"/",arctic.hycos.station.list,".kml",sep=""),layer = arctic.hycos.station.list,driver = "KML",overwrite_layer = T)
    }
    
    # assign some global variables needed by other functions
    assign("stn.metadata",stn.metadata,envir = .GlobalEnv)
    assign("stn.metadata.key",stn.metadata.key,envir = .GlobalEnv)
    assign("stn.metadata.layer",stn.metadata.layer,envir = .GlobalEnv)
    
    return(0)
  }
}


### Function to download archive data from GRDC
{
  download.archive.data.from.GRDC<-function(forcedReload = F){
    # download daiy and monthly archive files (compressed folders)
    if(forcedReload|!file.exists(paste(tmpdir,"/",archive.daily.zip,sep=""))){
      download.file(url = URL.archive.daily,destfile = paste(tmpdir,"/",archive.daily.zip,sep=""))
    }
    if(forcedReload|!file.exists(paste(tmpdir,"/",archive.monthly.zip,sep=""))){
      download.file(url = URL.archive.monthly,destfile = paste(tmpdir,"/",archive.monthly.zip,sep=""))
    }
    # unzip to archive data folder
    unzip(zipfile = paste(tmpdir,"/",archive.daily.zip,sep=""),exdir = arcdir)
    unzip(zipfile = paste(tmpdir,"/",archive.monthly.zip,sep=""),exdir = arcdir)
    
    return(0)    
  }
}

### Search/Import functions
{
  
  # Support function to read a single GRDC text file into an R data frame:
  read.GRDC.discharge.data.file<-function(fname=NULL){
    if(file.exists(fname)){
      # open connection to read metadata
      {
        # open connection
        con = file(fname,open = "r")
        # read first 50 lines
        textin = readLines(con=con,n = 50)
        # identify the comment liens with a leading #
        ncom=length(which(substr(textin,1,1)=="#"))
        textout=textin[1:ncom]
        # close connection
        close(con)
        # extract the grdc_no from metadata
        grdc_no=0
        for(i in 1:ncom){
          if(nchar(textout[i])>=11){
            if(substr(textout[i],1,11)=="# GRDC-No.:"){
              grdc_no=as.integer(substr(textout[i],12,nchar(textout[i])))
            }
          }
        }
        
      }
      # read data
      {
        datain = read.delim(file = fname,header = T,sep = ";",comment.char = "#")
      }
      # transform datestring to date format (posix)
      {
        datenum=as.POSIXct(datain[,1],tz = "GMT")
      }
      # Daily or Monthly data?
      {
        if(ncol(datain)==3){
          isDaily=T
          isMOnthly=F
        }else{
          if(ncol(datain)==5){
            isDaily=F
            isMonthly=T
          }else{
            print("unknown GRDC format")
            return(-1)
          }
        }
      }
      # Create output data frame
      {
        if(isDaily){
          dataout = data.frame("date"=datenum,"value"=datain$Value)
        }else{
          if(isMonthly){
            dataout = data.frame("year"=as.integer(as.character(datenum,format="%Y")),"month"=as.integer(as.character(datenum,format="%m")),"date"=datenum,"original"=datain$Original,"calculated"=datain$Calculated,"flag"=datain$Flag)
          }
        }
        # metadata, grdc_no, and time resolution as data frame attributes
        attr(dataout,"comment")=textout
        attr(dataout,"grdc_no")=grdc_no
        if(isDaily){
          attr(dataout,"timeresolution")="daily"
        }else{
          if(isDaily){
            attr(dataout,"timeresolution")="monthly"
          }else{
            attr(dataout,"timeresolution")="unknown"
          }
        }
      }
      # Return
      {
        return(dataout)
      }
    }else{
      print("file does not exist")
      return(-1)
    } 
  }
  
  # Support function to find local files with GRDC data for a given station
  find.GRDC.discharge.data.files<-function(grdc_no){
    # initialize som counters
    no_daily=0
    no_monthly=0
    # loop over grdc_no
    for(i in 1:length(grdc_no)){
      # find daily files
      if(dir.exists(paste(arcdir,"/",archive.daily,sep=""))){
        daily.file = dir(path = paste(arcdir,"/",archive.daily,sep=""),pattern = as.character(grdc_no[i]))
        if(nchar(daily.file)>0){
          if(no_daily>0){
            daily.files = c(daily.files,paste(arcdir,"/",archive.daily,"/",daily.file,sep=""))
          }else{
            daily.files = paste(arcdir,"/",archive.daily,"/",daily.file,sep="")
          }
          no_daily=no_daily+1
        }
      }
      # find monthly files
      if(dir.exists(paste(arcdir,"/",archive.monthly,sep=""))){
        monthly.file = dir(path = paste(arcdir,"/",archive.monthly,sep=""),pattern = as.character(grdc_no[i]))
        if(nchar(monthly.file)>0){
          if(no_monthly>0){
            monthly.files = c(monthly.files,paste(arcdir,"/",archive.monthly,"/",monthly.file,sep=""))
          }else{
            monthly.files = paste(arcdir,"/",archive.monthly,"/",monthly.file,sep="")
          }
          no_monthly=no_monthly+1
        }
      }
    }
    if(no_daily>0 & no_monthly>0){
      return(list("no_daily"=no_daily,"no_monthly"=no_monthly,"daily.files"=daily.files,"monthly.files"=monthly.files))
    }else{
      if(no_daily>0){
        return(list("no_daily"=no_daily,"no_monthly"=no_monthly,"daily.files"=daily.files,"monthly.files"=NULL))
      }else{
        if(no_monthly>0){
          return(list("no_daily"=no_daily,"no_monthly"=no_monthly,"daily.files"=NULL,"monthly.files"=monthly.files))
        }else{
          return(list("no_daily"=no_daily,"no_monthly"=no_monthly,"daily.files"=NULL,"monthly.files"=NULL))
        }
      }
    }
  }
  
  # Support function to load GRDC archive data files into a list array
  load.GRDC.discharge.data.files<-function(grdc_no){
    # initialize output list array
    output=array(list(NULL),c(length(grdc_no),1))
    # loop over stations
    for(i in 1:length(grdc_no)){
      # find archive files in local storage
      {
        local.files = find.GRDC.discharge.data.files(grdc_no[i])
      }
      
      # load daily data
      if(local.files$no_daily>0){
        daily.data = read.GRDC.discharge.data.file(fname = local.files$daily.files[1])
      }else{
        daily.data = NULL
      }
      # load monthly data
      if(local.files$no_monthly>0){
        monthly.data = read.GRDC.discharge.data.file(fname = local.files$monthly.files[1])
      }else{
        monthly.data = NULL
      }
      # save to output list array
      output[[i]] = list("grdc_no"=grdc_no[i],"local.files"=local.files,"daily.data"=daily.data,"monthly.data"=monthly.data)
    }
    return(output)
  }
  
  # Main function to search and load Arctic-HYCOS station data
  get.Arctic.HYCOS.data<-function(grdc_no=NULL,station=NULL,river=NULL,country=NULL,read.archive.data=F){
    # select all (default)
    selected = vector(mode="logical",length=nrow(stn.metadata))
    selected[1:nrow(stn.metadata)] = T
    
    # filter by river
    if(!is.null(river)){
      iFind = grep(pattern = tolower(river),x = tolower(stn.metadata$river))
      if(length(iFind)>0){
        # remove all non-matching from selection
        iRem = which(is.na(match(1:nrow(stn.metadata),iFind)))
        selected[iRem]=F
      }else{
        # remove all
        selected[1:nrow(stn.metadata)]=F
      }
    }

    # filter by country code
    if(!is.null(country)){
      if(tolower(country)=="greenland"|country=="denmark"){
        country="DK"
      }
      if(tolower(country)=="iceland"){
        country="IS"
      }
      iFind = grep(pattern = tolower(substr(country,1,2)),x = tolower(stn.metadata$country))
      if(length(iFind)>0){
        # remove all non-matching from selection
        iRem = which(is.na(match(1:nrow(stn.metadata),iFind)))
        selected[iRem]=F
      }else{
        # remove all
        selected[1:nrow(stn.metadata)]=F
      }
    }
    
    # filter by station name
    if(!is.null(station)){
      iFind = grep(pattern = tolower(station),x = tolower(stn.metadata$station))
      if(length(iFind)>0){
        # remove all non-matching from selection
        iRem = which(is.na(match(1:nrow(stn.metadata),iFind)))
        selected[iRem]=F
      }else{
        # remove all
        selected[1:nrow(stn.metadata)]=F
      }
    }
    
    # filter by grdc_no
    if(!is.null(grdc_no)){
      iFind = match(grdc_no,stn.metadata$grdc_no)
      if(length(iFind)>0){
        # remove all non-matching from selection
        iRem = which(is.na(match(1:nrow(stn.metadata),iFind)))
        selected[iRem]=F
      }else{
        # remove all
        selected[1:nrow(stn.metadata)]=F
      }
    }
    
    # Add more filters, aoi, darea, downstream/upstream, etc
    
    
    # load discharge data if requested, or return search result only
    if(length(which(selected))>0){
      output = list("no_stations"=length(which(selected)),"grdc_no"=stn.metadata$grdc_no[which(selected)],"stn.metadata"=stn.metadata[which(selected),],"stn.metadata.layer"=stn.metadata.layer[which(selected),],"archive.discharge.data"=NULL)
      if(read.archive.data){
        output$archive.discharge.data = load.GRDC.discharge.data.files(grdc_no = output$grdc_no)
      }
    }else{
      print("no station found")
      ouptut = list("no_stations"=0,"grdc_no"=NULL,"stn.metadata"=NULL,"stn.metadata.layer"=NULL,"archive.discharge.data"=NULL)
    }
    
    return(output)
  }
  
}

### Export function
{
 
  # support function to export discharge data in a csv-format used in the H-TEP portal   
  write.htep.csv.files<-function(archive.data,outdir){
    # csv format defined for the Hydrology TEP portal (by Terradue, SMHI, and others):
    #
    # id, timestamp, Longitude, Latitude, uom, value, additional,...
    #
    # Daily data format:
    # ------------------
    # id, timestamp, Longitude, Latitude, uom, value, grdc_no, station, river, country 
    #
    #
    # Monthly data format:
    # ------------------
    # id, timestamp, Longitude, Latitude, uom, value, calculated, flag, grdc_no, station, river, country
    #
    # where value is the "original" provided value, and "calculated" is the calculated from daily, and flag is flag, according to GRDC data.
    
    # prepare data for daily and monthly data frames
    numDaily=0
    numMonthly=0
    for(i in 1:archive.data$no_stations){
      if(!is.null(archive.data$archive.discharge.data[[i]]$daily.data)){
        numRec = nrow(archive.data$archive.discharge.data[[i]]$daily.data)
        if(numDaily==0){
          dailyValues = archive.data$archive.discharge.data[[i]]$daily.data$value
          dailyTimes  = archive.data$archive.discharge.data[[i]]$daily.data$date
          dailyLong   = rep(x = archive.data$stn.metadata$long[i],each=numRec)
          dailyLat    = rep(x = archive.data$stn.metadata$lat[i],each=numRec)
          dailyUom    = rep(x = "m3/s",each=numRec)
          dailyGRDC   = rep(x = archive.data$stn.metadata$grdc_no[i],each=numRec)
          dailyStn    = rep(x = archive.data$stn.metadata$station[i],each=numRec)
          dailyRiver  = rep(x = archive.data$stn.metadata$river[i],each=numRec)
          dailyCountry= rep(x = archive.data$stn.metadata$country[i],each=numRec)
        }else{
          dailyValues = c(dailyValues,archive.data$archive.discharge.data[[i]]$daily.data$value)
          dailyTimes  = c(dailyTimes,archive.data$archive.discharge.data[[i]]$daily.data$date)
          dailyLong   = c(dailyLong,rep(x = archive.data$stn.metadata$long[i],each=numRec))
          dailyLat    = c(dailyLat,rep(x = archive.data$stn.metadata$lat[i],each=numRec))
          dailyUom    = c(dailyUom,rep(x = "m3/s",each=numRec))
          dailyGRDC   = c(dailyGRDC,rep(x = archive.data$stn.metadata$grdc_no[i],each=numRec))
          dailyStn    = c(dailyStn,rep(x = archive.data$stn.metadata$station[i],each=numRec))
          dailyRiver  = c(dailyRiver,rep(x = archive.data$stn.metadata$river[i],each=numRec))
          dailyCountry= c(dailyCountry,rep(x = archive.data$stn.metadata$country[i],each=numRec))
        }
        numDaily = numDaily + numRec
      }
      if(!is.null(archive.data$archive.discharge.data[[i]]$monthly.data)){
        numRec = nrow(archive.data$archive.discharge.data[[i]]$monthly.data)
        if(numMonthly==0){
          monthlyValues = archive.data$archive.discharge.data[[i]]$monthly.data$original
          monthlyCalc   = archive.data$archive.discharge.data[[i]]$monthly.data$calculated
          monthlyFlag   = archive.data$archive.discharge.data[[i]]$monthly.data$flag
          monthlyTimes  = archive.data$archive.discharge.data[[i]]$monthly.data$date
          monthlyLong   = rep(x = archive.data$stn.metadata$long[i],each=numRec)
          monthlyLat    = rep(x = archive.data$stn.metadata$lat[i],each=numRec)
          monthlyUom    = rep(x = "m3/s",each=numRec)
          monthlyGRDC   = rep(x = archive.data$stn.metadata$grdc_no[i],each=numRec)
          monthlyStn    = rep(x = archive.data$stn.metadata$station[i],each=numRec)
          monthlyRiver  = rep(x = archive.data$stn.metadata$river[i],each=numRec)
          monthlyCountry= rep(x = archive.data$stn.metadata$country[i],each=numRec)
        }else{
          monthlyValues = c(monthlyValues,archive.data$archive.discharge.data[[i]]$monthly.data$original)
          monthlyCalc   = c(monthlyCalc,archive.data$archive.discharge.data[[i]]$monthly.data$calculated)
          monthlyFlag   = c(monthlyFlag,archive.data$archive.discharge.data[[i]]$monthly.data$flag)
          monthlyTimes  = c(monthlyTimes,archive.data$archive.discharge.data[[i]]$monthly.data$date)
          monthlyLong   = c(monthlyLong,rep(x = archive.data$stn.metadata$long[i],each=numRec))
          monthlyLat    = c(monthlyLat,rep(x = archive.data$stn.metadata$lat[i],each=numRec))
          monthlyUom    = c(monthlyUom,rep(x = "m3/s",each=numRec))
          monthlyGRDC   = c(monthlyGRDC,rep(x = archive.data$stn.metadata$grdc_no[i],each=numRec))
          monthlyStn    = c(monthlyStn,rep(x = archive.data$stn.metadata$station[i],each=numRec))
          monthlyRiver  = c(monthlyRiver,rep(x = archive.data$stn.metadata$river[i],each=numRec))
          monthlyCountry= c(monthlyCountry,rep(x = archive.data$stn.metadata$country[i],each=numRec))
        }
        numMonthly = numMonthly + numRec
      }
    }
    if(numDaily>0){
      daily.table = data.frame(id=seq(1,numDaily,1),
                               timestamp = dailyTimes,
                               longitude = dailyLong,
                               latitude  = dailyLat,
                               uom       = dailyUom,
                               value     = dailyValues,
                               grdc_no   = dailyGRDC,
                               station   = dailyStn,
                               river     = dailyRiver,
                               country   = dailyCountry
                               ) 
    }else{
      daily.table = NULL
    }
    if(numMonthly>0){
      monthly.table = data.frame(id=seq(1,numMonthly,1),
                               timestamp = monthlyTimes,
                               longitude = monthlyLong,
                               latitude  = monthlyLat,
                               uom       = monthlyUom,
                               value     = monthlyValues,
                               calculated= monthlyCalc,
                               flag      = monthlyFlag,
                               grdc_no   = monthlyGRDC,
                               station   = monthlyStn,
                               river     = monthlyRiver,
                               country   = monthlyCountry
      ) 
    }else{
      monthly.table = NULL
    }
    
    # write csv files
    if(!is.null(daily.table)){
      write.table(daily.table,file=paste(outdir,"/daily.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)
    }
    if(!is.null(monthly.table)){
      write.table(monthly.table,file=paste(outdir,"/monthly.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)
    }
    return(0)
  }
 
  # Support function to write HYPE model PTQobs format (from HYPEtools)
  WritePTQobs<-function (x, filename, dt.format = "%Y-%m-%d", digits = 3, nsmall = 1, obsid = NULL){
    
    ## check if consistent header information is available, obsid arguments take precedence before attribute
    if(!is.null(obsid)) {
      if (length(obsid) == ncol(x) - 1) {
        header <- c("DATE", obsid)
      } else {
        stop("Length of function argument 'obsid' does not match number of obsid columns in export object.")
      }
    } else if (!is.null(attr(x, which = "obsid"))) {
      if (length(attr(x, which = "obsid")) == ncol(x) - 1) {
        header <- c("DATE", attr(x, which = "obsid"))
      } else {
        stop("Length of attribute 'obsid' does not match number of obsid columns in export object.")
      }
    } else {
      stop("No information available from 'obsid' argument or 'obsid' attribute to construct export header.")
    }
    
    # date conversion, conditional on that the date column is a posix class
    if (any(class(x[, 1]) == "POSIXct")) {
      x[, 1] <- format(x[, 1], format = dt.format)
    } else {
      warning("First column in export data frame is not of class 'POSIXct', will be exported unchanged.")
    }
    
    # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
    x[is.na(x)] <- -9999
    
    # export
    write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
                quote = FALSE, sep = "\t", row.names = FALSE, col.names = header)
    
  }
  
  # Support function to write HYPE model Xobs format (from HYPEtools)
  WriteXobs <- function(x, filename = "Xobs.txt", append = F, comment = NA, variable = NA, subid = NA, 
                        lastDate = NA, timestep = "d") {
    if (!append) {
      # no appending, just write to a new file
      
      # Export of comment (if it exists) and header information, uses a cascade of conditional checks
      # which will stop the export with informative error messages if inconsistencies are found
      
      # remove existing export file. this makes it possible to have a consistent 'append' connection open below, instead of
      # overwriting first and then reopening in append mode...
      invisible(suppressWarnings(file.remove(filename)))
      
      # create and open a connection to which the following writeLines() will be written
      fcon <- file(description = filename, open ="at")
      
      ## export comment line
      if (is.na(comment)){
        # comment argument is empty
        if(!is.null(attr(x, which = "comment"))) {
          # comment attribute exists, export
          writeLines(paste(attr(x, which = "comment"), collapse = "\t"), con = fcon)
        } else {
          # comment attribute does not exist, export the following string
          writeLines("Exported from R", con = fcon)
        }
      } else {
        # export comment argument
        writeLines(comment, con = fcon)
      }
      
      ## export variable line
      if (length(variable) == 1 & is.na(variable[1])) {
        # variable argument is empty
        if(!is.null(attr(x, which = "variable"))) {
          # attribute variable exists
          if (length(attr(x, which = "variable")) == ncol(x) - 1) {
            # attribute and export dataframe match in length, export attribute with padded 'name' string and newline
            tmp <- paste(c("x", attr(x, which = "variable")), collapse = "\t")
            writeLines(tmp, con = fcon)
          } else {
            # mismatch in length, stop with error
            close(fcon)
            stop("Length of attribute 'variable' does not match number of variables in export object.\n 
                 Check consistency, e.g. with attr(x, 'variable') and names(x)[-1].")
          }
          } else {
            # attribute variable does not exist, stop with error
            close(fcon)
            stop("'variable' argument not given and 'variable' attribute not existing in export object.")
        }
      } else {
        # export the variable argument with padded 'name' string and newline, if length matches no. of observation data cols in x
        if (length(variable) == ncol(x) - 1) {
          tmp <- paste(c("x", variable), collapse ="\t")
          writeLines(tmp, con = fcon)
        } else {
          # mismatch in length, stop with error
          close(fcon)
          stop("Length of argument 'variable' does not match number of variables in export object.")
        }
      }
      
      ## export subid line
      if (length(subid == 1) & is.na(subid[1])) {
        # subid argument is empty
        if(!is.null(attr(x, which = "subid"))) {
          # attribute subid exists
          if (length(attr(x, which = "subid")) == ncol(x) - 1) {
            # attribute and export dataframe match in length, export attribute with padded 0 and newline
            tmp <- paste(as.character(c(0, attr(x, which = "subid"))), collapse = "\t")
            writeLines(tmp, con = fcon)
          } else {
            # mismatch in length, stop with error
            close(fcon)
            stop("Length of attribute 'subid' does not match number of variables in export object.\n 
                 Check consistency, e.g. with attr(x, 'subid') and names(x)[-1].")
          }
          } else {
            # attribute subid does not exist, stop with error
            close(fcon)
            stop("'subid' argument not given and 'subid' attribute not existing in export object.")
        }
      } else {
        # export the subid argument with padded 0 and newline, if length matches no. of observation data cols in x
        if (length(subid) == ncol(x) - 1) {
          tmp <- paste(as.character(c(0, subid)), collapse = "\t")
          writeLines(tmp, con = fcon)
        } else {
          # mismatch in length, stop with error
          close(fcon)
          stop("Length of argument 'subid' does not match number of variables in export object.")
        }
      }
      
      close(fcon)
      
      
      
    } else {
      # export will be appended to existing file
      
      # check if file to export to exists, stop otherwise
      stopifnot(file.exists(filename))
      
      # read variable names from existing file into two vectors, to be compared against the export data for consistency
      tmp <- readLines(filename,n=3)
      existingVar <- strsplit(tmp[2], split = "\t")[[1]][-1]
      existingSbd <- as.integer(strsplit(tmp[3], split = "\t")[[1]][-1])
      
      # first consistency check: number of columns identical?
      if (length(existingVar) != ncol(x) - 1) {
        stop("Inconsistent number of data columns between export object and existing file.")
      }
      
      ## second consistency check: variables and SUBIDs identical and in the same order?
      # select export data to compare with, either from function argument or from attribute of x
      if (!is.na(variable[1])) {
        exportVar <- variable
      } else {
        if (!is.null(attr(x, "variable"))) {
          exportVar <- attr(x, "variable")
        } else {
          stop("'variable' argument not given and 'variable' attribute not existing in export object.")
        }
      }
      
      if (!is.na(subid[1])) {
        exportSbd <- subid
      } else {
        if (!is.null(attr(x, "subid"))) {
          exportSbd <- attr(x, "subid")
        } else {
          stop("'subid' argument not given and 'subid' attribute not existing in export object.")
        }
      }
      
      # check consistency, will fail if at least one column is different for either subid or variable
      if (any(!(exportVar == existingVar), !(exportSbd == existingSbd))) {
        stop("Inconsistent variable names or SUBIDs.")
      }
      
      ## third consistency check: is last date in existing Xobs earlier than the first export row?
      ## split into hour and day cases
      if (timestep == "h" | timestep == "hourly") {
        tdiff <- difftime(x[1,1], lastDate, units = "hours")
        if (tdiff < 1) {
          stop("Time series in existing and new Xobs overlap or difference is smaller than one hour.")
        }
        ## export '-9999' lines if gap is larger than one hour
        # create date vector
        dpad <- seq(from = lastDate, to = x[1,1], length = tdiff + 1)
        # cut off end and start dates
        dpad <- dpad[-c(1, length(dpad))]
        # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
        pad <- cbind(format(dpad, format = "%Y-%m-%d %H:%M"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                                   ncol = ncol(x) - 1)))
        write.table(pad, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
      }
      
      if (timestep == "d" | timestep == "daily") {
        tdiff <- difftime(x[1,1], lastDate, units = "days")
        if (tdiff < 1) {
          stop("Time series in existing and new Xobs overlap or difference is smaller than one day.")
        }
        ## export '-9999' lines if gap is larger than one day
        # create date vector
        dpad <- seq(from = lastDate, to = x[1,1], length = tdiff + 1)
        # cut off end and start dates
        dpad <- dpad[-c(1, length(dpad))]
        # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
        pad <- cbind(format(dpad, format = "%Y-%m-%d"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                             ncol = ncol(x) - 1)))
        write.table(pad, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
      }
    }
    
    # Export of the dataframe, format date-times to HYPE requirements first
    if (timestep == "d" | timestep == "daily") {
      x[,1] <- format(x[,1], format = "%Y-%m-%d")
    }
    if (timestep == "h" | timestep == "hourly") {
      x[,1] <- format(x[,1], format = "%Y-%m-%d %H:%M")
    }
    write.table(x, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
    
    
  }
  
  # Support function to export discharge data in a text-format used by the HYPE model
  write.hype.obs.files<-function(archive.data,outdir,grdc2hype=NULL){
    # Format 1:
    # --------
    # this is a simple text format useful for multi-station time-series with single variables:
    #
    # each timeseries is identified by the stantion id number in the header:
    #
    # date stnid.1 stnid.2 ...
    # yyyy-mm-dd value.1 value.2 ...
    #
    # Format 2:
    # ---------
    # if more than one variable is needed per station, variable name can be given 
    # in a second header:
    #
    # date var1 var2 var1 var2
    # 0    stn1 stn1 stn2 stn2
    # ...
    #
    # Format 1 is used for the daily data, and format 2 for the monthly data
    # ----------------------------------------------------------------------
    
    # Daily data (format 1 - "Qobs" format)
    {
      # Prepare date vector and id of available stations
      {
        numDaily=0
        for(i in 1:archive.data$no_stations){
          if(!is.null(archive.data$archive.discharge.data[[i]]$daily.data)){
            if(nrow(archive.data$archive.discharge.data[[i]]$daily.data)>0){
              if(numDaily==0){
                idDaily=archive.data$archive.discharge.data[[i]]$grdc_no
                dates=archive.data$archive.discharge.data[[i]]$daily.data$date
                dateStart=dates[1]
                dateEnd=dates[length(dates)]
              }else{
                idDaily=c(idDaily,archive.data$archive.discharge.data[[i]]$grdc_no)
                dates=archive.data$archive.discharge.data[[i]]$daily.data$date
                dateStart=min(dateStart,dates[1])
                dateEnd=max(dateEnd,dates[length(dates)])
              }
              numDaily=numDaily+1
            }
          }
        }
        if(numDaily>0){
          dateVector=seq(from = dateStart,to = dateEnd, by = "days")
        }
      }
      # continue only if any daily data is present in the search result
      if(numDaily>0){
        # Change station id to HYPE model subid if requested
        {
          if(!is.null(grdc2hype)){
            obsid = grdc2hype$subid[match(idDaily,grdc2hype$grdc_no)]
          }else{
            obsid = idDaily
          }
        }
        # Create output data frame
        {
          # initialize data frame with datevector as first variable
          dailyOut = data.frame("date"=dateVector)
          jDaily=0
          # loop over stations, and add the daily discharge values in new variables
          for(i in 1:archive.data$no_stations){
            if(!is.null(archive.data$archive.discharge.data[[i]]$daily.data)){
              if(nrow(archive.data$archive.discharge.data[[i]]$daily.data)>0){
                jDaily=jDaily+1
                # variable name = obsid number (station id or HYPE model subid, see previous section)
                varName=as.character(obsid[jDaily])
                # initialize with missing vobsidlue
                dailyOut[,varName]=NA
                # match station records to the output datevector
                iStn = match(dailyOut$date,archive.data$archive.discharge.data[[i]]$daily.data$date)
                iOut = which(!is.na(iStn))
                iStn = iStn[iOut]
                dailyOut[iOut,varName]=archive.data$archive.discharge.data[[i]]$daily.data$value[iStn]
              }
            }
          }
          attr(dailyOut,"obsid")=obsid
        }
        # Change -999 to NA
        {
          for(i in 2:ncol(dailyOut)){
            dailyOut[which(dailyOut[,i]==-999),i]=NA
          }
        }
        # Export to output folder
        {
          WritePTQobs(x = dailyOut,filename = paste(outdir,"/hype-Qobs-format-daily-data.txt",sep=""))
        }
      }
    }
    
    # Monthly data (format 2 - "Xobs" format)
    {
      # Prepare date vector, vector with id of available stations, and vector of output variables
      {
        numMonthly=0
        for(i in 1:archive.data$no_stations){
          if(!is.null(archive.data$archive.discharge.data[[i]]$monthly.data)){
            if(nrow(archive.data$archive.discharge.data[[i]]$monthly.data)>0){
              if(numMonthly==0){
                # vector of output variables, 3 monthly variables for each station:
                # (Original, Calculated, Flag, abbreviated to ORIG, CALC, FLAG)
                varVector=c("ORIG","CALC","FLAG")
                
                # vector of station id number (each repeted 3 times for 3 variables)
                idMonthly=rep(archive.data$archive.discharge.data[[i]]$grdc_no,each=3)
                
                # first and last date in time series
                dates=archive.data$archive.discharge.data[[i]]$monthly.data$date
                dateStart=dates[1]
                dateEnd=dates[length(dates)]
                # also, prepare a 
              }else{
                varVector=c(vars,c("ORIG","CALC","FLAG"))
                idMonthly=c(idMonthly,rep(archive.data$archive.discharge.data[[i]]$grdc_no,each=3))
                dates=archive.data$archive.discharge.data[[i]]$monthly.data$date
                dateStart=min(dateStart,dates[1])
                dateEnd=max(dateEnd,dates[length(dates)])
              }
              numMonthly=numMonthly+1
            }
          }
        }
        if(numMonthly>0){
          dateVector=seq(from = dateStart,to = dateEnd, by = "months")
        }
      }
      # Continue only if any monthly data is present in the search result
      if(numMonthly>0){
        # Change station id to HYPE model subid if requested
        {
          if(!is.null(grdc2hype)){
            obsid = grdc2hype$subid[match(idMonthly,grdc2hype$grdc_no)]
          }else{
            obsid = idMonthly
          }
        }
        # Create output data frame
        {
          # initialize data frame with datevector as first variable
          monthlyOut = data.frame("date"=dateVector)
          jMonthly=-2:0
          # loop over stations, and add the monthly data in new variables
          for(i in 1:archive.data$no_stations){
            if(!is.null(archive.data$archive.discharge.data[[i]]$monthly.data)){
              jMonthly=jMonthly+3
              # variable name = VAR1[obsid number] (station id or HYPE model subid, see previous section)
              varNames=paste(varVector[jMonthly],as.character(obsid[jMonthly]),sep="")
              
              # initialize with missing value
              monthlyOut[,varNames]=NA
              
              # match station records to the output datevector
              iStn = match(monthlyOut$date,archive.data$archive.discharge.data[[i]]$monthly.data$date)
              iOut = which(!is.na(iStn))
              iStn = iStn[iOut]
              monthlyOut[iOut,varNames[1]]=archive.data$archive.discharge.data[[i]]$monthly.data$original[iStn]
              monthlyOut[iOut,varNames[2]]=archive.data$archive.discharge.data[[i]]$monthly.data$calculated[iStn]
              monthlyOut[iOut,varNames[3]]=archive.data$archive.discharge.data[[i]]$monthly.data$flag[iStn]
            }
          }
          attr(monthlyOut,"subid")=obsid
          attr(monthlyOut,"variable")=varVector
        }
        # Change -999 to NA
        {
          for(i in 2:ncol(monthlyOut)){
            monthlyOut[which(monthlyOut[,i]==-999),i]=NA
          }
        }
        # Export to output folder
        {
          WriteXobs(x = monthlyOut,filename = paste(outdir,"/hype-Xobs-format-monthly-data.txt",sep=""),comment = "Arctic-HYCOS monthly mean discharge data (m3/s), ORIG=originally provided by National hydrological services to GRDC, CALC=calculated by GRDC from daily data, FLAG=percentage of daily values used for calculated values. Missing data = -9999. Station metadata is provided in separate files, see [exportdir]/metadata/station.metadata.*")
        }
      }
    }
    return(0)
  }

  # Main export function - exports metadata and discharge data in different formats
  export.archive.data<-function(archive.data,subdir=NULL,exportMetadata=T,exportOriginal=T,exportCSV=T,exportHYPE=F,grdc2hype=NULL){
    # create export sub-folders
    {
      if(is.null(subdir)){
        subdir=paste("exported_",format(Sys.time(),"%Y.%d.%m_%H.%M.%S"),sep="")
      }
      if(exportMetadata){
        if(!dir.exists(paste(exportdir,"/",subdir,"/metadata",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/metadata",sep=""),recursive = T)
        }
      }
      if(exportOriginal|exportCSV|exportHYPE){
        if(!dir.exists(paste(exportdir,"/",subdir,"/data",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/data",sep=""),recursive = T)
        }
      }
      if(exportOriginal){
        if(!dir.exists(paste(exportdir,"/",subdir,"/data/grdc.original/daily",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/data/grdc.original/daily",sep=""),recursive = T)
        }
        if(!dir.exists(paste(exportdir,"/",subdir,"/data/grdc.original/monthly",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/data/grdc.original/monthly",sep=""),recursive = T)
        }
      }
      if(exportCSV){
        if(!dir.exists(paste(exportdir,"/",subdir,"/data/csvfiles",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/data/csvfiles",sep=""),recursive = T)
        }
      }
      if(exportHYPE){
        if(!dir.exists(paste(exportdir,"/",subdir,"/data/hypeobsfiles",sep=""))){
          dir.create(paste(exportdir,"/",subdir,"/data/hypeobsfiles",sep=""),recursive = T)
        }
      }
    }
    
    # export station metadata
    {
      if(exportMetadata){
        # export metadata and metadata key to textfile tables
        write.table(x = archive.data$stn.metadata,file = paste(exportdir,"/",subdir,"/metadata/","station.metadata.txt",sep=""),quote = F,sep = "\t",row.names = F)
        write.table(x = stn.metadata.key,file = paste(exportdir,"/",subdir,"/metadata/","station.metadata-column-key.txt",sep=""),quote = F,sep = "\t",row.names = F)
        
        # export ESRI shapefile (column names are autmatically abbreviated by R)
        writeOGR(obj = archive.data$stn.metadata.layer,dsn = paste(exportdir,"/",subdir,"/metadata",sep=""),layer = "station.metadata",driver = "ESRI Shapefile",overwrite_layer = T)
        
        # export KML file
        writeOGR(obj = archive.data$stn.metadata.layer, dsn = paste(exportdir,"/",subdir,"/metadata/","station.metadata.kml",sep=""),layer = "station.metadata",driver = "KML",overwrite_layer = T)
      }
    }
    
    # export original GRDC data files
    {
      if(exportOriginal){
        # copy original GRDC files
        for(i in 1:archive.data$no_stations){
          # daily files
          for(j in 1:archive.data$archive.discharge.data[[i]]$local.files$no_daily){
            file.copy(from = archive.data$archive.discharge.data[[i]]$local.files$daily.files[j]
                      ,to = paste(exportdir,"/",subdir,"/data/grdc.original/daily",sep="")
                      ,overwrite = T)
          }
          # monthly files
          for(j in 1:archive.data$archive.discharge.data[[i]]$local.files$no_monthly){
            file.copy(from = archive.data$archive.discharge.data[[i]]$local.files$monthly.files[j]
                      ,to = paste(exportdir,"/",subdir,"/data/grdc.original/monthly",sep="")
                      ,overwrite = T)
          }
        }
      }
    }
    
    
    # export csv files (H-TEP format) with daily and monthly data
    {
      if(exportCSV){
        write.htep.csv.files(archive.data
                             ,outdir = paste(exportdir,"/",subdir,"/data/csvfiles",sep=""))
      }
    }
      
    # export HYPE-model obs-files with daily and monthly data
    {
      if(exportHYPE){
        write.hype.obs.files(archive.data
                             ,outdir = paste(exportdir,"/",subdir,"/data/hypeobsfiles",sep="")
                             ,grdc2hype = grdc2hype)
      }
    }
    
    return(0)
  }
}
