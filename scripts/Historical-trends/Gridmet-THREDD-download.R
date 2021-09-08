#######################################################################################
# Script to download daily gridmet data
# Aggregated data available here: http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
#######################################################################################

dir.create('./data/park-specific/parsed-data/Gridmet') # Create folder to store .nc files from GridMET

data.dir <- "./data/park-specific/parsed-data/Gridmet/"

var = c("pr","tmmx","tmmn")
longVar = c("precipitation_amount", "daily_maximum_temperature", "daily_minimum_temperature")


#Set working directory where files will download


# Download data - takes ~30 sec / var
for (i in 1:length(var)){
    writeLines("")
  print(paste("Downloading", longVar[i], "data"))
  writeLines("")
  x<-paste("http://thredds.northwestknowledge.net:8080/thredds/ncss/agg_met_",var[i],"_1979_CurrentYear_CONUS.nc?var=",longVar[i],
         "&north=",box[1],"&west=",box[4],"&east=",box[3],"&south=",box[2],"&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=",
         startDate,"T00%3A00%3A00Z&time_end=",endDate,"T00%3A00%3A00Z&timeStride=1&accept=netcdf",sep="")
  download.file(url=x, destfile=paste0(data.dir,var[i],".nc"),method="auto",quiet=FALSE,mode="wb",cacheOK=TRUE)
}

# parsing data
files <- list.files(data.dir)

for(i in 1:length(files)){
  nc<-nc_open(paste0(data.dir,var[i],".nc")) 
  varName <- names(nc$var)
  varUnits <- ncatt_get(nc, varName, "units")$value
  All_lat <- data.frame(nc$dim$lat$vals)
  All_lon <- data.frame(nc$dim$lon$vals)
  Lat_index = as.numeric(which.min(abs(All_lat$nc.dim.lat.vals - Lat)))
  Lon_index = as.numeric(which.min(abs(All_lon$nc.dim.lon.vals - Lon)))

  Date <- as.Date(nc$dim$day$vals, origin = "1900-01-01")
  Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))
  if(varUnits == "mm"){
    Extr <- Extr/25.4
  } else {(varUnits == "K")
    Extr <- (Extr * (9/5)) - 459.67
  }
  Data <- data.frame(Date, Extr)
  nc_close(nc)
  names(Data)[2] <- var[i] 
  Data1<-Data[order(Date),]
  if (var[i] == var[1]){GridMet<-Data} else {GridMet<-cbind(GridMet,Data1[2])} 
}

#set names so match output from other scripts -- need to change if alter variables
names(GridMet)<-c("Date","precip","tmax","tmin")

write.csv(GridMet,"./data/park-specific/parsed-data/GridMet.csv",row.names=F)

# Remove saved climate files
if(Remove_files == "Y") {
  do.call(file.remove, list(list.files(data.dir, pattern = '.nc', full.names = TRUE)))
  print("Files removed")
} else {print("Files remain")}
