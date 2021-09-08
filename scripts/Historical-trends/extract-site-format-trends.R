####################################################
##    ExTRACT DATA FROM NOAA CLIMGRID   ############
####################################################

# 09-07-2021
# written by Amber Runyon and Annie Kellner

# This script extracts historical data from NOAA ClimGrid
# This data needs to be downloaded one time, and may be stored on an external hard drive (data ~ 16GB)
# See CCRP's noaa-nClimGrid repository for scripts that will download and unzip tar files from the NOAA FTP 


park_specific = './data/park-specific' # Create folder for park-specific data (i.e. parsed data and output)
if(dir.exists(park_specific) == FALSE){
  dir.create(park_specific)
}


parsed_data = './data/park-specific/parsed-data'
if(dir.exists(parsed_data) == FALSE){
  dir.create(parsed_data)
}

OutDir <- paste0(parsed_data, "/")

AoaExt <- extent(Lon-Buffer, Lon+Buffer, Lat-Buffer, Lat+Buffer)

var.list = c("tmax", "tmin", "prcp")

for (v in 1:length(var.list)){
  DF <- data.frame()
  var = var.list[v]
  print(paste0("extracting ",var))
  pnt.list<-list.files(path=data.dir, pattern=paste(var, "conus", sep = ".")) #list all files by var
  #tmax, tmin, tave, prcp
  
  # Create list of tables
  
  for(i in 1:length(pnt.list)){
    t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
    yrmon = substr(pnt.list[i], 1, 6) 
    colnames(t) = c("Lat","Lon", var)
    tt = subset(t, Lat >= AoaExt@ymin & Lat <= AoaExt@ymax &
                  Lon >=AoaExt@xmin & Lon<=AoaExt@xmax)
    tt$Date = as.Date(paste0(yrmon,"01"),format="%Y%m%d")
    tt.merge = aggregate(.~Date,tt,mean)
    tt.merge$YearMon <- yrmon
    DF = rbind(DF,tt.merge)
  }
  assign(paste0(var,".data"), DF)
  # write.csv(DF,paste0(OutDir,var,"_",SiteID,".csv"),row.names = F)
}

#  Deal with dates and seasons
GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

prcp.data %>%
  mutate(PptIn = prcp/25.4) %>%   # mm to in
  mutate(Season = GetSeason(Date)) -> PptMeans

tmax.data %>%
  mutate(TmaxF = tmax * 9/5 + 32) %>%
  mutate(Season = GetSeason(Date)) -> TmaxMeans

tmin.data %>% 
  mutate(TminF = tmin * 9/5 + 32) %>%
  mutate(Season = GetSeason(Date)) -> TminMeans

rm(DF, prcp.data, t, tave.data, tmax.data, tmin.data, tt, tt.merge)


save.image(sprintf("%s%s_%s_%s_nClimGrid_IntermediateFiles.RData", OutDir, SiteID, Lat, Lon))

