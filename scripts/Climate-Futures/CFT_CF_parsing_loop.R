## CFT_CF_parsing_loop -- parse MACA data using CFT package and format for reading through RSS scripts
# functions same as CFT_CF_parsing_v1 except reads in park.csv and centroids.csv and loops through downloading and formatting
# Output same as RSS_MACA_Parsing.R, saved as SiteID_init_parsed.RData
# To be replaced once Rmd written from RCF project
# A. Runyon 20201021

#Units: temp = K, precip = mm, Rh = %, vpd = kPa

library(cft)
library(tibble)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggrepel)
library(dplyr)
library(sp)
library(sf)

rm(list=ls())

DataDir <- "C:/Users/msears/Documents/FY21-Parks/" #local site for saving data/intermed files
ParkDir<-"C:/Users/msears/Documents/FY21-Parks/"
source <- "RSS_1" #RSS or SFIP -- subset data so only running one at a time.  MGS added "RSS_1" to loop small # of parks at a time

parks<-read.csv(paste(DataDir, "FY21_parks.csv",sep=""))
centroids<-read.csv(paste(DataDir, "NPS_Boundary_Centroids.csv",sep=""))

centroids<-plyr::rename(centroids, replace=c("UNIT_CODE"="Park"))
parks<-merge(parks,centroids,by="Park")

parks<-subset(parks, Source == source)

## Download data
#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin","rhsmax","rhsmin","vpd")
scens = c("rcp45", "rcp85")

# GCMs to be extracted
GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2006   #2006-2099
Future_EndYear = 2099   #2006-2099
Hist_StartYear = 1950    #1950-2005
Hist_EndYear = 2005      #1950-2005

Remove_files = "Y" # "N"       #Removes all climate data files saved in directory

############################## END INITIALS ##################################################

## Park data
for (i in 1:nrow(parks)){
SiteID <- parks$Park[i]
Lat <- parks$LAT[i]
Lon = parks$LONG[i]

# Now can only use spatial object (not park name)
Site_coordinates <- data.frame(PARK=SiteID,lat=Lat,lon=Lon)
coordinates(Site_coordinates) <- ~lon+lat
proj4string(Site_coordinates) <- "+proj=longlat +datum=NAD83 +no_defs " #same proj4string used in NPS_boundary_centroids.shp


# download data
file_refs <- cftdata(aoi = Site_coordinates, 
                     area_name = SiteID,
                     years = c(Hist_StartYear, Future_EndYear),
                     models = GCMs,
                     local_dir = DataDir,
                     parameters = vars,
                     scenarios = scens,
                     ncores = parallel::detectCores() / 2)
glimpse(file_refs)
df <- cft_df(file_refs, ncores = parallel::detectCores() / 2)
glimpse(df)

######################## MANIPULATE INTO DF FOR PLOT_TABLE SCRIPT #####################
df$PrecipCustom <- df$pr/25.4
df$TmaxCustom <- ((df$tasmax) * (9/5)) - 459.67 #updated to match the original MACA parsing script
df$TminCustom <- ((df$tasmin) * (9/5)) - 459.67 #updated to match the original MACA parsing script
#df$TmaxCustom <- 9/5 * (df$tasmax-273) + 32
#df$TminCustom <- 9/5 * (df$tasmin-273) + 32
df$TavgCustom <- (df$TmaxCustom + df$TminCustom) / 2
df$RHmaxCustom <- df$rhsmax
df$RHminCustom <-df$rhsmin
df$VpdCustom <-df$vpd
df$GCM<-paste(df$model,df$rcp,sep=".")
df$Date<-as.POSIXlt(df$date,format="%Y-%m-%d")

# Date, GCM,PrecipCustom, TmaxCustom, TminCustom, RHmaxCustom, RHminCustom,TavgCustom

Baseline_all<-as.data.frame(subset(df,Date<"2005-12-31",select=c("Date", "GCM", "PrecipCustom", "TmaxCustom", "TminCustom", 
                                                                  "RHmaxCustom", "RHminCustom","TavgCustom")))
Future_all<-as.data.frame(subset(df,Date>"2005-12-31",select=c("Date", "GCM", "PrecipCustom", "TmaxCustom", "TminCustom", 
                                                                "RHmaxCustom", "RHminCustom","TavgCustom")))

#Baseline_all<-as.data.frame(subset(df,Date<"2006-01-01",select=c("Date", "GCM", "PrecipCustom")))
#Future_all<-as.data.frame(subset(df,Date>"2005-12-31",select=c("Date", "GCM", "PrecipCustom")))

save.image(paste(ParkDir,"/",SiteID,"_init_parsed.RData",sep=""))

# Remove saved climate files

# Remove saved climate files
if(Remove_files == "Y") {
  do.call(file.remove, list(list.files(paste(DataDir,SiteID,sep="/"), full.names = TRUE)))
  print("Files removed")
} else {print("Files remain")}
} 

######################################### SCRIPT END #####################################


