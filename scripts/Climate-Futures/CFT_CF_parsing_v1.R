## CFT_CF_parsing_v1 -- parse MACA data using CFT package and format for reading through RSS scripts
# Output same as RSS_MACA_Parsing.R, saved as SiteID_init_parsed.RData
# To be replaced once Rmd written from RCF project
# A. Runyon 20201009

#Units: temp = K, precip = mm, Rh = %

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

## Park data
proj_dir <- "~/Parse_test"
Lat <- 41.83476
Lon = -103.707
SiteID <- "SCBL"

## Download data
#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin","rhsmax","rhsmin")
scens = c("rcp45", "rcp85")

#Variable names for output tables
VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom","RHmaxCustom","RHminCustom")

# GCMs to be extracted
GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2006   #2006-2099
Future_EndYear = 2099   #2006-2099
Hist_StartYear = 1950     #1950-2005
Hist_EndYear = 2005      #1950-2005

Remove_files = "Y" # "N"       #Removes all climate data files saved in directory

############################## END INITIALS ##################################################

# Now can only use spatial object (not park name)
Site_coordinates <- data.frame(PARK=SiteID,lat=Lat,lon=Lon)
coordinates(Site_coordinates) <- ~lon+lat
proj4string(Site_coordinates) <- "+proj=longlat +datum=NAD83 +no_defs " #same proj4string used in NPS_boundary_centroids.shp

# # Load Data - from centroids
# nps_boundary_centroids <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
# centroid <- filter(nps_boundary_centroids, UNIT_CODE == "BIBE")
# Sp_centroid <- as_Spatial(centroid) # Does not accept sf objects 

# download data
file_refs <- cftdata(aoi = Site_coordinates, 
                     area_name = SiteID,
                     years = c(Hist_StartYear, Future_EndYear),
                     models = GCMs,
                     local_dir = proj_dir,
                     parameters = vars,
                     scenarios = scens,
                     ncores = parallel::detectCores() / 2)
glimpse(file_refs)
df <- cft_df(file_refs, ncores = parallel::detectCores() / 2)
glimpse(df)

######################## MANIPULATE INTO DF FOR PLOT_TABLE SCRIPT #####################
df$PrecipCustom <- df$pr/25.4
df$TmaxCustom <- ((df$tasmax) * (9/5)) - 459.67
df$TminCustom <- ((df$tasmin) * (9/5)) - 459.67
df$TavgCustom <- (df$TmaxCustom + df$TminCustom) / 2
df$RHmaxCustom <- df$rhsmax
df$RHminCustom <-df$rhsmin
df$GCM<-paste(df$model,df$rcp,sep=".")
df$Date<-as.POSIXlt(df$date,format="%Y-%m-%d")

# Date, GCM,PrecipCustom, TmaxCustom, TminCustom, RHmaxCustom, RHminCustom,TavgCustom
Baseline_all<-as.data.frame(subset(df,Date<"2005-12-31",select=c("Date", "GCM", "PrecipCustom", "TmaxCustom", "TminCustom", 
                                               "RHmaxCustom", "RHminCustom","TavgCustom")))
Future_all<-as.data.frame(subset(df,Date>"2005-12-31",select=c("Date", "GCM", "PrecipCustom", "TmaxCustom", "TminCustom", 
                                                                 "RHmaxCustom", "RHminCustom","TavgCustom")))
save.image(paste(proj_dir,"/",SiteID,"_init_parsed.RData",sep=""))

# Remove saved climate files

# Remove saved climate files
if(Remove_files == "Y") {
  do.call(file.remove, list(list.files(paste(proj_dir,SiteID,sep="/"), full.names = TRUE)))
  print("Files removed")
} else {print("Files remain")}

######################################### SCRIPT END #####################################
