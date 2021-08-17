## CFT_CF_parsing_v1 -- parse MACA data using CFT package and format for reading through RSS scripts
# Output same as RSS_MACA_Parsing.R, saved as SiteID_init_parsed.RData
# To be replaced once Rmd written from RCF project
# A. Runyon 20201009

#Units: temp = K, precip = mm, Rh = %

## Park data

proj_dir = './data/park-specific/output' # for .csv's
if(dir.exists(proj_dir) == FALSE){
  dir.create(proj_dir)
}


## Download data


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
