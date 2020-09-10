####### Batch water balance code #######
##  Revised for DINO-S T2 Variables #######
# Update from Amanda batch code - to bring work from climate data 

############################################################# USER INPUTS ##################################################################### 

#Formatted input data as a daily time series. Needs to include the following columns: Date, ppt_mm, tmax_C, tmin_C, and tmean_C (temp.'s in deg. Celsius)

#BA<-Baseline_all
#Baseline_all$GCM<-paste(Baseline_all$GCM,".rcp45",sep="");BA$GCM<-paste(BA$GCM,".rcp85",sep="")
#Baseline_all<-rbind(Baseline_all,BA);rm(BA)
#ALL_HIST<-Baseline_all 
#ALL_FUTURE<-Future_all

#rm(list=setdiff(ls(), c("ALL_HIST","ALL_FUTURE","PARK")))

#Site characteristics 
Sites = read.csv("C:/Users/adillon/Documents/RSS/DINO/GIS/DINO_South_site_characteristics.csv") #CSV file containing properties for all sites
n<-nrow(Sites)

jennings.raster <- raster('C:/Users/adillon/Documents/Repos/WaterBalance/merged_jennings2.tif') # Jennings coefficient layer
projection <- CRS("+init=epsg:4326") # Lat/Long projection for spatial data


get_freeze_jennings = function(tmean, high_thresh_temperatures, low_thresh_temperatures){ # redefining get_freeze to incorporate Jennings Coefficient (* double check R will pull 'global' function and not package function)
  freeze = ifelse(tmean > high_thresh_temperatures, 1, ifelse(tmean < low_thresh_temperatures, 0, tmean*(1/6)))
  return(freeze)
}


#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 


Method = "Hamon"  #Hamon is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

#Output directory
#OutDir = "~/RSS/Working/DINO/WB/S"

#Select GCMs - Include RCP

GCMs = c("GFDL-ESM2G.rcp85", "MIROC-ESM.rcp85", "CSIRO-Mk3-6-0.rcp45", "HadGEM2-ES365.rcp85") # DINO S
CFs<- c("Climate Future 1","Climate Future 2","Climate Future 3","Climate Future 4")


#Method for PET calculation 


colors5 <-   c("#12045C","#9A9EE5","#ffcccc","#E10720","white")
colors4 <- c("#12045C","#9A9EE5","#ffcccc","#E10720")
############################################################ END USER INPUTS ###################################################################

############################################################ CREATE CLIMATE INPUTS #############################################################
#### Historical
# Convert pr.In to mm and F to C
ALL_HIST$ppt_mm <- (ALL_HIST$PrecipCustom*25.4)
ALL_HIST$tmax_C <- 5/9*(ALL_HIST$TmaxCustom - 32)
ALL_HIST$tmin_C <- 5/9*(ALL_HIST$TminCustom - 32)
ALL_HIST$tmean_C <- (ALL_HIST$tmax_C + ALL_HIST$tmin_C)/2

#### Projected
# Convert pr.In to mm
ALL_FUTURE$ppt_mm <- (ALL_FUTURE$PrecipCustom*25.4)
ALL_FUTURE$tmax_C <- 5/9*(ALL_FUTURE$TmaxCustom - 32)
ALL_FUTURE$tmin_C <- 5/9*(ALL_FUTURE$TminCustom - 32)
ALL_FUTURE$tmean_C <- (ALL_FUTURE$tmax_C + ALL_FUTURE$tmin_C)/2
#Add YrMon column


#if(dir.exists(OutDir) == FALSE){
  #dir.create(OutDir)
#}

ClimData<-data.frame(Date=as.numeric(),ppt_mm=as.numeric(),tmean_C=as.numeric(),GCM=as.character())
# Loop through selected GCMs
for(i in 1:length(GCMs)){
  gcm <- GCMs[i]
    x<-subset(ALL_HIST,GCM == gcm, select=c("Date","ppt_mm","tmean_C","GCM"))
    y<-subset(ALL_FUTURE,GCM == gcm, select=c("Date","ppt_mm","tmean_C","GCM"))
    ClimData = rbind(ClimData,x,y)
}
ClimData$GCM<-factor(ClimData$GCM,levels=GCMs)
######################################################### END CLIMATE INPUTS ####################################################################


######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()

for (j in 1:length(GCMs)){
  gcm = GCMs[j]
  DailyWB = subset(ClimData,GCM=gcm)
for(i in 1:nrow(Sites)){
  SiteID = Sites$SiteID[i]
  Lat = Sites$Lat[i]
  Lon = Sites$Lon[i]
  Elev = Sites$Elevation[i]
  Aspect = Sites$Aspect[i]
  Slope = Sites$Slope[i]
  SWC.Max = Sites$SWC.Max[i]
  Wind = Sites$Wind[i]
  Snowpack.Init = Sites$Snowpack.Init[i]
  Soil.Init = Sites$Soil.Init[i]
  Shade.Coeff = Sites$Shade.Coeff[i]
  coords = cbind(Sites$Lon[i], Sites$Lat[i])
  sp <- SpatialPoints(coords, proj4string = projection)
  jennings.Coeff = raster::extract(jennings.raster, sp) # The Jennings coefficient = temperature where precip is half snow, half rain
  
  
  # Not totally sure where to place this
  low_thresh_temperatures = jennings.Coeff - 3 # This sets up a 6 degree span, which corresponds to the 1/6 = 0.167 precip fraction.
  high_thresh_temperatures = jennings.Coeff + 3
  
   #Calculate daily water balance variables 
  DailyWB$SiteID = SiteID
  DailyWB$daylength = get_daylength(DailyWB$Date, Lat)
  DailyWB$F = get_freeze_jennings(DailyWB$tmean_C, high_thresh_temperatures, low_thresh_temperatures)
  DailyWB$RAIN = get_rain(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$SNOW = get_snow(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$PACK = get_snowpack(DailyWB$ppt_mm, DailyWB$F, Snowpack.Init)
  DailyWB$MELT = get_melt(DailyWB$PACK, DailyWB$SNOW, DailyWB$F, Snowpack.Init)
  DailyWB$W = DailyWB$MELT + DailyWB$RAIN
  if(Method == "Hamon"){
    DailyWB$PET = ET_Hamon_daily(DailyWB)
  } else {
    if(Method == "Penman-Monteith"){
      DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
    } else {
      print("Error - PET method not found")
    }
  }
  DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
  DailyWB$W_PET = DailyWB$W - DailyWB$PET
  DailyWB$SOIL = get_soil(DailyWB$W, DailyWB$PET, SWC.Max, Soil.Init)
  DailyWB$DSOIL = diff(c(Soil.Init, DailyWB$SOIL))
  DailyWB$AET = get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
  DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
  DailyWB$D = DailyWB$PET - DailyWB$AET
  DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)
  AllDailyWB[[i]] = DailyWB
  }
}
WBData<-do.call(rbind,AllDailyWB)
######################################################### END WB VARIABLE CALCULATIONS ################################################################

