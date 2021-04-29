# Download portion of script downloads data for whole Contiguous US.
# These data are stored on FC-Science Adaptation Sharepoint, so code commented out.
# C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/Miscellaneous/PRISM4k/
# If needed locally, uncomment, run, and recomment.

# CODE
library(rgdal)
library(raster)
library(fields)   # for image.plot
library(ggplot2)
library(WriteXLS)
library(prism)
rm(list=ls())


#Location for saving data
# DataDir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/Miscellaneous/PRISM4k/"
DataDir <- "C:/Users/msears/Documents/PRISM4k/" #location PRISM data - if not downloading
OutDir <- "C:/Users/msears/Documents/PRISM-testing/YELL-StephCr-PRISM4k/"  #location saving output
# Coordinates for cell center. Cell size is 0.04166 for 4-km dataset, .008333 for 800-m dataset. 
Lat = 45.0426
Lon = -110.7527

BeginYr = 1895
EndYr =  2018
BeginMon = 1
EndMon = 12

var<-c("tmin","tmax","ppt")

# ############################################ PRISM DOWNLOAD ##################################################
# # ONLY RUN THIS SECTION TO STORE LOCALLY
# Date=seq(as.Date(paste(BeginYr,BeginMon,"01",sep="-")),as.Date(paste(EndYr,EndMon,"01",sep="-")),"months")
# Date<-as.Date(Date,format="%Y-%d=%m")
# df<-data.frame(Date=Date)
# 
# for (i in 1:length(var)) {
# varDir<-paste(DataDir, var[i],sep="")
# dir.create(varDir)
# options(prism.path=varDir)
# get_prism_monthlys(type = var[i], years = BeginYr:EndYr,mon = seq(BeginMon,EndMon,1), keepZip = FALSE) # data download (.bil files) # this command/argument changes depending on whether you want annual, monthly, tmax, tmin, etc.
# print(paste(var[i],"files downloaded",sep=" "))
# df1 <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) #plots slice of data from single location from list of prism files
# df2<-df1$data
# rownames(df2)<-NULL
# colnames(df2)<-df1$labels
# df<-merge(df,df2,by="Date")
# rm(df1,df2)
# }
# colnames(df)<-c("Date",var)

####################################### PRISM PARSE ############################################################
Date=seq(as.Date(paste(BeginYr,BeginMon,"01",sep="-")),as.Date(paste(EndYr,EndMon,"01",sep="-")),"months")
Date<-as.Date(Date,format="%Y-%d=%m")
df<-data.frame(Date=Date)

for (i in 1:length(var)) {
  varDir<-paste(DataDir, var[i],sep="")
  options(prism.path=varDir)
  df1 <- prism_slice(c(Lon,Lat),ls_prism_data()[,1]) #plots slice of data from single location from list of prism files
  df2<-df1$data
  rownames(df2)<-NULL
  colnames(df2)<-df1$labels
  df<-merge(df,df2,by="Date")
  rm(df1,df2)
}
colnames(df)<-c("Date",var)

df$Year <- format(df$Date,format="%Y")


#  Deal with dates and seasons
GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

df$Season<-GetSeason(df$Date)

PptMeans<-subset(df,select = -c(tmin, tmax))
PptMeans$PptIn <- PptMeans$ppt/25.4     # mm to in

TminMeans<-subset(df,select = -c(ppt, tmax))
TminMeans$TminF <- TminMeans$tmin * 9/5 + 32

TmaxMeans<-subset(df,select = -c(tmin, ppt))
TmaxMeans$TmaxF <- TmaxMeans$tmax * 9/5 + 32

#### Save RData
save.image(sprintf("%s%s_%s_%s_PRISM_PptTminTmax_IntermediateFiles.RData", OutDir,SiteID, Lat, Lon))

#### Create .xslx workbook with all data tables
WriteXLS(c("PptMeans", "TmaxMeans", "TminMeans"), paste(OutDir, "/",SiteID, "_", Lat,"_", Lon,"_PRISM.xlsx", sep=""), BoldHeaderRow = TRUE)



