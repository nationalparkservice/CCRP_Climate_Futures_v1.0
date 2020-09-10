# RSS_Plot_Table_Creation vxx.R
# v02.1 - added Future_Means to xlsx output.
# v02 STABLE - 22 Oct 2015 - days below ColdTemp added and debugged.

# AC - 07/03/2018 - Altered delta formulas to calculate difference from a standard averaged historical value, rather than by comparing CFs from Baseline_all data frame. 
#   This is needed to make script compatible with MACA 

# ACR - 01/02/2020 - Merged with A.Runyon code. Moved CF creation into this script. 

#ACR- 03/16/2020 - change to Plot_Table_T1 to reflect CCSP order of things

library(ncdf4)
library(reshape2)
library(WriteXLS)
library(data.table)
library(zoo)
library(corrplot)

rm(list=ls())

################################################## INITIALS ##################################################

SiteID = "DINO-S"

#Directory and RData file where daily data series is stored
DataDir = "C:/Users/akell/Documents/NPS/RSS/DINO/MACA_S" # Home Computer
DataFile = "DINO-S_init_parsed.RData"
WB<-read.csv("C:/Users/akell/Documents/NPS/RSS/DINO/WB/MonthlyWB.csv",header=T)

#Year range for summarizing future climate (Year - Range/2) to (Year + Range/2)
Year = 2040 #Central year
Range = 30  #Number of years to summarize (should be at least 30)


#Temperature/precip threshold values
HotTemp = 95    # deg F. Default should be about 100 deg F
ColdTemp = 32    # deg F
PrecipThreshold = 0.05    # inches per day. Precip Threshold (used to measure Drought duration). For many GCMs shoud not 
#  be 0 because models "drizzle". Some investigation necessary.
QuantileLow = 0.05   #Quantiles for temperature threshold calculations
QuantileHigh = 0.95

#Month and season names 
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)
seasons=factor(c("Winter", "Spring", "Summer", "Fall"))
levels(seasons)=seasons

#Create output directory
WD_plots = paste(DataDir, "Figs MACA", sep="/")
if(dir.exists(WD_plots) == FALSE){
  dir.create(WD_plots)
}

################################################### SUBSET TIME PERIOD ########################################
setwd(DataDir)
load(DataFile)

Baseline_all$Date = strptime(Baseline_all$Date, "%Y-%m-%d")
Future_all$Date = strptime(Future_all$Date, "%Y-%m-%d")

# # Subset Future_all to only be near future (2025-2055) and Baseline_all to only but until 2000
BA<-Baseline_all
Baseline_all$GCM<-paste(Baseline_all$GCM,".rcp45",sep="");BA$GCM<-paste(BA$GCM,".rcp85",sep="")
Baseline_all<-rbind(Baseline_all,BA);rm(BA)
ALL_HIST<-Baseline_all
Baseline_all$Year<-format(as.Date(Baseline_all$Date, format="%Y-%m-%d"),"%Y")
Baseline_all<-subset(Baseline_all,Year<2000)
Baseline_all$Year<-NULL

ALL_FUTURE<-Future_all  
Future_all$yr = Future_all$Date$year + 1900
Future_all = subset(Future_all, yr >= Year - (Range/2) & yr <= (Year + (Range/2)))
################################################### FUNCTION DEFINITIONS ########################################

#### FUNCTION TO CALCULATE SEASON FROM 'DATE' ####

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF TOTAL DAYS/YEAR FOR A VARIABLE ####

MeanAnnualTotals = function(DF, VarName){
  Years = length(unique(strftime(DF$Date, "%Y")))
  VarIndex = which(colnames(DF) == VarName)
  MeanAnnualTotal = aggregate(DF[,VarIndex] ~ DF$GCM, FUN=function(x){sum(x)/Years}) 
  names(MeanAnnualTotal) = c("GCM", "MeanAnnualTotals")
  return(MeanAnnualTotal)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM ANNUAL DAYS/YEAR FOR A VARIABLE ####
MeanAnnualMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,3] ~ YearlyMax$GCM, FUN=mean)
  names(MeanAnnualMax) = c("GCM", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetAnnualMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by="GCM")
  TotalMeans$Delta = unlist(FutureMeans[2] - BaseMeans[2])
  return(TotalMeans)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM SEASONAL DAYS/YEAR FOR A VARIABLE ####
MeanSeasonalMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year, Season=DF$season), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,4], by=list(GCM=YearlyMax$GCM, Season=YearlyMax$Season), FUN=mean)
  names(MeanAnnualMax) = c("GCM", "Season", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetSeasonalMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by=c("GCM", "Season"))
  TotalMeans$Delta = unlist(FutureMeans[3] - BaseMeans[3])
  return(TotalMeans)
}

#### END FUNCTION ####

#### FUNCTION TO CALCULATE HEAT INDEX ####
#use daily tmax for temp as well as rhs_max
heat_index <- function(temp, RH) {
  Sted <- 0.5 * (temp + 61 + ((temp - 68) * 1.2) + (RH * 0.094))
  Roth <- -42.379 + (2.04901523 * temp) + (10.14333127 * RH) + (-.22475541 * temp * RH) +
    (-.00683783 * temp^2) + (-.05481717 * RH^2) + (.00122874 * temp^2 * RH) + 
    (.00085282 * temp * RH^2) + (-.00000199 * temp^2 * RH^2)
  adj1 <- ((13 - RH) / 4) * sqrt((17 - abs(temp - 95)) / 17)
  adj2 <- ((RH - 85) / 10) * ((87 - temp) / 5)
  heat_index<-ifelse(temp < 80, Sted, 
                     ifelse(RH < 13 & temp > 80 & temp < 112, Roth-adj1,
                            ifelse(RH > 85 & temp > 80 & temp < 87, Roth+adj2, Roth)))
  heat_index
} #creates errors but doesn't matter becuase not used when not applicable

#### END FUNCTION ####

################################ END FUNCTION DEFINITIONS #########################################

################################# SUMMARIZE CHANGE IN FUTURE TEMP/PRECIP MEANS BY GCM ####################
####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmx = mean(Baseline_all$TmaxCustom)
BaseMeanTmn = mean(Baseline_all$TminCustom)

################################ SUMMARIZE TEMPERATURE, PRECIP, RH BY MONTH & SEASON #######################
Baseline_all$Month<-format(Baseline_all$Date,"%m")
Baseline_all$Year<-format(Baseline_all$Date,"%Y")
Future_all$Month<-format(Future_all$Date,"%m")
Future_all$Year<-format(Future_all$Date,"%Y")

#Add season variable for all data in Future_all and Baseline_all
Future_all$season=getSeason(Future_all$Date)
Baseline_all$season=getSeason(Baseline_all$Date)


#### Create tables with monthly tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxCustom~Month+GCM,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~Month+GCM,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~Month+GCM,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+Year+GCM,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+GCM,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxCustom+Baseline_all$RHminCustom)/2
RHmean = aggregate(RHmean~Month+GCM,Baseline_all,mean,na.rm=TRUE)

H_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)

# Future
Tmax = aggregate(TmaxCustom~Month+GCM,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~Month+GCM,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~Month+GCM,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+Year+GCM,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~Month+GCM,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxCustom+Future_all$RHminCustom)/2
RHmean = aggregate(RHmean~Month+GCM,Future_all,mean,na.rm=TRUE)

F_Monthly<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)


#### Create tables with seasonal tmax/tmin/tmean/precip/RHmean delta by CF
# Historical
Tmax = aggregate(TmaxCustom~season+GCM,Baseline_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~season+GCM,Baseline_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~season+GCM,Baseline_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+Year+GCM,Baseline_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+GCM,Precip,mean,na.rm=TRUE)
Baseline_all$RHmean<-(Baseline_all$RHmaxCustom+Baseline_all$RHminCustom)/2
RHmean = aggregate(RHmean~season+GCM,Baseline_all,mean,na.rm=TRUE)

H_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)

# Future
Tmax = aggregate(TmaxCustom~season+GCM,Future_all,mean,na.rm=TRUE)
Tmin = aggregate(TminCustom~season+GCM,Future_all,mean,na.rm=TRUE)
Tmean = aggregate(TavgCustom~season+GCM,Future_all,mean,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+Year+GCM,Future_all,sum,na.rm=TRUE)
Precip = aggregate(PrecipCustom~season+GCM,Precip,mean,na.rm=TRUE)
Future_all$RHmean<-(Future_all$RHmaxCustom+Future_all$RHminCustom)/2
RHmean = aggregate(RHmean~season+GCM,Future_all,mean,na.rm=TRUE)

F_Season<-Reduce(function(...)merge(...,all=T),list(Tmax,Tmin,Tmean,Precip,RHmean))
rm(Tmax,Tmin,Tmean,Precip,RHmean)


########################################## END MONTH & SEASON SUMMARY ##########################################

######################################## CALCULATE ANNUAL DAYS ABOVE/BELOW TEMP & PRECIP THRESHOLDS ##########################

###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPS ######

HistYears = length(unique(Baseline_all$Date$year))

HistTmax99 = quantile(Baseline_all$TmaxCustom, 0.99)
HistTmaxHigh = quantile(Baseline_all$TmaxCustom, QuantileHigh)
HistTminLow = quantile(Baseline_all$TminCustom, QuantileLow)
HistPrecip95 = quantile(Baseline_all$PrecipCustom[which(Baseline_all$PrecipCustom > 0.05)], 0.95) #percentil of days receiving precip
HistPr99 = quantile(Baseline_all$PrecipCustom[which(Baseline_all$PrecipCustom > 0.05)], 0.99)

Baseline_all<- Baseline_all[with(Baseline_all, order(GCM, Date)), ]
Future_all<- Future_all[with(Future_all, order(GCM, Date)), ]

Baseline_all$Julian = Baseline_all$Date$yday
Baseline_all$TavgCustom = (Baseline_all$TmaxCustom + Baseline_all$TminCustom)/2
Baseline_all$OverHotTemp = Baseline_all$TmaxCustom > HotTemp
Baseline_all$OverHighQ = Baseline_all$TmaxCustom > HistTmaxHigh
Baseline_all$Tmax99 = Baseline_all$TmaxCustom > HistTmax99
Baseline_all$HeatConsecutive=(Baseline_all$Tmax99)*unlist(lapply(rle(Baseline_all$Tmax99)$lengths, seq_len))
Baseline_all$UnderColdTemp = Baseline_all$TminCustom < ColdTemp
Baseline_all$UnderLowQ = Baseline_all$TminCustom < HistTminLow
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
Baseline_all$NoPrecip = Baseline_all$PrecipCustom < PrecipThreshold
Baseline_all$NoPrecipLength = (Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) 
Baseline_all$OverPrecip95 = Baseline_all$PrecipCustom > HistPrecip95
Baseline_all$OverPrecip99 = Baseline_all$PrecipCustom > HistPr99
Baseline_all$PrecipOver1 = Baseline_all$PrecipCustom > 1
Baseline_all$PrecipOver2 = Baseline_all$PrecipCustom > 2
Baseline_all$FThaw = Baseline_all$TminCustom<28 & Baseline_all$TmaxCustom>34
Baseline_all$GDD = Baseline_all$TavgCustom>41 # 5 deg C
Baseline_all$GDD_count = Baseline_all$GDD * unlist(lapply(rle(Baseline_all$GDD)$lengths, seq_len))
Baseline_all$N_GDD_count = (Baseline_all$GDD == FALSE) * unlist(lapply(rle(Baseline_all$GDD)$lengths, seq_len))
Baseline_all$HI = heat_index(Baseline_all$TmaxCustom,Baseline_all$RHminCustom)
Baseline_all$HI.EC = Baseline_all$HI >89 & Baseline_all$HI <103
Baseline_all$HI.Dan = Baseline_all$HI >102 & Baseline_all$HI < 124
Baseline_all$Frost = Baseline_all$GDD == TRUE & Baseline_all$TminCustom < 32

Future_all$Julian = Future_all$Date$yday
Future_all$TavgCustom = (Future_all$TmaxCustom + Future_all$TminCustom)/2
Future_all$OverHotTemp = Future_all$TmaxCustom > HotTemp
Future_all$OverHighQ = Future_all$TmaxCustom > HistTmaxHigh
Future_all$Tmax99 = Future_all$TmaxCustom > HistTmax99
Future_all$HeatConsecutive=(Future_all$Tmax99)*unlist(lapply(rle(Future_all$Tmax99)$lengths, seq_len))
Future_all$UnderColdTemp = Future_all$TminCustom < ColdTemp
Future_all$UnderLowQ = Future_all$TminCustom < HistTminLow
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
Future_all$NoPrecip = Future_all$PrecipCustom < PrecipThreshold
Future_all$NoPrecipLength = (Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len)) 
Future_all$OverPrecip95 = Future_all$PrecipCustom > HistPrecip95
Future_all$OverPrecip99 = Future_all$PrecipCustom > HistPr99
Future_all$PrecipOver1 = Future_all$PrecipCustom > 1
Future_all$PrecipOver2 = Future_all$PrecipCustom > 2
Future_all$FThaw = Future_all$TminCustom<28 & Future_all$TmaxCustom>34
Future_all$GDD = Future_all$TavgCustom>41 # 5 deg C
Future_all$GDD_count = Future_all$GDD * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))
Future_all$N_GDD_count = (Future_all$GDD == FALSE) * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))
Future_all$HI = heat_index(Future_all$TmaxCustom,Future_all$RHminCustom)
Future_all$HI.EC = Future_all$HI >89 & Future_all$HI <103
Future_all$HI.Dan = Future_all$HI >102 & Future_all$HI < 124
Future_all$Frost = Future_all$GDD == TRUE & Future_all$TminCustom < 32

#### Historical Dataframes aggregated by Year+GCM ###########
H_annual<-aggregate(cbind(PrecipCustom,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                          NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                          FThaw, GDD,HI.EC,HI.Dan)~GCM+Year,Baseline_all,sum)

Hmeans<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,RHmean)~GCM+Year,Baseline_all,FUN=mean)
H_annual<-merge(H_annual,Hmeans,by=c("GCM","Year"));rm(Hmeans)

# Agrregate mean w/ temps only W months
H.WinterTemp<-aggregate(TavgCustom~GCM+Year,data=subset(Baseline_all,Month<3 | Month>11), mean)
colnames(H.WinterTemp)[3]<-"W.Temp"
H_annual <- merge(H_annual,H.WinterTemp,by=c("GCM","Year")); rm(H.WinterTemp)

# Further Growing Season Calculations
Historical_GS <- as.data.table(subset(Baseline_all,select=c(Year,GCM,Julian,GDD_count,N_GDD_count)))
Historical_GU<-Historical_GS[GDD_count==7,.SD[1],by=.(Year,GCM)]

Historical_SE<-Historical_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year,GCM)]
Historical_SE$adjusted<-Historical_SE$Julian - 6

H<-aggregate(cbind(Julian)~GCM+Year,data=Historical_GU,mean,na.rm=TRUE)
colnames(H)[3] <- "BegGrow"
H<-merge(H,Historical_SE[,c("GCM","Year","adjusted")], by=c("Year","GCM"))
colnames(H)[4] <- "EndGrow"
H$GrowLen<- H$EndGrow - H$BegGrow
H_annual<-merge(H_annual,H,by=c("GCM","Year"))
rm(Historical_GS,Historical_GU,Historical_SE,H)

# Frost length calculations - late spring freeze events
Sp.Frost<-aggregate(Frost~GCM+Year,data=subset(Baseline_all,Julian<180),sum)
colnames(Sp.Frost)[3] <- "Sp.Frost"
H_annual<-merge(H_annual,Sp.Frost,by=c("GCM","Year"));rm(Sp.Frost)

############################### Merge WB variables for Sp.SM, CS.SM, D_in #######################

#### Future Dataframes aggregated by Year+GCM ###########
F_annual<-aggregate(cbind(PrecipCustom,OverHotTemp, OverHighQ, Tmax99, UnderColdTemp,UnderLowQ,  
                          NoPrecip, NoPrecipLength, OverPrecip95, OverPrecip99, PrecipOver1, PrecipOver2,
                          FThaw, GDD,HI.EC,HI.Dan)~GCM+Year,Future_all,sum)

Fmeans<-aggregate(cbind(TmaxCustom,TminCustom,TavgCustom,RHmean)~GCM+Year,Future_all,FUN=mean)
F_annual<-merge(F_annual,Fmeans,by=c("GCM","Year"));rm(Fmeans)

# Agrregate mean w/ temps only W months
F.WinterTemp<-aggregate(TavgCustom~GCM+Year,data=subset(Future_all,Month<3 | Month>11), mean)
colnames(F.WinterTemp)[3]<-"W.Temp"
F_annual <- merge(F_annual,F.WinterTemp,by=c("GCM","Year")); rm(F.WinterTemp)

# Further Growing Season Calculations
Future_GS <- as.data.table(subset(Future_all,select=c(Year,GCM,Julian,GDD_count,N_GDD_count)))
Future_GU<-Future_GS[GDD_count==7,.SD[1],by=.(Year,GCM)]

Future_SE<-Future_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year,GCM)]
Future_SE$adjusted<-Future_SE$Julian - 6

F<-aggregate(cbind(Julian)~GCM+Year,data=Future_GU,mean,na.rm=TRUE)
colnames(F)[3] <- "BegGrow"
F<-merge(F,Future_SE[,c("GCM","Year","adjusted")], by=c("Year","GCM"))
colnames(F)[4] <- "EndGrow"
F$GrowLen<- F$EndGrow - F$BegGrow
F_annual<-merge(F_annual,F,by=c("GCM","Year"))
rm(Future_GS,Future_GU,Future_SE,F)

# Frost length calculations - late spring freeze events
Sp.Frost<-aggregate(Frost~GCM+Year,data=subset(Future_all,Julian<180),sum)
colnames(Sp.Frost)[3] <- "Sp.Frost"
F_annual<-merge(F_annual,Sp.Frost,by=c("GCM","Year"));rm(Sp.Frost)

############################### Merge WB variables for Sp.SM, CS.SM, D_in #######################
head(WB)
WB$Year<-as.integer(substr(WB$yrmon,1,4))
WB$Month<-as.integer(substr(WB$yrmon,5,6))

Sp.SM<-subset(WB,Month>4&Month<10,select=c("avg_soil","GCM","Year","Month"))
Sp.SM<-aggregate(avg_soil~Year+GCM,Sp.SM,mean)
CS.SM<-subset(WB,Month<4|Month>10,select=c("avg_soil","GCM","Year","Month"))
CS.SM<-aggregate(avg_soil~Year+GCM,CS.SM,mean)
D<-aggregate(sum_d~Year+GCM,WB,mean)
wb2<-merge(Sp.SM,CS.SM,by=c("Year","GCM"));wb2<-merge(wb2,D,by=c("Year","GCM"))
names(wb2)<-c("Year","GCM","Sp.SM_mm","CS.SM_mm","D_mm")
WB_H<-subset(wb2,Year<2000)
WB_F<-subset(wb2,Year>2024&Year<2056)

H_annual<-merge(H_annual,WB_H,by=c("Year","GCM"))
F_annual<-merge(F_annual,WB_F,by=c("Year","GCM"))


######################################## END THRESHOLD CALCULATIONS ##############################


####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM


Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                    ~ Future_all$GCM, Future_all, mean,na.rm=F))   # , Future_all$Wind
names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")    # , "Wind"
Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2

Baseline_Means = data.frame(aggregate(cbind(PrecipCustom, TmaxCustom, TminCustom)~GCM, 
                                      Baseline_all[which(Baseline_all$GCM %in% unique(Future_all$GCM)),], mean))    #  ,Baseline_all$Wind)
names(Baseline_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")  #  , "Wind")
Baseline_Means$TavgCustom = (Baseline_Means$TmaxCustom + Baseline_Means$TminCustom)/2

HA_means<-aggregate(cbind(Tmax99,GrowLen,Sp.SM_mm,CS.SM_mm,D_mm)~GCM,H_annual,mean,na.rm=F)
FA_means<-aggregate(cbind(Tmax99,GrowLen,Sp.SM_mm,CS.SM_mm,D_mm)~GCM,F_annual,mean,na.rm=F)

Baseline_Means<-merge(Baseline_Means,HA_means,by="GCM")
Future_Means<-merge(Future_Means,FA_means,by="GCM")

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$PrecipCustom - Baseline_Means$PrecipCustom
Future_Means$DeltaTmx = Future_Means$TmaxCustom - Baseline_Means$TmaxCustom
Future_Means$DeltaTmn = Future_Means$TminCustom - Baseline_Means$TminCustom
Future_Means$DeltaTavg = Future_Means$TavgCustom - Baseline_Means$TavgCustom
Future_Means$DeltaTmax99 = Future_Means$Tmax99 - Baseline_Means$Tmax99
Future_Means$DeltaGrowLen = Future_Means$GrowLen - Baseline_Means$GrowLen
Future_Means$DeltaSp.SM = Future_Means$Sp.SM_mm - Baseline_Means$Sp.SM_mm
Future_Means$DeltaCS.SM = Future_Means$CS.SM_mm - Baseline_Means$CS.SM_mm
Future_Means$DeltaD = Future_Means$D_mm - Baseline_Means$D_mm

#     Add column with emissions scenario for each GCM run
Future_Means$emissions[grep("rcp85",Future_Means$GCM)] = "RCP 8.5"
Future_Means$emissions[grep("rcp45",Future_Means$GCM)] = "RCP 4.5"


#### SetWD and save
setwd(WD_plots)

##### Save Current workspace environment
save.image(sprintf("%s_T1.RData",SiteID))

#  EOF

# Correlation matrix
T1.cor<-cor(Future_Means[,14:19], method = c("spearman"))
corrplot.mixed(T1.cor)


