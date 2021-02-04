
################################ USER INPUTS #################################################

Gridmet <- read.csv("data/park-specific/input/GridMet.csv",header=T)

file <- list.files(path = './data/park-specific/output', pattern = 'Final_Environment.RData', full.names = TRUE) 
load(file)


## All 508-compliant color scheme -- navy (hot wet), light blue (warm wet), pink (warm dry), red (hot dry)
colors2<- c("#9A9EE5","#E10720")  # WarmWet/HotDry
# colors2<- c("#F3D3CB","#12045C")  # HotWet/WarmDry

colors3<-c("white",colors2)

if(dir.exists('./figures/additional') == FALSE){
  dir.create('./figures/additional')
}

OutDir<-("./figures/additional")

################################ END USER INPUTS #############################################

############################### FORMAT DATAFRAMES  ############################################
# Gridmet
Gridmet$Date<-ymd(Gridmet$Date)
Gridmet$Month<-format(Gridmet$Date,format="%m")
Gridmet$Year<-format(Gridmet$Date,format="%Y")
Gridmet$TmeanC<-(((Gridmet$tmax+Gridmet$tmin)/2)-32)*5/9
Gridmet$Pr_mm<-Gridmet$precip*25.4
d<-aggregate(Pr_mm~Month+Year,Gridmet,sum)
d2<-aggregate(TmeanC~Month+Year,Gridmet,mean)
drt<-merge(d,d2,by=c("Month","Year"));rm(d,d2)
drt<-drt[with(drt, order(Year, Month)),]
drt$PET<-thornthwaite(drt$TmeanC,lat = Lat)

# Run SPEI on gridmet
tp<-ts(drt$Pr_mm,frequency=12,start=c(1979,1))
tpet<-ts(drt$PET,frequency=12,start=c(1979,1))
SPEI<-spei(tp - tpet, SPEI_per)
PlotName <- "Gridmet-SPEI"
plot1 <- paste('./figures/additional/', PlotName)
jpeg(paste(plot1, ".jpg", sep = ""), width = 350, height = 350)

plot(x=SPEI,main="Gridmet") #eventually prob want to figure out how to make x-axis date
dev.off()

drt$SPEI<-SPEI$fitted;drt$SPEI[which(is.na(drt$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
names(drt)[6]<-"SPEI"
drt3<-aggregate(cbind(Pr_mm,SPEI)~Year,drt,mean)

# # MACA      This step only needed if historical GCMs don't have RCPs pasted on end
# AH<-ALL_HIST
# ALL_HIST$GCM<-paste(ALL_HIST$GCM,"rcp45",sep=".")
# AH$GCM<-paste(AH$GCM,"rcp85",sep=".")
# ALL_HIST<-rbind(ALL_HIST,AH); rm(AH)
H<-subset(ALL_HIST,GCM %in% WB_GCMs,select=c(Date,GCM,PrecipCustom,TavgCustom))
F<-subset(ALL_FUTURE, GCM %in% WB_GCMs, select=c(Date,GCM,PrecipCustom,TavgCustom))
ALL<-rbind(H,F)

ALL$Month<-format(ALL$Date,format="%m")
ALL$Year<-format(ALL$Date,format="%Y")
ALL$Pr_mm<-ALL$PrecipCustom*25.4
ALL$TmeanC<-(ALL$TavgCustom-32)*5/9

M<-aggregate(Pr_mm~Month+Year+GCM,ALL,sum)
Mon<-aggregate(TmeanC~Month+Year+GCM,ALL,mean)
Mon<-merge(Mon,M,by=c("Month","Year","GCM"));rm(M)
Mon$PET<-thornthwaite(Mon$TmeanC,lat=Lat)
Mon<-merge(Mon,CF_GCM,by="GCM")
Mon$CF<-factor(Mon$CF,levels=unique(Mon$CF))
MON<-aggregate(cbind(Pr_mm,PET)~Month+Year+CF,Mon,mean) 
MON<-MON[with(MON, order(CF,Year, Month)),]

CF.split<-split(MON,MON$CF) #Splits df into array by CF
# this step is done because each CF has unique historical record and SPEI normalized to average conditions at beginning of record

for (i in 1:length(CF.split)){
  name=names(CF.split)[i]
  t<-CF.split[[i]]
  tp<-ts(t$Pr_mm,frequency=12,start=c(1950,1))
  tpet<-ts(t$PET,frequency=12,start=c(1950,1))
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(1950,1),ref.end=c(1999,12))
  CF.split[[i]]$SPEI <- SPEI$fitted[1:length(SPEI$fitted)]
  # Plot each CF
  plot <- paste('./figures/additional/', name)
  jpeg(paste(plot,"-SPEI.jpg",sep=""), width = 350, height = 350)
  plot(x=SPEI,main=name) #eventually prob want to figure out how to make x-axis date
  dev.off()
}

all2<- ldply(CF.split, data.frame) #convert back to df
all2$SPEI[which(is.na(all2$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(Pr_mm,SPEI)~Year+CF,all2,mean)

###################################### PLOT ANNUAL TIME-SERIES #################################################
############################################# Plotting ###########################################################
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom",
                  panel.background = element_blank(), #Background white
                  panel.grid.major = element_line("light grey",0.3)) #add grid back

BarPlotTheme = theme(axis.text.x=element_text(size=24),    #Text size for axis tick mark labels
                     axis.text.y=element_text(size=20),
                     axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                     axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                     plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                     legend.position = "none") 
#Height and width 
PlotWidth = 15
PlotHeight = 9

# Gridmet
drt3$col[drt3$SPEI>=0]<-"wet"
drt3$col[drt3$SPEI<0]<-"dry"
drt3$col<-factor(drt3$col, levels=c("wet","dry"))

ggplot(data = drt3, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_bar(stat="identity",aes(fill=col),col="black") + 
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = "SPEI values for Historical Period (gridMET)", 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave("Recent Drought.png", path = './figures/additional', width = 18, height = 9)

# MACA prep dataframe
all3$col[all3$SPEI>=0]<-"wet"
all3$col[all3$SPEI<0]<-"dry"
all3$col<-factor(all3$col, levels=c("wet","dry"))
all3$Year<-as.numeric(all3$Year)

# CF 

CF1<-subset(all3, CF %in% CFs[1] )
grid.append<-drt3; grid.append$CF<-CFs[1]
grid.append<-subset(grid.append, select=c(Year,CF,Pr_mm:col))
grid.append<-rbind(grid.append, subset(CF1,Year>=2020 & Year < 2070))

ggplot(data = subset(CF1,Year>=2025&Year<2056), aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[1], "climate future", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[1], "Drought.png",sep=" "), path = './figures/additional', width = 18, height = 9)

ggplot(data = grid.append, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[1], "(Gridmet + MACA)", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[1], "Drought+Gridmet.png",sep=" "), path = './figures/additional', width = 18, height = 9)

# CF 2

CF2<-subset(all3, CF %in% CFs[2] )
grid.append<-drt3; grid.append$CF<-CFs[2]
grid.append<-subset(grid.append, select=c(Year,CF,Pr_mm:col))
grid.append<-rbind(grid.append, subset(CF2,Year>=2020 & Year < 2070))

ggplot(data = subset(CF2,Year>=2025&Year<2056), aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[2], "climate future", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[2], "Drought.png",sep=" "), path = './figures/additional', width = 18, height = 9)

ggplot(data = grid.append, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
    geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[2], "(Gridmet + MACA)", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[2], "Drought+Gridmet.png",sep=" "), path = './figures/additional', width = 18, height = 9)


# Split into periods
Historical2<-subset(all3, Year >= 1950 & Year <2000)
min(Historical2$SPEI)

Future2<-subset(all3, Year >= 2025 & Year <2056)
min(Future2$SPEI)

# Calculate drought characteristics
Historical2$Drought=0
Historical2$Drought[which(Historical2$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(Historical2)

# Create count of years within CF
length(Historical2$CF)/length(unique(Historical2$CF))
Historical2$count<-rep(seq(1, length(Historical2$CF)/length(unique(Historical2$CF)) # 50=# years in historical period
                           , 1),length(unique(Historical2$CF))) # 4=repeat # of CFs 

Historical2$length<-0
Historical2$length <- Historical2$Drought * unlist(lapply(rle(Historical2$Drought)$lengths, seq_len))
mean(Historical2$length[Historical2$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(Historical2$length==1)
HistoricalDrought<-data.frame()
HistoricalDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
HistoricalDrought$Start = Sys.time(); HistoricalDrought$End = Sys.time()
HistoricalDrought$per<-as.factor("H")


# Calculate variables for each drought period
for (i in 1:length(D)){
  HistoricalDrought$DID[i]<-i
  HistoricalDrought$Start[i]<-strptime(Historical2$Date[D[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$Year[i]<-Historical2$Year[D[i]]
}

ND<- which((Historical2$length == 0) * unlist(lapply(rle(Historical2$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(Historical2$Drought[length(Historical2$Drought)]==1) ND[length(ND)+1]<-length(Historical2$length)

###### !!!!!!!!!!! 
# If last row in drought df is a drought period - use next line of code. Otherwies proceed.
# ND[length(ND)+1]<-length(Historical2$length) #had to add this step because last drought went until end of df so no end in ND

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  HistoricalDrought$CF[i]<-as.character(Historical2$CF[D[i]])
  HistoricalDrought$End[i]<-strptime(Historical2$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$duration[i]<-Historical2$length[ND[i]-1]
  HistoricalDrought$severity[i]<-sum(Historical2$SPEI[D[i]:(ND[i]-1)])
  HistoricalDrought$peak[i]<-min(Historical2$SPEI[D[i]:(ND[i]-1)])
}
HistoricalDrought$CF<-factor(HistoricalDrought$CF, levels=levels(Historical2$CF))

## Freq
CF.split<-split(Historical2,Historical2$CF)
for (i in 1:length(CF.split)){
  name=as.character(unique(CF.split[[i]]$CF))
  d<-which(CF.split[[i]]$length==1)
  nd<-which((CF.split[[i]]$length == 0) * unlist(lapply(rle(CF.split[[i]]$length)$lengths, seq_len)) == 1)
  if(length(nd)>length(d)) {nd=nd[2:length(nd)]}
  for (j in 1:length(d)){
    HistoricalDrought$freq[which(HistoricalDrought$CF==name & HistoricalDrought$Year==CF.split[[i]]$Year[d[j]])] <-
      CF.split[[i]]$count[d[j+1]]-CF.split[[i]]$count[nd[j]]
  }
}

####### Future
# Calculate drought characteristics
Future2$Drought=0
Future2$Drought[which(Future2$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(Future2)

# Create count of months within CF
length(Future2$CF)/length(unique(Future2$CF))
Future2$count<-rep(seq(1, length(Future2$CF)/length(unique(Future2$CF)), 
                       1),length(unique(Future2$CF))) # repeat # of CFs 

Future2$length<-0
Future2$length <- Future2$Drought * unlist(lapply(rle(Future2$Drought)$lengths, seq_len))
mean(Future2$length[Future2$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(Future2$length==1)
FutureDrought<-data.frame()
FutureDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
FutureDrought$Start = Sys.time(); FutureDrought$End = Sys.time()
FutureDrought$per<-as.factor("F")


# Calculate variables for each drought period
for (i in 1:length(D)){
  FutureDrought$DID[i]<-i
  FutureDrought$Start[i]<-strptime(Future2$Date[D[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$Year[i]<-Future2$Year[D[i]]
}

ND<- which((Future2$length == 0) * unlist(lapply(rle(Future2$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(Future2$Drought[length(Future2$Drought)]==1) ND[length(ND)+1]<-length(Future2$length)

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  FutureDrought$CF[i]<-as.character(Future2$CF[D[i]])
  FutureDrought$End[i]<-strptime(Future2$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$duration[i]<-Future2$length[ND[i]-1]
  FutureDrought$severity[i]<-sum(Future2$SPEI[D[i]:(ND[i]-1)])
  FutureDrought$peak[i]<-min(Future2$SPEI[D[i]:(ND[i]-1)])
}
FutureDrought$CF<-as.factor(FutureDrought$CF)

## Freq

CF.split<-split(Future2,Future2$CF)
for (i in 1:length(CF.split)){
  name=as.character(unique(CF.split[[i]]$CF))
  d<-which(CF.split[[i]]$length==1)
  nd<-which((CF.split[[i]]$length == 0) * unlist(lapply(rle(CF.split[[i]]$length)$lengths, seq_len)) == 1)
  if(length(nd)>length(d)) {nd=nd[2:length(nd)]}
  for (j in 1:length(d)){
    FutureDrought$freq[which(FutureDrought$CF==name & FutureDrought$Year==CF.split[[i]]$Year[d[j]])] <-
      CF.split[[i]]$count[d[j+1]]-CF.split[[i]]$count[nd[j]]
  }
}

head(HistoricalDrought)
head(FutureDrought)
Drought<-rbind(HistoricalDrought,FutureDrought)
write.csv(Drought,"./data/derived-data/Drt.all.csv",row.names=FALSE)  # csv with all drought events

Hist_char<-setNames(data.frame(matrix(ncol=6,nrow=length(levels(HistoricalDrought$CF)))),c("CF","per","Duration","Severity","Intensity","Frequency"))
Hist_char$CF<-levels(HistoricalDrought$CF)
Hist_char$per<-"H"
for (i in 1:length(Hist_char$CF)){
  name<-Hist_char$CF[i]
  Hist_char$Frequency[i]<-mean(HistoricalDrought$freq[which(HistoricalDrought$CF == name)],na.rm=TRUE)
  Hist_char$Duration[i]<-mean(HistoricalDrought$duration[which(HistoricalDrought$CF == name)])
  Hist_char$Severity[i]<-mean(HistoricalDrought$severity[which(HistoricalDrought$CF == name)])
  Hist_char$Intensity[i]<-mean(HistoricalDrought$peak[which(HistoricalDrought$CF == name)])
}


Drought_char<-setNames(data.frame(matrix(ncol=6,nrow=length(levels(FutureDrought$CF)))),c("CF","per","Duration","Severity","Intensity","Frequency"))
Drought_char$CF<-levels(FutureDrought$CF)
Drought_char$per<-"F"
for (i in 1:length(Drought_char$CF)){
  name<-Drought_char$CF[i]
  Drought_char$Frequency[i]<-mean(FutureDrought$freq[which(FutureDrought$CF == name)],na.rm=TRUE)
  Drought_char$Duration[i]<-mean(FutureDrought$duration[which(FutureDrought$CF == name)])
  Drought_char$Severity[i]<-mean(FutureDrought$severity[which(FutureDrought$CF == name)])
  Drought_char$Intensity[i]<-mean(FutureDrought$peak[which(FutureDrought$CF == name)])
  }

Drought_char<-rbind(Hist_char,Drought_char) 

# csv for averages for each CF for hist and future periods
write.csv(Drought_char,"./data/derived-data/Drought_char.csv",row.names=FALSE)
########################################### BAR PLOTS ###############################################
#Drought duration barplot
Drought_char_H = subset(Drought_char, per == "H")
Drought_char_F = subset(Drought_char, per == "F")
Drought_char_H$CF<-"Historical"
DroughtH = aggregate(cbind(Duration,Severity,Intensity,Frequency)~CF+per,Drought_char_H,mean, na.rm=TRUE)

Drought_all = rbind(DroughtH, Drought_char_F)
Drought_all$CF = factor(Drought_all$CF, levels = c("Historical",CFs))

#Change NaN's to 0's 
Drought_char_H[is.na(Drought_char_H) == TRUE] = 0
Drought_delta = data.frame(CF = Drought_char_H$CF)
Drought_delta$Duration = Drought_char_F$Duration - Drought_char_H$Duration
Drought_delta$Severity = Drought_char_F$Severity - Drought_char_H$Severity
Drought_delta$Intensity = Drought_char_F$Intensity - Drought_char_H$Intensity
Drought_delta$Frequency = Drought_char_F$Frequency - Drought_char_H$Frequency
Drought_delta$CF = factor(Drought_delta$CF, levels = c(CFs))

#Drought duraton barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Duration), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Years", 
       title=paste(SiteID, "- Average Drought Duration")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Duration.png"), path = './figures/additional', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought severity barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Severity), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Severity (SPEI * duration)", 
       title=paste(SiteID, "- Average Drought Severity")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Severity.png"), path = './figures/additional', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought intensity barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Intensity), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Intensity (Minimum SPEI values)", 
       title=paste(SiteID, "- Average Drought Intensity")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Intensity.png"), path = './figures/additional', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought-free interval barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Frequency), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Years", 
       title=paste(SiteID, "- Average Drought-Free Interval")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Frequency.png"), path = './figures/additional', height=PlotHeight, width=PlotWidth, dpi=600)

