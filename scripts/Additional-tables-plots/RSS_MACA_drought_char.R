

############################### FORMAT DATAFRAMES  ############################################
# Historical
head(Gridmet)
Gridmet$TavgC<-(Gridmet$TavgF-32)*5/9
Gridmet$Prcpmm<-Gridmet$PrcpIn*25.4
Gridmet$Month<-format(Gridmet$Date,format="%m")
d<-aggregate(Prcpmm~Month+Year,Gridmet,sum)
d2<-aggregate(TavgC~Month+Year,Gridmet,mean)
drt<-merge(d,d2,by=c("Month","Year"));rm(d,d2)
drt<-drt[with(drt, order(Year, Month)),]
drt$PET<-thornthwaite(drt$TavgC,lat = Lat)

# Run SPEI on gridmet
tp<-ts(drt$Prcpmm,frequency=12,start=c(1979,1))
tpet<-ts(drt$PET,frequency=12,start=c(1979,1))
SPEI<-spei(tp - tpet, SPEI_per, ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
plot(x=SPEI,main="Gridmet") #eventually prob want to figure out how to make x-axis date

drt$SPEI<-SPEI$fitted;drt$SPEI[which(is.na(drt$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
names(drt)[6]<-"SPEI"
drt3<-aggregate(cbind(Prcpmm,SPEI)~Year,drt,mean)

F<-subset(ALL_FUTURE, GCM %in% WB_GCMs$GCM, select=c(Date,Year,GCM,PrcpIn,TavgF))
F$Month<-format(F$Date,format="%m")
F$Prcpmm<-F$PrcpIn*25.4
F$TavgC<-(F$TavgF-32)*5/9

M<-aggregate(Prcpmm~Month+Year+GCM,F,sum)
Mon<-aggregate(TavgC~Month+Year+GCM,F,mean)
Mon<-merge(Mon,M,by=c("Month","Year","GCM"));rm(M)
Mon$PET<-thornthwaite(Mon$TavgC,lat=Lat)

# add in Historical data for baseline calc
hist.sample <- subset(drt, Year > 1979 & Year < 1982)#MACA starts at 2023 but need continuous data for timeseries calc - create data for 2021&2022 from 1980-1981
hist.sample$Year = c(rep(2021,12), rep(2022,12))
drt<-rbind(drt,hist.sample); rm(hist.sample)

drt$GCM<-WB_GCMs$GCM[1]
d<-drt; d$GCM <- WB_GCMs$GCM[2]
drt<-rbind(drt,d)
d<-subset(drt, select=c("Month","Year","GCM","TavgC","Prcpmm","PET"))
Mon <- rbind(d,Mon)

Mon<-merge(Mon,CF_GCM,by="GCM")
Mon$CF<-factor(Mon$CF,levels=unique(Mon$CF))
MON<-aggregate(cbind(Prcpmm,PET)~Month+Year+CF,Mon,mean) 
MON<-MON[with(MON, order(CF,Year, Month)),]

CF.split<-split(MON,MON$CF) #Splits df into array by CF
# this step is done because each CF has unique historical record and SPEI normalized to average conditions at beginning of record

for (i in 1:length(CF.split)){
  name=names(CF.split)[i]
  t<-CF.split[[i]]
  tp<-ts(t$Prcpmm,frequency=12,start=c(SPEI_start,1))
  tpet<-ts(t$PET,frequency=12,start=c(SPEI_start,1))
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
  CF.split[[i]]$SPEI <- SPEI$fitted[1:length(SPEI$fitted)]
  plot(x=SPEI,main=name) #eventually prob want to figure out how to make x-axis date
}

all2<- ldply(CF.split, data.frame) #convert back to df
all2$SPEI[which(is.na(all2$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(Prcpmm,SPEI)~Year+CF,all2,mean)

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
ggsave("Recent Drought.png", path = './figures/MACA', width = 18, height = 9)

# MACA prep dataframe
all3$col[all3$SPEI>=0]<-"wet"
all3$col[all3$SPEI<0]<-"dry"
all3$col<-factor(all3$col, levels=c("wet","dry"))
all3$Year<-as.numeric(all3$Year)

# CF 
CF1<-subset(all3, CF %in% CFs[1] )

ggplot(data = subset(CF1,Year>=Yr-Range/2 & Year<=Yr+Range/2), aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[1], "climate future", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[1], "Drought.png",sep=" "), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

ggplot(data = CF1, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[1], "(Gridmet + MACA)", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[1], "Drought+Gridmet.png",sep=" "), path = './figures/MACA', width = 18, height = 9)

# CF 2

CF2<-subset(all3, CF %in% CFs[2] )

ggplot(data = subset(CF2,Year>=Yr-Range/2 & Year<=Yr+Range/2), aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
  geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[2], "climate future", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[2], "Drought.png",sep=" "), path = './figures/MACA', width = 18, height = 9)

ggplot(data = CF2, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray") +
  geom_bar(stat="identity",aes(fill=col),col="black") +
    geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
  scale_fill_manual(name="",values =c("blue","red")) +
  labs(title = paste("SPEI values for", CFs[2], "(Gridmet + MACA)", sep = " " ), 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
ggsave(paste(CFs[2], "Drought+Gridmet.png",sep=" "), path = './figures/MACA', width = 18, height = 9)


# Split into periods
drt3<-subset(drt3, Year <=2012)
min(drt3$SPEI)

Future.drt<-subset(all3, Year >= Yr-Range/2 & Year <= Yr+Range/2)
min(Future.drt$SPEI)

# Calculate drought characteristics
drt3$Drought=0
drt3$Drought[which(drt3$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(drt3)

# Create count of years within CF
length(drt3$Year)
drt3$count<-seq(1, length(drt3$Year),1) 

drt3$length<-0
drt3$length <- drt3$Drought * unlist(lapply(rle(drt3$Drought)$lengths, seq_len))
mean(drt3$length[drt3$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(drt3$length==1)
HistoricalDrought<-data.frame()
HistoricalDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
HistoricalDrought$Start = Sys.time(); HistoricalDrought$End = Sys.time()
HistoricalDrought$per<-as.factor("H")


# Calculate variables for each drought period
for (i in 1:length(D)){
  HistoricalDrought$DID[i]<-i
  HistoricalDrought$Start[i]<-strptime(drt3$Date[D[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$Year[i]<-drt3$Year[D[i]]
}

ND<- which((drt3$length == 0) * unlist(lapply(rle(drt3$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(drt3$Drought[length(drt3$Drought)]==1) ND[length(ND)+1]<-length(drt3$length)

###### !!!!!!!!!!! 
# If last row in drought df is a drought period - use next line of code. Otherwies proceed.
# ND[length(ND)+1]<-length(drt3$length) #had to add this step because last drought went until end of df so no end in ND

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  HistoricalDrought$End[i]<-strptime(drt3$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$duration[i]<-drt3$length[ND[i]-1]
  HistoricalDrought$severity[i]<-sum(drt3$SPEI[D[i]:(ND[i]-1)])
  HistoricalDrought$peak[i]<-min(drt3$SPEI[D[i]:(ND[i]-1)])
}

## Freq
  d<-which(drt3$length==1)
  nd<-which((drt3$length == 0) * unlist(lapply(rle(drt3$length)$lengths, seq_len)) == 1)
  if(length(nd)>length(d)) {nd=nd[2:length(nd)]}
  for (j in 1:length(d)){
    HistoricalDrought$freq[which(HistoricalDrought$Year==drt3$Year[d[j]])] <-
      drt3$count[d[j+1]]-drt3$count[nd[j]]
  }

####### Future
# Calculate drought characteristics
Future.drt$Drought=0
Future.drt$Drought[which(Future.drt$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(Future.drt)

# Create count of months within CF
length(Future.drt$CF)/length(unique(Future.drt$CF))
Future.drt$count<-rep(seq(1, length(Future.drt$CF)/length(unique(Future.drt$CF)), 
                       1),length(unique(Future.drt$CF))) # repeat # of CFs 

Future.drt$length<-0
Future.drt$length <- Future.drt$Drought * unlist(lapply(rle(Future.drt$Drought)$lengths, seq_len))
mean(Future.drt$length[Future.drt$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(Future.drt$length==1)
FutureDrought<-data.frame()
FutureDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
FutureDrought$Start = Sys.time(); FutureDrought$End = Sys.time()
FutureDrought$per<-as.factor("F")


# Calculate variables for each drought period
for (i in 1:length(D)){
  FutureDrought$DID[i]<-i
  FutureDrought$Start[i]<-strptime(Future.drt$Date[D[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$Year[i]<-Future.drt$Year[D[i]]
}

ND<- which((Future.drt$length == 0) * unlist(lapply(rle(Future.drt$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(Future.drt$Drought[length(Future.drt$Drought)]==1) ND[length(ND)+1]<-length(Future.drt$length)

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  FutureDrought$CF[i]<-as.character(Future.drt$CF[D[i]])
  FutureDrought$End[i]<-strptime(Future.drt$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$duration[i]<-Future.drt$length[ND[i]-1]
  FutureDrought$severity[i]<-sum(Future.drt$SPEI[D[i]:(ND[i]-1)])
  FutureDrought$peak[i]<-min(Future.drt$SPEI[D[i]:(ND[i]-1)])
}
FutureDrought$CF<-as.factor(FutureDrought$CF)

## Freq

CF.split<-split(Future.drt,Future.drt$CF)
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
write.csv(Drought,"./data/park-specific/output/Drt.all.csv",row.names=FALSE)  # csv with all drought events

Hist_char<-setNames(data.frame(matrix(ncol=6,nrow=1)),c("CF","per","Duration","Severity","Intensity","Frequency"))
Hist_char$CF<-"Historical"
Hist_char$per<-"H"
Hist_char$Frequency<-mean(HistoricalDrought$freq,na.rm=TRUE)
Hist_char$Duration<-mean(HistoricalDrought$duration)
Hist_char$Severity<-mean(HistoricalDrought$severity)
Hist_char$Intensity<-mean(HistoricalDrought$peak)


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
write.csv(Drought_char,"./data/park-specific/output/Drought_char.csv",row.names=FALSE)
########################################### BAR PLOTS ###############################################
#Drought duration barplot
Drought_all = Drought_char
Drought_all$CF = factor(Drought_all$CF, levels = c("Historical",CFs))

#Change NaN's to 0's 
Drought_char_H[is.na(Hist_char) == TRUE] = 0
Drought_delta = data.frame(CF=Drought_char_F$CF)
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
ggsave(paste(SiteID, "Duration.png"), path = './figures/MACA', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought severity barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Severity), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Severity (SPEI * duration)", 
       title=paste(SiteID, "- Average Drought Severity")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Severity.png"), path = './figures/MACA', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought intensity barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Intensity), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Intensity (Minimum SPEI values)", 
       title=paste(SiteID, "- Average Drought Intensity")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Intensity.png"), path = './figures/MACA', height=PlotHeight, width=PlotWidth, dpi=600)

#Drought-free interval barplot
ggplot(Drought_all, aes(x=CF, y=as.numeric(Frequency), fill=CF)) + geom_bar(stat="identity", col="black") + 
  scale_y_continuous() + 
  labs(x="", y="Years", 
       title=paste(SiteID, "- Average Drought-Free Interval")) + 
  scale_fill_manual(values = colors3) + 
  BarPlotTheme
ggsave(paste(SiteID, "Frequency.png"), path = './figures/MACA', height=PlotHeight, width=PlotWidth, dpi=600)

