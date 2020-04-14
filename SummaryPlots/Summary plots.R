rm(list=ls())
library(ggplot2)
library(zoo)
library(gridExtra)
library(grid)

setwd("C:/Users/adillon/Documents/RSS/CONG")

load("PRISM/CONG_33.791868_-80.748665_PRISM_PptTminTmax_IntermediateFiles.RData")
load("MACA/Figs MACA/CONG_33.791868_-80.748665_Final_Environment.RData")
Future_all<-merge(ALL_FUTURE,CF_GCM,by="GCM")
grid<-read.csv("Gridmet/GridMet.csv",header=T)

BC.min = 1979 #Bias correction range
BC.max = 2017

CF.sub = c("Historical", "Warm Damp", "Hot Wet") #CFs using
# col<- c("darkgray","#9A9EE5","#E10720")  # WarmWet/HotDry
col<- c("darkgray","#F3D3CB","#12045C")  # HotWet/WarmDry
#col<- c("darkgray","light green","orange")  # HotWet/WarmDry

############################################ Format Gridmet data ####################################################
head(grid)
grid$tmean<-(grid$tmax+grid$tmin)/2
grid$Date = as.Date(as.character(grid$Date, "%m/%d/%Y"))
grid$year = strftime(grid$Date, "%Y")
grid.yrAvgs = aggregate(tmean ~ year, data=grid, FUN=mean)

ppt.yrAvgs = aggregate(precip~year, data=grid, FUN=sum)

grid.yrAvgs = merge(grid.yrAvgs,ppt.yrAvgs,by="year")
grid.yrAvgs$CF<-"Historical"


############################################ Format PRISM data #####################################################
PptMeans$year = strftime(PptMeans$Date, "%Y")
PRISM.tmean = data.frame(Date = TmaxMeans$Date, TavgCustom = (TmaxMeans$TmaxF + TminMeans$TminF)/2)
PRISM.tmean$year = strftime(PRISM.tmean$Date, "%Y")

PRISM.yrAvgs = aggregate(TavgCustom ~ year, data=PRISM.tmean, FUN=mean)
PRISM.ppt.yrAvgs = aggregate(PptIn ~ year, data=PptMeans, FUN=sum)
PRISM.yrAvgs = merge(PRISM.yrAvgs,PRISM.ppt.yrAvgs,by="year")
PRISM.yrAvgs$year = as.numeric(as.character(PRISM.yrAvgs$year))


########################################### Bias correction ########################################################
Grid.tmean = mean(grid.yrAvgs$tmean[which(grid.yrAvgs$year>=BC.min & grid.yrAvgs$year<=BC.max)])
PRISM.tmean = mean(PRISM.yrAvgs$TavgCustom[which(PRISM.yrAvgs$year>=BC.min & PRISM.yrAvgs$year <=BC.max)])
BC.tmean = Grid.tmean - PRISM.tmean

Grid.ppt = mean(grid.yrAvgs$precip[which(grid.yrAvgs$year>=BC.min & grid.yrAvgs$year<=BC.max)])
PRISM.ppt = mean(PRISM.yrAvgs$PptIn[which(PRISM.yrAvgs$year>=BC.min & PRISM.yrAvgs$year <=BC.max)])
BC.ppt = Grid.ppt - PRISM.ppt

#Bias-corrected values
PRISM.BC = data.frame(year = PRISM.yrAvgs$year, 
                          CF = "Historical",
                          Tavg.mean = PRISM.yrAvgs$TavgCustom + BC.tmean,
                          Tavg.max = NA,
                          Tavg.min = NA,
                          Precip.mean = PRISM.yrAvgs$PptIn + BC.ppt,
                          Precip.max = NA,
                          Precip.min = NA)

########################################### Format MACA data #######################################################
Future_all$year = strftime(Future_all$Date, "%Y")
MACA.tmean.yrAvgs = aggregate(TavgCustom ~ year+GCM+CF, data=Future_all, FUN=mean)
MACA.ppt.yrAvgs = aggregate(PrecipCustom ~ year+GCM+CF, data=Future_all, FUN=sum)

MACA.tmean.mean = aggregate(TavgCustom ~ year+CF, data=MACA.tmean.yrAvgs, FUN=mean)
MACA.tmean.min = aggregate(TavgCustom ~ year+CF, data=MACA.tmean.yrAvgs, FUN=min)
MACA.tmean.max = aggregate(TavgCustom ~ year+CF, data=MACA.tmean.yrAvgs, FUN=max)

MACA.ppt.mean = aggregate(PrecipCustom ~ year+CF, data=MACA.ppt.yrAvgs, FUN=mean)
MACA.ppt.min = aggregate(PrecipCustom ~ year+CF, data=MACA.ppt.yrAvgs, FUN=min)
MACA.ppt.max = aggregate(PrecipCustom ~ year+CF, data=MACA.ppt.yrAvgs, FUN=max)

MACA.yrAvgs = MACA.tmean.mean
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.tmean.max, by=c("year", "CF"))
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.tmean.min, by=c("year", "CF"))
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.ppt.mean, by=c("year", "CF"))
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.ppt.max, by=c("year", "CF"))
MACA.yrAvgs = merge(MACA.yrAvgs, MACA.ppt.min, by=c("year", "CF"))
colnames(MACA.yrAvgs) = c("year", "CF", "Tavg.mean", "Tavg.max", "Tavg.min", "Precip.mean", "Precip.max", "Precip.min")

PRISM.BC = subset(PRISM.BC,year<2016)
MACA.yrAvgs = subset(MACA.yrAvgs, year > 2019 & year < 2099)

yrAvgs = rbind(PRISM.BC, MACA.yrAvgs)
yrAvgs.sub = subset(yrAvgs, CF %in% CF.sub)


############################################# Plotting ###########################################################
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_blank(),      #No title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20), #Text size of legend title
                  legend.position = c(0,1),legend.justification = c(-.1,1.1),  #Set top left
                  panel.border = element_blank(), #Remove border around plot
                  axis.line = element_line(colour = "black"), #Add axis lines
                  panel.background = element_blank(), #Background white
                  panel.grid.major = element_line("light grey",0.3)) #add grid back
#Height and width 
PlotWidth = 15
PlotHeight = 9

# Rolling 10 year avg
tmeanAvg = with(PRISM.BC, tapply(Tavg.mean, year, mean))
pptAvg = with(PRISM.BC, tapply(Precip.mean, year, mean))
rollLen = 10
PRISM.BC$Tmean.roll10 <- rollmean(PRISM.BC$Tavg.mean, rollLen,fill=NA)
PRISM.BC$Precip.roll10 <- rollmean(PRISM.BC$Precip.mean, rollLen,fill=NA)
yrAvgs.sub<-merge(yrAvgs.sub,PRISM.BC[,c("year","Tmean.roll10","Precip.roll10")],by="year",all=TRUE)


# Tmean
ggplot(yrAvgs.sub, aes(x=as.numeric(as.character(year)), y=Tavg.mean, col=CF, fill=CF)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
  geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Tavg.min, ymax=Tavg.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=as.numeric(as.character(year)), y=Tmean.roll10),size=1.5,colour="gray37",na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs(x="Year", y=expression("Mean annual temperature "~(degree~F))) +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme
ggsave(paste(SiteID,"-tmean.png",sep=""), height=PlotHeight, width=PlotWidth)

# Precip
ggplot(yrAvgs.sub, aes(x=as.numeric(as.character(year)), y=Precip.mean, col=CF, fill=CF)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=as.numeric(as.character(year)), y=Precip.roll10),size=1.5,colour="gray37",na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual precipitation (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme
ggsave(paste(SiteID,"-precip.png",sep=""), height=PlotHeight, width=PlotWidth)

#Panel plot
t<-ggplot(yrAvgs.sub, aes(x=as.numeric(as.character(year)), y=Tavg.mean, col=CF, fill=CF)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
  geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Tavg.min, ymax=Tavg.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=as.numeric(as.character(year)), y=Tmean.roll10),size=1.5,colour="gray37",na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs(title=paste("Historical and future projections for",SiteID, sep=" "),
       x="Year", y=expression("Mean annual temperature "~(degree~F))) +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) +
  theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        axis.title.y=element_text(size=20, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        plot.title=element_text(size=24,hjust=.5),  
        legend.position = "none",  #Set top left
        panel.border = element_blank(), #Remove border around plot
        axis.line = element_line(colour = "black"), #Add axis lines
        panel.background = element_blank(), #Background white
        panel.grid.major = element_line("light grey",0.3)) #add grid back
t

p<-ggplot(yrAvgs.sub, aes(x=as.numeric(as.character(year)), y=Precip.mean, col=CF, fill=CF)) + 
  geom_rect(xmin=2025, xmax=2055, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=as.numeric(as.character(year)), y=Precip.roll10),size=1.5,colour="gray37",na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual precipitation (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + 
  theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
        axis.title.x=element_text(size=20, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
        axis.title.y=element_text(size=20, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        plot.title=element_blank(),      #No title       
        legend.position = "bottom",  #Set top left
        legend.text=element_text(size=20), legend.title=element_text(size=20),
        panel.border = element_blank(), #Remove border around plot
        axis.line = element_line(colour = "black"), #Add axis lines
        panel.background = element_blank(), #Background white
        panel.grid.major = element_line("light grey",0.3)) #add grid back
p


grid.arrange(t,p, nrow=2)

g <- arrangeGrob(t,p, nrow=2)
ggsave("Long-term_panel-gridmet.png", g,width = 15, height = 15)
