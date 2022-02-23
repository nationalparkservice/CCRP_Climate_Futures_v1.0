# RSS_Plotting_Bar_Charts.R
DataFile <- list.files(path = DataDir, pattern = 'Final_Environment.RData', full.names = TRUE) # Environment needs to be added if not parsing MACA data
load(DataFile)

#################################################### SUBSET DATAFRAMES ###################################################
Monthly<-subset(Monthly,CF %in% FutureSubset); Monthly$CF<-factor(Monthly$CF, levels=c("Historical",FutureSubset))
# Monthly$Month<-factor(Monthly$Month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


Monthly_delta<-subset(Monthly_delta,CF %in% FutureSubset); Monthly_delta$CF<-factor(Monthly_delta$CF, levels=c(FutureSubset))
# Monthly_delta$Month<-factor(Monthly_delta$Month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

Season<-subset(Season,CF %in% FutureSubset); Season$CF<-factor(Season$CF, levels=c("Historical",FutureSubset))
Season$season<-factor(Season$season,levels=c("Winter","Spring","Summer","Fall"))

Season_delta<-subset(Season_delta,CF %in% FutureSubset); Season_delta$CF<-factor(Season_delta$CF, levels=c(FutureSubset))
Season_delta$season<-factor(Season_delta$season,levels=c("Winter","Spring","Summer","Fall"))

# Join historical and future for bar plots
H <- subset(H_annual, select = -c(GCM))
F_annual<-subset(F_annual, CF %in% FutureSubset)
Fut_annual<-aggregate(.~Year+CF,subset(F_annual, select = -c(GCM)),mean)
Annual<-rbind(H, Fut_annual)
Annual$CF<-factor(Annual$CF,levels=c("Historical",CFs), ordered=is.ordered(Annual$CF))

MACA_avgPr <- mean(H$PrcpIn)

### Add column to change color of CF1
Annual$me.col<-"b"
Annual$me.col[which(Annual$CF=="Hot Wet")]<-"w" #If not using HW, will get error


############################################################### Begin output plots ############################################
## Quadrant means scatterplots

#### quadrant means
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))

dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[1])]), y=mean(365*DeltaPr[which(CF==CFs[1])])), shape=8, size=7, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[2])]), y=mean(365*DeltaPr[which(CF==CFs[2])])), shape=8, size=7, stroke=3, colour=colors2[2]) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in ", Yr, " by GCM run",sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave("scatter-CFmeansStar.png", width = PlotWidth, height = PlotHeight, path = FigDir)

#### quadrant means + WB selected models
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))

dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[1])]), y=mean(365*DeltaPr[which(CF==CFs[1])])), shape=8, size=7, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(CF==CFs[2])]), y=mean(365*DeltaPr[which(CF==CFs[2])])), shape=8, size=7, stroke=3, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[1])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[1])])), shape=21, size=10, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[2])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[2])])), shape=21, size=10, stroke=3, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[1])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[1])])), shape=20, size=2,  colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTavg[which(WB_GCM==CFs[2])]), y=mean(365*DeltaPr[which(WB_GCM==CFs[2])])), shape=20, size=2,  colour=colors2[2]) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in ", Yr, " by GCM run",sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave("scatter-CFmeansStar-IndivCircled.png", width = PlotWidth, height = PlotHeight, path = FigDir)


################################## Monthly/Seasonal delta plots #####################################

Month_line_plot(Monthly_delta,xvar=Month,yvar=TavgF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TavgFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_line_plot(Monthly_delta,xvar=Month,yvar=TminF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily min temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TminFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_line_plot(Monthly_delta,xvar=Month,yvar=TmaxF,grp=CF,cols=colors2,
                title=paste0("Change in avg daily max temperature in ",Yr," vs ", BasePeriod),
                xlab = "Month", ylab="Change in temperature (\u00B0F)")
ggsave("Monthly-line-TmaxFDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Monthly_delta,xvar=Month,yvar=PrcpIn,grp=CF,cols=colors2,
               title=paste0("Change in avg monthly precipitation in ",Yr," vs ", BasePeriod),
               xlab = "Month", ylab="Change in precipitation (in)",label=MonthLabels)
ggsave("Monthly-bar-PrcpInDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Season_delta,xvar=season,yvar=PrcpIn,grp=CF,cols=colors2,
               title=paste0("Change in avg seasonal precipitation in ",Yr," vs ", BasePeriod),
               xlab = "Season", ylab="Change in precipitation (in)",label=SeasonLabels)
ggsave("Seasonal-bar-PrcpInDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Monthly_delta,xvar=Month,yvar=RHmean,grp=CF,cols=colors2,
               title=paste0("Change in avg monthly relative humidity in ",Yr," vs ", BasePeriod),
               xlab = "Month", ylab="Change in relative humidity (%)",label=MonthLabels)
ggsave("Monthly-bar-RHmeanDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

Month_bar_plot(Season_delta,xvar=season,yvar=RHmean,grp=CF,cols=colors2,
               title=paste0("Change in avg seasonal relative humidity in ",Yr," vs ", BasePeriod),
               xlab = "Season", ylab="Change in relative humidity (%)",label=SeasonLabels)
ggsave("Seasonal-bar-RHmeanDelta.png", width = PlotWidth, height = PlotHeight, path = FigDir)

############################################### BAR PLOTS ######################################################################

####################
#Total number of days/year over hot temperature threshold
var_bar_plot(Annual, "OverHotTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-bar-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_box_plot(Annual, "OverHotTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F) in ", Yr, " vs ", BasePeriod))
ggsave("Annual-box-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

var_line_plot(Annual, var=OverHotTemp, cols=col, title=paste0("Average Days/Yr > ", HotTemp, " (\u00B0F)"),
                                                              ylab="Days/Yr")
ggsave("Annual-line-OverHotTemp.png", width = PlotWidth, height = PlotHeight, path = FigDir)

####################
#Total number of days/year under cold temperature threshold
var<-"UnderColdTemp"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr <", ColdTemp, "Deg \n in ", Yr, " vs 1979-2012"), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Under_ColdTemp.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

DaysUnderCold <- At

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr <", ColdTemp, "Deg \n in in ", Yr, " vs 1979-2012"), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Under_ColdTemp-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year under 5th low temperature threshold
var<-"UnderLowQ"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Tmin < Historic 5th Percentile (", round(HistTminLow, 1), "˚F) in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Under_5th.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Tmin < Historic 5th Percentile (", round(HistTminLow, 1), "˚F) in ", Yr, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Under_5th-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Total number of days/year over 95th low temperature threshold
var<-"OverHighQ"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Tmax > Historic 95th Percentile (", round(HistTmaxHigh, 1), "˚F) in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Over_95th.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

OverHighQ <- At

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Tmax > Historic 95th Percentile (", round(HistTmaxHigh, 1), "˚F) in ", Yr, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Over_95th-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year over 95th precip threshold
var<-"OverPrecip95"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > Historic 95th Percentile (", round(HistPrecip95, 1), "in) in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Over_95th_Precip.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

Over95Pr <- At # summary for presentation

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > Historic 95th Percentile (", round(HistPrecip95, 1), "in) in ", Yr, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Over_95th_Precip-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 1" threshold
var<-"PrecipOver1"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 1 in. in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Over_1_Precip.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 1 in. in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Over_1_Precip-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 2" threshold
var<-"PrecipOver2"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 2 in. in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Over_2_Precip.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 2 in. in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s__Days_Over_2_Precip-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 2" threshold
var<-"FThaw"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Annual freeze-thaw cycles in ", Yr, sep=""), 
       y = "Cycles/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Freeze-thaw.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Annual freeze-thaw cycles in ", Yr, sep=""),
       y = "Cycles/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Freeze-thaw-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Total number of Growing degree days/year (Tavg >41F)
var<-"GDD"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual growing degree days in ", Yr, sep=""),
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Growing-day.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) +
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual growing degree days in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax,
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Growing-day-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


#Total number of days/year in growing season (ClimDex defn)
var<-"GrowLen"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual growing season length in ", Yr, sep=""),
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Growing-season.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) +
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual growing season length in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax,
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Growing-season-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Beginning of growing season - green-up (Julian Day)
var<-"BegGrow"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual green-up date (Julian day) in ", Yr, sep=""),
       y = "Julian day", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Green-up.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) +
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual green-up date (Julian day) in ", Yr, sep=""),
       y = "Julian day") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax,
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Green-up-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################

# Total number of days/year with spring frost (GDD==T, Tmin<32)
var<-"Sp.Frost"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
   # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual spring frost days in ", Yr, sep=""),
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Spring_frost.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual spring frost days in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Spring_frost-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year with 'dangerous' heat index values
var<-"HI.Dan"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual dangerous heat index days in ", Yr, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Heat-index-danger.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual dangerous heat index days in ", Yr, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Heat-index-danger-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Annual Tavg
var<-"TavgF"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual temperature (deg F) in ", Yr, sep=""), 
       y = "Deg F", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Average-temp.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual temperature (deg F) in ", Yr, sep=""),
       y = "Deg F") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Avg-temp-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Annual Precip
var<-"PrcpIn"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual precipitation (in) in ", Yr, sep=""), 
       y = "inches/yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Average-temp.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual precipitation (in) in ", Yr, sep=""),
       y = "inches/yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Heat-index-danger-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


############################################### PRINT TABLES #################################################################
A<-aggregate(.~CF,Annual[,c(1,3:27)], mean) 
write.xlsx(list("Means"=A,"Annual"=Annual,"Season"=Season,"D_Season"=Season_delta,"Monthly"=Monthly,"Monthly_delta"=Monthly_delta), 
           file=("./data/park-specific/output/Plot_data.xlsx"),col.names=TRUE)

