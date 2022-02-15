# RSS_Plotting_Bar_Charts.R

################ INITIALS ##########################

# Need to check all the subsets
colors3<-c("white",colors2)

##Plot parameters

#Height and width 
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom")  

BarPlotTheme = theme(axis.text.x=element_text(size=24),    #Text size for axis tick mark labels
                     axis.text.y=element_text(size=20),
                     axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                     axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                     plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                     legend.position = "none") 

#X-axis labels for monthly plots
MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
SeasonLabels = c("Winter", "Spring","Summer", "Fall")


#################### END INITIALS ##########################
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

##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TavgF, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily temperature in ", Yr, " vs 1979-2012"),
       x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TavgF)))) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_Avg_Monthly_Tavg_Delta_Line.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


#################################################### MONTHLY/SEASONAL DELTA PLOTS ###############################################################3
#Bar graph of change in average monthly precip by CF
ggplot(Monthly_delta, aes(x=Month,y=PrcpIn,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. monthly precip. in ", Yr, " vs 1979-2012"), 
       x = "Month", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="",values = colors2) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_Avg_Monthly_Precip_Delta_Bar.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average seasonal precip by CF
ggplot(Season_delta, aes(x=season,y=PrcpIn,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. seasonal precip. in ", Yr, " vs 1979-2012"), 
       x = "Season", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="",values = colors2) 

ggsave(sprintf("%s_Avg_Season_Precip_Delta_Bar.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


#Line plot of change in MaxTemp by CF/month
ggplot(Monthly_delta, aes(x=Month, y=TmaxF, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily Tmax in ", Yr, " vs 1979-2012"), 
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TmaxF)))) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_Avg_Monthly_Tmax_Delta_Line.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TminF, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily Tmin in ", Yr, " vs 1979-2012"),
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TminF)))) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_Avg_Monthly_Tmin_Delta_Line.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TavgF, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily temperature in ", Yr, " vs 1979-2012"),
       x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TavgF)))) +
  scale_x_discrete(labels = MonthLabels)
ggsave(sprintf("%s_Avg_Monthly_Tavg_Delta_Line.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average monthly RHmean by CF
ggplot(Monthly_delta, aes(x=Month,y=RHmean,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. monthly relative humidity\n in ", Yr, " vs 1979-2012"), 
       x = "Month", y = "Change in relative humidity (%)") +
  scale_fill_manual(name="",values = colors2) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_Avg_Monthly_RHmean_Delta_Bar.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average seasonal RHmean by CF
ggplot(Season_delta, aes(x=season,y=RHmean,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. seasonal relative humidity\n in ", Yr, " vs 1979-2012"), 
       x = "Season", y = "Change in relative humidity (%)") +
  scale_fill_manual(name="",values = colors2) 

ggsave(sprintf("%s_Avg_Season_RHmean_Delta_Bar.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

############################################### BAR PLOTS ######################################################################
# create dataset for bar graph of total number of days/year over hot temperature threshold

####################
#Total number of days/year over hot temperature threshold
var<-"OverHotTemp"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, "- Avg. Days/Yr >", HotTemp, "Deg in in ", Yr, " vs 1979-2012"), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_Days_Over_HotTemp.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, "- Avg. Days/Yr >", HotTemp, "Deg in ", Yr, " vs 1979-2012"), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_Days_Over_HotTemp-Boxplot.png", SiteID), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

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

# ####################
# #Total number of Growing degree days/year (Tavg >41F)
# var<-"GDD"
# At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
# names(At)<-c("CF",var)
# ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
#   geom_bar(stat="identity",position="dodge",colour="black") +
#   BarPlotTheme +
#   # coord_cartesian(ylim=c(0, 40)) +
#   labs(title = paste(SiteID, " - Average annual growing degree days in ", Year, sep=""),
#        y = "Days/Yr", colour = "Climate Future")  +
#   scale_fill_manual(name="",values = colors3) +
#   coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))
#
# ggsave(sprintf("%s_%s_%s_Growing-day.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
#
# # Boxplot
# p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) +
#   geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+
#   geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
#   BarPlotTheme +
#   labs(title = paste(SiteID, " - Average annual growing degree days in ", Year, sep=""),
#        y = "Days/Yr") +
#   scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
#   scale_fill_manual(name="",values = colors3)
# dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
# p + geom_segment(data=dat1, aes(x=xmin, xend=xmax,
#                                 y=middle, yend=middle), colour="grey", size=1)
# ggsave(sprintf("%s_%s_%s_Growing-day-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


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
ggsave(sprintf("%s_%s_%s_Growing-season-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


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
#var<-"Sp.Frost"
#<-aggregate(eval(parse(text=var))~CF,Annual,mean);
#names(At)<-c("CF",var)
#ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  #geom_bar(stat="identity",position="dodge",colour="black") +
  #BarPlotTheme +
   #coord_cartesian(ylim=c(0, 40)) +
  #labs(title = paste(SiteID, " - Average annual spring frost days in ", Year, sep=""),
       #y = "Days/Yr", colour = "Climate Future")  +
  #scale_fill_manual(name="",values = colors3) +
  #coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

#ggsave(sprintf("%s_%s_%s_Spring_frost.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual spring frost days in ", Year, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Spring_frost-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year with 'dangerous' heat index values
var<-"HI.Dan"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual dangerous heat index days in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Heat-index-danger.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual dangerous heat index days in ", Year, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Heat-index-danger-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Annual Tavg
var<-"TavgF"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual temperature (deg F) in ", Year, sep=""), 
       y = "Deg F", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Average-temp.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual temperature (deg F) in ", Year, sep=""),
       y = "Deg F") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Heat-index-danger-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Annual Precip
var<-"PrcpIn"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual precipitation (in) in ", Year, sep=""), 
       y = "inches/yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Average-temp.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Average annual precipitation (in)) in ", Year, sep=""),
       y = "inches/yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Heat-index-danger-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


############################################### PRINT TABLES #################################################################
A<-aggregate(.~CF,Annual[,2:23], mean) 
write.xlsx(list("Means"=A,"A3nual"=Annual,"Season"=Season,"D_Season"=Season_delta,"Monthly"=Monthly,"Monthly_delta"=Monthly_delta), 
           file=("./data/park-specific/output/Plot_data.xlsx"),col.names=TRUE)

