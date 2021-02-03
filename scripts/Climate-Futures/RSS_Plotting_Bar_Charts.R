# RSS_Plotting_Bar_Charts.R

################ INITIALS ##########################

# Need to check all the subsets
### NEED TO CHANGE LINE 72 for ordering scenarios on plots

Scenario1<-FutureSubset[1]
Scenario2<-FutureSubset[2]

#Colors for RCP 4.5, RCP 8.5
col.RCP2 = c("blue", "red")

## All 508-compliant color scheme -- navy (hot wet), light blue (warm wet), pink (warm dry), red (hot dry)
colors5 <-   c("white","#12045C","#9A9EE5","#F3D3CB","#E10720")
colors2<- c("#9A9EE5","#E10720")  # WarmWet/HotDamp
#colors2<- c("#F3D3CB","#12045C")  # HotWet/WarmDry

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
H<-H_annual[,-c(1:2)]
Hist_annual<-aggregate(.~Year,data=H,mean);rm(H)
Hist_annual$CF<-"Historical"
Hist_annual<-Hist_annual[,c("Year","CF",names(Hist_annual[,2:26]))] 
F_annual<-subset(F_annual, CF %in% FutureSubset, select = -c(GCM))
Fut_annual<-aggregate(.~Year+CF,F_annual,mean)
Annual<-rbind(Hist_annual,Fut_annual)
Annual$CF<-factor(Annual$CF,levels=c("Historical",Scenario1, Scenario2), ordered=is.ordered(Annual$CF))

MACA_avgPr <- mean(Hist_annual$PrecipCustom)

# Sample from Historical and create joined dataframes for boxplots
# set.seed(7)
# H_samp<-sample_n(H_annual,30); H_samp$CF<-"Historical"
# H_samp$GCM<-NULL
# Annual_samp<-rbind(H_samp,Fut_annual)

set.seed(7)
H<-subset(H_annual, select= -c(CF))
H_samp<-merge(H,CF_GCM, by="GCM");H_samp$GCM=NULL
H_samp<-aggregate(.~Year+CF,H_samp,mean)
H_samp<-sample_n(H_samp,30); H_samp$CF<-"Historical"
Annual_samp<-rbind(H_samp,Fut_annual)
Annual_samp$CF<-factor(Annual_samp$CF,levels=c("Historical",Scenario1, Scenario2), ordered=is.ordered(Annual_samp$CF))

### Add column to change color of CF1
Annual_samp$me.col<-"b"
Annual_samp$me.col[which(Annual_samp$CF=="Hot Wet")]<-"w" #If not using HW, will get error


############################################################### Begin output plots ############################################

###Scatter plot showing delta precip and tavg, color by emissions scenario, x-axis scaled 0-max, with points for averages of 3 CFs
###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for all 3 CF's
if(grepl("Warm", Scenario1)){
  xmin1 <- Tavg0
  xmax1 <- Tavg
  xmin2 <- Tavg
  xmax2 <- Tavg100
  col1 <- "blue"
  col2 <- "red"
}else{ 
  xmin1 <- Tavg
  xmax1 <- Tavg100
  xmin2 <- Tavg0
  xmax2 <- Tavg
  col1 <- "red"
  col2 <- "blue"
}
if(grepl("Dry", Scenario1) | grepl("Damp", Scenario1)){
  ymin1 <- 365*Pr0
  ymax1 <- 365*PrAvg
  ymin2 <- 365*PrAvg
  ymax2 <- 365*Pr100
}else{
  ymin1 <- 365*PrAvg
  ymax1 <- 365*Pr100
  ymin2 <- 365*Pr0
  ymax2 <- 365*PrAvg
}
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)") +
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions Scenarios\n")) +
  geom_rect(color = "blue", alpha=0) +
   geom_hline(aes(yintercept=365*mean(Future_Means$DeltaPr)),linetype=2) +
   geom_vline(aes(xintercept=mean(Future_Means$DeltaTavg)),linetype=2)  +
  geom_rect(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1, color = colors2[1], alpha=0, size=1) + 
  geom_rect(xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75, color = "grey", alpha=0, size=1) +
  geom_rect(xmin=xmin2, xmax=xmax2, ymin=ymin2, ymax=ymax2, color = colors2[2], alpha=0, size=1) +
  geom_point(aes(x=mean(DeltaTavg), y=mean(365*DeltaPr)), shape=23, size=10, fill='black', colour='black') +
  geom_point(aes(x=mean(DeltaTavg[which(CF==Scenario1)]), y=mean(365*DeltaPr[which(CF==Scenario1)])), shape=23, size=10, fill='black', colour='black') +
  geom_point(aes(x=mean(DeltaTavg[which(CF==Scenario2)]), y=mean(365*DeltaPr[which(CF==Scenario2)])), shape=23, size=10, fill='black', colour='black') +
  scale_x_continuous(limits=c(0, max(Future_Means$DeltaTavg)+.25))



##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TavgCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily temperature in 2040 (2025-2055) vs 1950-1999"),
       x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TavgCustom)))) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tavg_Delta_Line.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
ggsave(sprintf("%s_%s_%s_GCM_Scatter_CF_Averages_Plot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


#################################################### MONTHLY/SEASONAL DELTA PLOTS ###############################################################3
#Bar graph of change in average monthly precip by CF
ggplot(Monthly_delta, aes(x=Month,y=PrecipCustom,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. monthly precip. in 2040 (2025-2055) vs 1950-1999"), 
       x = "Month", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="",values = colors2) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Precip_Delta_Bar.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average seasonal precip by CF
ggplot(Season_delta, aes(x=season,y=PrecipCustom,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. seasonal precip. in 2040 (2025-2055) vs 1950-1999"), 
       x = "Season", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="",values = colors2) 

ggsave(sprintf("%s_%s_%s_Avg_Season_Precip_Delta_Bar.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


#Line plot of change in MaxTemp by CF/month
ggplot(Monthly_delta, aes(x=Month, y=TmaxCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily Tmax in 2040 (2025-2055) vs 1950-1999"), 
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TmaxCustom)))) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmax_Delta_Line.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TminCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily Tmin in 2040 (2025-2055) vs 1950-1999"),
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TminCustom)))) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmin_Delta_Line.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


##Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TavgCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. daily temperature in 2040 (2025-2055) vs 1950-1999"),
       x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TavgCustom)))) +
  scale_x_discrete(labels = MonthLabels)
ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tavg_Delta_Line.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average monthly RHmean by CF
ggplot(Monthly_delta, aes(x=Month,y=RHmean,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. monthly relative humidity\n in 2040 (2025-2055) vs 1950-1999"), 
       x = "Month", y = "Change in relative humidity (%)") +
  scale_fill_manual(name="",values = colors2) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_RHmean_Delta_Bar.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

#Bar graph of change in average seasonal RHmean by CF
ggplot(Season_delta, aes(x=season,y=RHmean,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in avg. seasonal relative humidity\n in 2040 (2025-2055) vs 1950-1999"), 
       x = "Season", y = "Change in relative humidity (%)") +
  scale_fill_manual(name="",values = colors2) 

ggsave(sprintf("%s_%s_%s_Avg_Season_RHmean_Delta_Bar.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

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
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr >", HotTemp, "Deg in Historical (1950-1999) & Future (2025-2055)"), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Over_HotTemp.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr >", HotTemp, "Deg in Historical (1950-1999) & Future (2025-2055)"), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Over_HotTemp-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Total number of days/year under cold temperature threshold
var<-"UnderColdTemp"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr <", ColdTemp, "Deg \n in Historical (1950-1999) & Future (2025-2055)"), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Under_ColdTemp.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

DaysUnderCold <- At

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr <", ColdTemp, "Deg \n in Historical (1950-1999) & Future (2025-2055)"), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Under_ColdTemp-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year under 5th low temperature threshold
var<-"UnderLowQ"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Tmin < Historic 5th Percentile (", round(HistTminLow, 1), "˚F) in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Under_5th.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Tmin < Historic 5th Percentile (", round(HistTminLow, 1), "˚F) in ", Year, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Under_5th-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

####################
#Total number of days/year over 95th low temperature threshold
var<-"OverHighQ"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Tmax > Historic 95th Percentile (", round(HistTmaxHigh, 1), "˚F) in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Over_95th.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

OverHighQ <- At

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Tmax > Historic 95th Percentile (", round(HistTmaxHigh, 1), "˚F) in ", Year, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Over_95th-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year over 95th precip threshold
var<-"OverPrecip95"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > Historic 95th Percentile (", round(HistPrecip95, 1), "in) in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Over_95th_Precip.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

Over95Pr <- At # summary for presentation

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > Historic 95th Percentile (", round(HistPrecip95, 1), "in) in ", Year, sep=""), 
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Over_95th_Precip-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 1" threshold
var<-"PrecipOver1"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 1 in. in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Over_1_Precip.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 1 in. in ", Year, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Over_1_Precip-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 2" threshold
var<-"PrecipOver2"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 2 in. in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Days_Over_2_Precip.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Days/Yr with Precip > 2 in. in ", Year, sep=""),
       y = "Days/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Days_Over_2_Precip-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)


####################
#Total number of days/year Precip over 2" threshold
var<-"FThaw"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Annual freeze-thaw cycles in ", Year, sep=""), 
       y = "Cycles/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

ggsave(sprintf("%s_%s_%s_Freeze-thaw.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

# Boxplot
p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
  geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
  BarPlotTheme +
  labs(title = paste(SiteID, " - Annual freeze-thaw cycles in ", Year, sep=""),
       y = "Cycles/Yr") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors3)
dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)
ggsave(sprintf("%s_%s_%s_Freeze-thaw-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)

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


# ####################
# ####### COMMENTED OUT BECAUSE YEILDS WEIRD RESULTS IN COASTAL PARKS. MAKE SURE OUTPUT PLAUSIBLE IF USING
# #Total number of days/year in growing season (ClimDex defn)
# var<-"GrowLen"
# At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
# names(At)<-c("CF",var)
# ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
#   geom_bar(stat="identity",position="dodge",colour="black") +
#   BarPlotTheme +
#   # coord_cartesian(ylim=c(0, 40)) +
#   labs(title = paste(SiteID, " - Average annual growing season length in ", Year, sep=""), 
#        y = "Days/Yr", colour = "Climate Future")  +
#   scale_fill_manual(name="",values = colors3) +
#   coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))
# 
# ggsave(sprintf("%s_%s_%s_Growing-season.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
# 
# # Boxplot
# p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
#   geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
#   geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
#   BarPlotTheme +
#   labs(title = paste(SiteID, " - Average annual growing season length in ", Year, sep=""),
#        y = "Days/Yr") +
#   scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
#   scale_fill_manual(name="",values = colors3)
# dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
# p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
#                                 y=middle, yend=middle), colour="grey", size=1)
# ggsave(sprintf("%s_%s_%s_Growing-season-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
# 

# ####################
# #Beginning of growing season - green-up (Julian Day)
# var<-"BegGrow"
# At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
# names(At)<-c("CF",var)
# ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
#   geom_bar(stat="identity",position="dodge",colour="black") +
#   BarPlotTheme +
#   # coord_cartesian(ylim=c(0, 40)) +
#   labs(title = paste(SiteID, " - Average annual green-up date (Julian day) in ", Year, sep=""), 
#        y = "Julian day", colour = "Climate Future")  +
#   scale_fill_manual(name="",values = colors3) +
#   coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))
# 
# ggsave(sprintf("%s_%s_%s_Green-up.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
# 
# # Boxplot
# p<-ggplot(Annual_samp, aes(x=CF, y=(eval(parse(text=var))), colour=CF)) + 
#   geom_boxplot(colour="black",aes(fill = factor(CF)), outlier.shape=NA)+ 
#   geom_jitter(shape = 21, size = 5, aes(fill = factor(CF),colour=factor(me.col)), position=position_jitter(0.2)) +
#   BarPlotTheme +
#   labs(title = paste(SiteID, " - Average annual green-up date (Julian day) in ", Year, sep=""), 
#        y = "Julian day") +
#   scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
#   scale_fill_manual(name="",values = colors3)
# dat<-ggplot_build(p)$data[[1]];dat1<-dat[3,]
# p + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
#                                 y=middle, yend=middle), colour="grey", size=1)
# ggsave(sprintf("%s_%s_%s_Green-up-Boxplot.png", SiteID, Lat, Lon), path = './figures/MACA', width = PlotWidth, height = PlotHeight)
# 

####################

# Total number of days/year with spring frost (GDD==T, Tmin<32)
var<-"Sp.Frost"
At<-aggregate(eval(parse(text=var))~CF,Annual,mean);
names(At)<-c("CF",var)
ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
   #coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, " - Average annual spring frost days in ", Year, sep=""), 
       y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3) +
  coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))

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
var<-"TavgCustom"
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
var<-"PrecipCustom"
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
A<-aggregate(.~CF,Annual[,2:24],mean) 
write.xlsx(list("Means"=A,"Annual"=Annual,"Season"=Season,"D_Season"=Season_delta,"Monthly"=Monthly,"Monthly_delta"=Monthly_delta), 
           file=("./data/park-specific/output/Plot_data.xlsx"),col.names=TRUE)

