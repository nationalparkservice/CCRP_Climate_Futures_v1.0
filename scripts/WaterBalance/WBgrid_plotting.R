WBData <- subset(WBdat, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
WBData <- subset(WBData, Year >= Yr-Range/2 & Year <= Yr+Range/2 | Year <= 2012)

######################################################### AGGREGATE OUTPUTS TO MONTLY/ANNUAL ################################################################

WBData$yrmon = strftime(WBData$Date, "%Y%m")

#Monthly
MonthlyWB = aggregate(Rain.in~yrmon+GCM,data=aggregate(Rain.in~yrmon+GCM,data=WBData,sum),mean)
colnames(MonthlyWB)[3]<-"sum_rain.in"

MonthlyWB$sum_SWEaccum.in = aggregate(SWEaccum.in~yrmon+GCM,data=aggregate(SWEaccum.in~yrmon+GCM,data=WBData,sum),mean)[,3]
MonthlyWB$sum_runoff.in = aggregate(Runoff.in~yrmon+GCM,data=aggregate(Runoff.in~yrmon+GCM,data=WBData,sum),mean)[,3]
MonthlyWB$sum_pet.in = aggregate(PET.in~yrmon+GCM,data=aggregate(PET.in~yrmon+GCM,data=WBData,sum),mean)[,3]
MonthlyWB$avg_SM.in = aggregate(SM.in ~ yrmon+GCM, data=WBData, FUN=mean)[,3]
MonthlyWB$sum_aet.in = aggregate(AET.in~yrmon+GCM,data=aggregate(AET.in~yrmon+GCM,data=WBData,sum),mean)[,3]
MonthlyWB$sum_d.in = aggregate(D.in~yrmon+GCM,data=aggregate(D.in~yrmon+GCM,data=WBData,sum),mean)[,3]

#Annual
AnnualWB = aggregate(Rain.in ~ Year+GCM, data=aggregate(Rain.in~Year+GCM,data=WBData,sum), mean)
colnames(AnnualWB)[3]<-"sum_rain.in"

AnnualWB$sum_SWEaccum.in = aggregate(SWEaccum.in ~ Year+GCM, data=aggregate(SWEaccum.in~Year+GCM,data=WBData,sum), mean)[,3]
AnnualWB$sum_runoff.in = aggregate(Runoff.in ~ Year+GCM, data=aggregate(Runoff.in~Year+GCM,data=WBData,sum), mean)[,3]
AnnualWB$sum_pet.in = aggregate(PET.in ~ Year+GCM, data=aggregate(PET.in~Year+GCM,data=WBData,sum), mean)[,3]
AnnualWB$avg_SM.in = aggregate(SM.in ~ Year+GCM, data=WBData, FUN=mean)[,3]
AnnualWB$sum_aet.in = aggregate(AET.in ~ Year+GCM, data=aggregate(AET.in~Year+GCM,data=WBData,sum), mean)[,3]
AnnualWB$sum_d.in = aggregate(D.in ~ Year+GCM, data=aggregate(D.in~Year+GCM,data=WBData,sum), mean)[,3]


write.csv(MonthlyWB,paste0(TableDir,"MonthlyWB.csv"),row.names=FALSE)
write.csv(AnnualWB,paste0(TableDir, "AnnualWB.csv"),row.names=FALSE)


#######################################################################################################################
######################################### PLOTTING ####################################################################
# Inputs
MonthlyWB<-merge(MonthlyWB,WB_GCMs,by="GCM", all.x=T)
MonthlyWB$CF<-factor(MonthlyWB$CF, levels=c("Historical",CFs))
MonthlyWB$CF[is.na(MonthlyWB$CF)] <- "Historical"
AnnualWB<-merge(AnnualWB,WB_GCMs,by="GCM", all.x=T)
AnnualWB$CF<-factor(AnnualWB$CF, levels=c("Historical",CFs))
AnnualWB$CF[is.na(AnnualWB$CF)] <- "Historical"


ggplot(AnnualWB, aes(x=sum_d.in, y=sum_aet.in, colour=CF)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+
  scale_colour_manual("Scenario",values=col) +
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual moisture deficit (in)",
    colour = "GCM",
    title = paste("Water Balance for ",SiteID,sep="")  
  ) + theme(plot.title = element_text(hjust = 0.5)) + #+ geom_vline(xintercept=mean(Historical.wb$deficit), colour="black") +geom_vline(xintercept=mean(Future.wb$deficit), colour="blue")
  # size is pts
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22)) #+xlim(20,45)+ylim(2,16)

ggsave("WaterBalance.png", path = FigDir, width = PlotWidth, height = PlotHeight)

density_plot(AnnualWB, xvar=sum_d.in,cols=col,title=paste(SiteID,"Water Deficit for GCMs in", Yr,  "and Historical Period (", BasePeriod,")",sep=" "),
             xlab="Annual deficit (in)")
ggsave("Density-sum_d.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

density_plot(AnnualWB, xvar=avg_SM.in,cols=col,title=paste(SiteID,"Soil Moisture for GCMs in", Yr,  "and Historical Period (", BasePeriod,")",sep=" "),
             xlab="Annual soil moisture (in)")
ggsave("Density-avg_SM.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)


### Monthly Plots
MonthlyWB$Month <- substr(MonthlyWB$yrmon, 5, 7)
MonthlyWB_mean <- aggregate(.~CF+Month, MonthlyWB[,3:11],mean)
MonthlyWB_H <- subset(MonthlyWB_mean, CF == "Historical")
MonthlyWB_delta = list()
split<-split(MonthlyWB_mean,MonthlyWB_mean$CF)
for(i in 1:length(split)){
  MD <- split[[i]]
  MD[,3:9] <- MD[,3:9] - MonthlyWB_H[,3:9]
  MonthlyWB_delta[[i]] <- MD ; rm(MD)
}
MonthlyWB_delta<- ldply(MonthlyWB_delta, data.frame)
MonthlyWB_delta <- subset(MonthlyWB_delta, CF %in% CFs)
MonthlyWB_delta$CF<-droplevels(MonthlyWB_delta$CF)


## avg_SM.in
Month_line_plot(MonthlyWB_delta, Month, avg_SM.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly soil moisture in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in soil moisture (inches)")
ggsave("Monthly-line-avg_SM.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, avg_SM.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly soil moisture in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in soil moisture (inches)",labels=MonthLabels)
ggsave("Monthly-dot-avg_SM.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

## sum_d.in
Month_line_plot(MonthlyWB_delta, Month, sum_d.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly water deficit in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in deficit (inches)")
ggsave("Monthly-line-sum_d.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_d.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly water deficit in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in deficit (inches)",labels=MonthLabels)
ggsave("Monthly-dot-sum_d.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## sum_runoff.in
Month_line_plot(MonthlyWB_delta, Month, sum_runoff.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly runoff in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in runoff (inches)")
ggsave("Monthly-line-sum_runoff.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_runoff.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly runoff in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in runoff (inches)",labels=MonthLabels)
ggsave("Monthly-dot-sum_runoff.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## sum_SWEaccum.in
Month_line_plot(MonthlyWB_delta, Month, sum_SWEaccum.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly SWE in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in SWE (inches)")
ggsave("Monthly-line-sum_SWEaccum.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_SWEaccum.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly SWE in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in SWE (inches)",labels=MonthLabels)
ggsave("Monthly-dot-sum_SWEaccum.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)


## sum_aet.in
Month_line_plot(MonthlyWB_delta, Month, sum_aet.in, grp=CF, cols=colors2, 
                title= paste("Change in average monthly AET in", Yr, "vs Historical (",BasePeriod,")"),
                xlab="Month", ylab="Change in AET (inches)")
ggsave("Monthly-line-sum_aet.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

dot_plot(MonthlyWB_delta, sum_aet.in, Month, grp=CF, cols=colors2,
         title = paste("Change in average monthly AET in", Yr, "vs Historical (",BasePeriod,")"),
         xlab="Change in AET (inches)",labels=MonthLabels)
ggsave("Monthly-dot-sum_aet.in.png", path = FigDir, width = PlotWidth, height = PlotHeight)

