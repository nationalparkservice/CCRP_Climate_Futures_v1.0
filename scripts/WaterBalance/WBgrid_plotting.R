WBData <- subset(WBdat, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
WBData <- subset(WBData, Year >= Yr-Range/2 & Year <= Yr+Range/2 | Year <= 2012)

MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

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


write.csv(MonthlyWB,"./data/park-specific/output/MonthlyWB.csv",row.names=F)
write.csv(AnnualWB,"./data/park-specific/output/AnnualWB.csv",row.names=F)


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

ggsave(paste("Water Balance-",SiteID,".png",sep=""), path = "./figures/MACA", width = 15, height = 9)

ggplot(AnnualWB, aes(x=sum_d.in, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=col) +
  scale_fill_manual(values=col) +  
  scale_linetype_manual(values=seq(1,length(unique(AnnualWB$CF)),1)) +
  labs(y = "Density",
       x = "Annual moisture deficit (in)",
       title = paste(SiteID,"Water Deficit for GCMs in", Yr,  "and Historical Period (1979-2012)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=20), legend.background=element_rect(fill = "White", size = 0.5),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(SiteID,"-Deficit_density_panel.png",sep=""), path = "./figures/MACA", width = 15, height = 9)

ggplot(AnnualWB, aes(x=avg_SM.in, colour=CF,fill=CF,linetype=CF),show.legend=F) +geom_density(alpha=0.3,size=1.5) +
  scale_colour_manual(values=col) +
  scale_fill_manual(values=col) +  
  scale_linetype_manual(values=seq(1,length(unique(AnnualWB$CF)),1)) +
  labs(y = "Density",
       x = "Annual soil moisture (in)",
       title = paste(SiteID,"Soil Moisture for GCMs in", Yr , "and Historical Period (1979-2012)",sep=" ")) +
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0),legend.position = c(.8,.8)) 

ggsave(paste(SiteID,"-SOIL_in_density_panel.png",sep=""), path = "./figures/MACA", width = 15, height = 9)



### Monthly
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



ggplot(MonthlyWB_delta, aes(x=Month, y=avg_SM.in, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = paste("Change in average monthly soil moisture in", Yr, "vs Historical (1979-2012)"),
       x = "Month", y = "Change in soil moisture (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("MonthlySoil Moisture.png", path = "./figures/MACA", width = 15, height = 9)

ggplot(MonthlyWB_delta, aes(x=Month, y=sum_d.in, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = paste("Change in average monthly water deficit in", Yr, "vs Historical (1979-2012)"),
       x = "Month", y = "Change in deficit (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly Deficit.png", path = "./figures/MACA", width = 15, height = 9)


ggplot(MonthlyWB_delta, aes(x=Month, y=sum_runoff.in, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = paste("Change in average monthly runoff in", Yr, "vs Historical (1979-2012)"),
       x = "Month", y = "Change in runoff (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly Runoff.png", path = "./figures/MACA", width = 15, height = 9)


ggplot(MonthlyWB_delta, aes(x=Month, y=sum_SWEaccum.in, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = paste("Change in average monthly SWE in", Yr, "vs Historical (1979-2012)"),
       x = "Month", y = "Change in SWE (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly SWE.png", path = "./figures/MACA", width = 15, height = 9)

ggplot(MonthlyWB_delta, aes(x=Month, y=sum_aet.in, group=CF, colour = CF)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  scale_x_discrete(labels=month.abb) +
  labs(title = paste("Change in average monthly AET in", Yr, "vs Historical (1979-2012)"),
       x = "Month", y = "Change in AET (inches)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

ggsave("Monthly AET.png", path = "./figures/MACA", width = 15, height = 9)
