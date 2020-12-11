# RSS_Plotting vxx.R

# v01.4 Fixed missing line plots for Avg_Monthly_Tmin_Delta and Avg_Monthly_Tmax_Delta by removing fill parameter on geom_line. Line color is already specified by aes(colour=CF)
# v01.3 Added GCM scatter w/ GCMs labelled. Revised plot titles to read Year.
# v01.2 Added a scatter w/o box, darker/larger axis labels
# v01.1 STABLE 22 Oct 2016 - added plots for below cold. Ref lines fixed. Scales OK, SiteID to plot titles & file names
# v01 18 Oct 2015 - stable, writes to /figs.  Vertical plot scale issues not all sorted out.
#                 - in 4-panel drought plot, all ref lines show on all plots (needs fixing)

library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ncdf4)
library(reshape2)
library(WriteXLS)


rm(list=ls())

setwd("C:/Users/adillon/Documents/RSS/CONG/MACA/Figs MACA/")
load("CONG_33.791868_-80.748665_Final_Environment.RData")

######### INITIALS #########

##Color schemes

#Colors for CF values plotted side by side (match order of CFs vector)
colors5 <-  c("#9A9EE5","#12045C","white","#F3D3CB","#E10720")

#Colors for RCP 4.5, RCP 8.5
col.RCP2 = c("blue", "red")

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
                  legend.position = "bottom")                                                                            #Legend position

#X-axis labels for monthly plots
MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
                 
######## END INITIALS ########

##### Scatterplots for presentation ######
## Scatterplots ##
head(Future_Means)
Longx<- "annual average temperature (F)"
Longy<- "annual average precipitation (in)"
x <- "DeltaTavg"
y <- "DeltaPr"

Future_Means$PrecipCustom<-Future_Means$PrecipCustom
Future_Means$DeltaPr<-Future_Means$DeltaPr

# No color
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in 2040 by GCM run",sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave(paste(SiteID,"-Scatter-",x,"--",y,".png",sep=""), width = 15, height = 9)

####### Scatterplot with CF color
FM<-Future_Means
# '%ni%' <- Negate('%in%')
FM$CFnew<-as.character(FM$CF)
# FM$CFnew[which(FM$CFnew %ni% FutureSubset)]<-"Not Selected"
FM$CFnew[which(FM$CFnew=="Central")]<-"Not Selected"
FM$CFnew<-factor(FM$CFnew,levels=c("Warm Wet","Hot Wet","Not Selected","Warm Dry","Hot Dry"))
levels(FM$CFnew)

ggplot(FM, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365)) +
  geom_text_repel(aes(label=GCM,color=CFnew),position=position_jitter(0,.2)) + 
  geom_point(size=5,colour="black")+
  geom_point(aes(color=CFnew),size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  ###
  labs(title =" Changes in climate means centered on 2065 (2050-2080)\n relative to historical period (1950-2000) by GCM run", 
       x = paste("Change in ",Longx,sep=""), # Change
       y = paste("Change in ",Longy,sep="")) + #change
  scale_color_manual(name="Climate Futures", values=colors5) +
  scale_fill_manual(name="Climate Futures",values = colors5) + 
  guides(color=guide_legend(title="Climate Futures\n",override.aes = list(size=7))) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(FM$DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(FM$DeltaTavg)),linetype=2)  #change

ggsave(paste(SiteID, "Scatter BY SCENARIO-",x,"--",y,".png",sep=""), width = 15, height = 9)

#~~~~~~~~~~~~~~
# Presetation only scatterplots
#~~~~~~~~~~~~~~
# Points only w/out
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
dualscatter  + geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=24,vjust=-0.5),
        axis.title.y=element_text(size=24,vjust=0.5),
        plot.title=element_text(size=26,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=22), legend.title=element_text(size=24)) + 
   ###
  labs(title =paste(SiteID, " Changes in climate means in 2040 by GCM run\n", Longx," vs. ",Longy,sep=""), 
            x = paste("Changes in ",Longx,sep=""), # Change
            y = paste("Changes in ",Longy,sep="")) + #change
  theme(legend.position="none") +
  xlim(0, max(Future_Means$DeltaTavg))
ggsave(paste(SiteID,"-Scatter-POINTS ONLY",x,"--",y,".png",sep=""), width = 15, height = 9)  

# Points only w/ box
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
dualscatter  + geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=24,vjust=-0.5),
        axis.title.y=element_text(size=24,vjust=0.5),
        plot.title=element_text(size=26,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=22), legend.title=element_text(size=24)) + 
    ###
  labs(title =paste(SiteID, " Changes in climate means in 2040 by GCM run\n", Longx," vs. ",Longy,sep=""), 
            x = paste("Changes in ",Longx,sep=""), # Change
            y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenariow",values = c("black")) +
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2)  #change
ggsave(paste(SiteID,"-Scatter-POINTS&BOX",x,"--",y,".png",sep=""), width = 15, height = 9)



#######################
##########
#####
###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for central CF
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)") +
  scale_colour_manual(values=c("blue", "red"))+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "blue", alpha=0) + 
  geom_hline(aes(yintercept=365*mean(Future_Means$DeltaPr)),linetype=2) + 
  geom_vline(aes(xintercept=mean(Future_Means$DeltaTavg)),linetype=2)  
#scale_y_continuous(limits=c(-3.75,3.75))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_Plot.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

###Scatter plot showing delta precip and tavg, color by emissions scenario, x-axis scaled 0-max
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)") +
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_point(aes(x=mean(DeltaTavg), y=mean(365*DeltaPr)), shape=23, size=10, fill='black', colour='black') +
  scale_x_continuous(limits=c(0, max(Future_Means$DeltaTavg)+.25))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_noBox_Plot.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

#  scatter plots with GCM name identifying points. For all, and separate 4.5 and 8.5 plots

png(filename = sprintf("%s_%s_%s_GCM_Scatter_8.5-4.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(Future_Means$DeltaTavg, 365*Future_Means$DeltaPr, pch=20, main=paste("RCP 4.5 & 8.5", Year), 
            xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg,data=Future_Means,subset = emissions == "RCP 4.5", col="blue",
      labels=Future_Means$GCM, pos=3, cex=1.5)
text(365*DeltaPr ~ DeltaTavg,data=Future_Means, subset = emissions == "RCP 8.5", col="red",
     labels=Future_Means$GCM, pos=3, cex=1.5)
dev.off()

png(filename = sprintf("%s_%s_%s_GCM_Scatter_4.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 4.5", pch=20, main= paste("RCP 4.5", Year), 
     xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 4.5", label=GCM, pos=3, cex=1.5, col="blue")
dev.off()

png(filename = sprintf("%s_%s_%s_GCM_Scatter_8.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 8.5",
     pch=20, main=paste("RCP 8.5", Year), xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 8.5", label=GCM, pos=3, cex=1.5, col="red")
dev.off()

#################################### DIAGNOSTIC CHANGE PLOTS ##############################################
#Bar graph of monthly precip/tmax/tmin by CF


ggplot(Monthly_delta, aes(x=Month,y=PrecipCustom,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in average monthly precipitation in", Year,"vs 1950-2005"), 
       x = "Month", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="Climate Future",values = colors5) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Precip_Delta_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

#Bar graph of seasonal precip by CF
ggplot(Season_delta, aes(x=season,y=PrecipCustom,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  PlotTheme +
  labs(ltitle = paste(SiteID, "- Change in average seasonal precipitation in", Year,"vs 1950-2005"), 
            x = "Season", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="Climate Future",values = colors5)

ggsave(sprintf("%s_%s_%s_Avg_Seasonal_Precip_Delta_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#Line plot of change in MaxTemp by CF/month
ggplot(Monthly_delta, aes(x=Month, y=TmaxCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in average monthly maximum temperature \nin", Year,"vs 1950-2005"), 
            x = "Month", y = "Deg F") +
  scale_color_manual(name="Climate Future",values = colors5) +
  scale_fill_manual(name="Climate Future",values = colors5) +
  scale_shape_manual(name="Climate Future",values = c(21,22,23,24,25)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TmaxCustom)))) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmax_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height =PlotHeight)


####Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_delta, aes(x=Month, y=TminCustom, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in average monthly minimum temperature \nin", Year,"vs 1950-2005"),
            x = "Month", y = "Deg F") +
  scale_color_manual(name="Climate Future",values = colors5) +
  scale_fill_manual(name="Climate Future",values = colors5) +
  scale_shape_manual(name="Climate Future",values = c(21,22,23,24,25)) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_delta$TminCustom))))+ 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmin_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


###PROGRAM COMPLETE###
