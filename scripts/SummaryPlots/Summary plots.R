
###################################################################################################################

Future_summary <- merge(ALL_FUTURE,CF_GCM,by="GCM")
Baseline_summary <- Gridmet; Baseline_summary$CF = "Historical"
all_summary <- rbind(Baseline_summary, Future_summary)
all_summary <- subset(all_summary, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
all_summary$CF <- factor(all_summary$CF, levels=c("Historical", CFs))

WB_summary <- subset(WBdat, GCM %in% WB_GCMs$GCM | GCM == "gridmet.historical")
WB_summary$CF <- factor(WB_summary$CF, levels=c("Historical", CFs))


########################################### Format MACA data #######################################################
yrAvgs <- aggregate(cbind(TavgF, PrcpIn)~Year+CF,all_summary,mean)
yrAvgs$PrcpIn <- yrAvgs$PrcpIn * 365
yrAvgs$TavgRoll10 <- rollmean(yrAvgs$TavgF, rollLen, fill = NA, align = "right")
yrAvgs$PrcpRoll10 <- rollmean(yrAvgs$Prcp, rollLen, fill = NA, align = "right")

WBAvgs <- aggregate(cbind(D.in, Runoff.in, SWEaccum.in)~Year+CF, WB_summary, sum)
WBAvgs$D.inRoll10 <- rollmean(WBAvgs$D.in, rollLen, fill = NA, align = "right")
WBAvgs$Runoff.inRoll10 <- rollmean(WBAvgs$Runoff.in, rollLen, fill = NA, align = "right")
WBAvgs$SWEaccum.inRoll10 <- rollmean(WBAvgs$SWEaccum.in, rollLen, fill = NA, align = "right")


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



# Tmean
ggplot(yrAvgs, aes(x=Year, y=TavgF, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Tavg.min, ymax=Tavg.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=TavgRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=TavgRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs(x="Year", y=expression("Mean annual temperature "~(degree~F))) +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme
ggsave(paste(SiteID,"-tmean.png",sep=""), path = './figures/MACA', height=PlotHeight, width=PlotWidth)

# Precip
ggplot(yrAvgs, aes(x=Year, y=PrcpIn, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=PrcpRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=PrcpRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual precipitation (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme
ggsave(paste(SiteID,"-precip.png",sep=""), path = './figures/MACA', height=PlotHeight, width=PlotWidth)

# Deficit
ggplot(WBAvgs, aes(x=Year, y=D.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=D.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=D.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual climatic water deficit (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme

ggsave(paste(SiteID,"-deficit.png",sep=""), path = './figures/MACA', height=PlotHeight, width=PlotWidth)

# Runoff
ggplot(WBAvgs, aes(x=Year, y=Runoff.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=Runoff.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=Runoff.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual Runoff (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme

ggsave(paste(SiteID,"-runoff.png",sep=""), path = './figures/MACA', height=PlotHeight, width=PlotWidth)

# SWEaccum
ggplot(WBAvgs, aes(x=Year, y=SWEaccum.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=SWEaccum.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=SWEaccum.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual accumulated SWE (in/year)") +
  scale_color_manual(name="Climate Future",values=col) +
  scale_fill_manual(name="Climate Future",values=col) + PlotTheme

ggsave(paste(SiteID,"-SWE.png",sep=""), path = './figures/MACA', height=PlotHeight, width=PlotWidth)




###### PANELS ##########
#Panel plot
t<-ggplot(yrAvgs, aes(x=Year, y=TavgF, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=80, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Tavg.min, ymax=Tavg.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=TavgRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=TavgRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
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


p<-ggplot(yrAvgs, aes(x=Year, y=PrcpIn, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=PrcpRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=PrcpRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
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

d<-ggplot(WBAvgs, aes(x=Year, y=D.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=D.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=D.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual climatic water deficit (in/year)") +
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
        panel.grid.major = element_line("light grey",0.3)) #add grid back\

r<-ggplot(WBAvgs, aes(x=Year, y=Runoff.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=Runoff.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=Runoff.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual runoff (in/year)") +
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

s<-ggplot(WBAvgs, aes(x=Year, y=SWEaccum.in, col=CF, fill=CF)) + 
  geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=0, ymax=100, alpha=0.1, fill="lightgray", col="lightgray") +
  # geom_ribbon(aes(x=as.numeric(as.character(year)), ymin=Precip.min, ymax=Precip.max, fill=CF), alpha=0.5) +
  geom_line(size=2) + geom_point(col="black", size=2, shape=16) +
  geom_point() +
  geom_line(aes(x=Year, y=SWEaccum.inRoll10),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=SWEaccum.inRoll10,colour = CF), size=.75 ,na.rm=TRUE) +
  scale_x_continuous(breaks=c(1980, 2000, 2020, 2040, 2060, 2080, 2100)) +
  labs( x="Year", y="Mean annual accumulated SWE (in/year)") +
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

#Temp and Precip
grid.arrange(t,p, nrow=2)
g <- arrangeGrob(t,p, nrow=2)
ggsave("Long-term_panel-Tavg_Prcp.png", g, path = './figures/MACA', width = 15, height = 15)

#Temp and Deficit
grid.arrange(t,d, nrow=2)
g <- arrangeGrob(t,d, nrow=2)
ggsave("Long-term_panel-Tavg_Deficit.png", g, path = './figures/MACA', width = 15, height = 15)

#Temp and Runoff
grid.arrange(t,r, nrow=2)
g <- arrangeGrob(t,r, nrow=2)
ggsave("Long-term_panel-Tavg_Runoff.png", g, path = './figures/MACA', width = 15, height = 15)

#Temp and SWE
grid.arrange(t,s, nrow=2)
g <- arrangeGrob(t,s, nrow=2)
ggsave("Long-term_panel-Tavg_SWE.png", g, path = './figures/MACA', width = 15, height = 15)

##################
