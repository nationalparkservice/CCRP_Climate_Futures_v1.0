##################################
##### Tests ######################
##################################

##  PRISM_3 seas_ann_avgs_plots vxx.R
#   John Gross   
#   Inputs:  rData output from RSS parse script

#  v1.5 - Converted all plots to ggplot except for Avg Monthly Tmin Tmax Precip, updated plot titles and captions.
#  v1.4 - Revised monthly average Tmax/Tmin/Ppt plots. Uses separate axes to improve visibility of ppt bar plot, 
#         added std. dev. bars to ppt bar plot, and changed x-axis labels to month abbreviations. 
#  v1.3 - Includes PRISM data up to 2016, edited code to easily change data end year (May 2017)
#  v1.1 - minor fixes to red-blue plot, 10-yr running mean
#  need to deal with outfile directories - use info from RData file and get rid of code here
#  v1.0. 30 Oct 2015 - No known errors. Runs top to bottom.
#   conversion to in and deg F; to read rdata file as input, modified some plots, directories

#################################################

doP1 <- "YES"  # Should a separate regression be calculated for the reference period (default 1900-1970)? 
doP2 <- "YES"  # Should a separate regression be calculate for the period after the reference period (default 1971-present)? 
beginRefYr = 1900
endRefYr = 1970

BeginYr	= 1895   # is this data file or for plots?
EndYr = 2018
dataEndYr = 2017   # needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal

dpi = 600    


##ggplot theme for all plots
#Theme for all plots
PlotTheme = theme_gray() %+replace% 
  theme(plot.title = element_text(size=18, face='bold', hjust=0.5, vjust=0.5),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.title.y = element_text(size = 18, angle = 90, margin=margin(0,5,0,0)),
        axis.text.x = element_text(size = 16, colour="black"),
        axis.title.x = element_text(size = 18, margin=margin(5,0,0,0)),
        legend.position = "none",
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

theme_set(PlotTheme)
TitleSize = theme_get()$plot.title$size  ##Needed for cowplot layouts

#################   End of Initials  ##########################  

DoYrMon <- function(YrMon){    #  YrMon = char vector of year mon as 189501.  Return vector of decimal year like 1895.42
  year <- as.numeric(substr(YrMon, 1,4))
  mon <- as.numeric(substr(YrMon, 5,6))
  yearMon <- (mon-1) * 0.083 + 0.042
  yearMon <- year+yearMon
  YRMON <- cbind(year, mon, yearMon)
  return(YRMON)
}

dte = Sys.Date()
# clean up trashy namespace

yrMons<- data.frame(DoYrMon(PptMeans$YearMon))
baseData <- cbind(yrMons, seas=PptMeans$Season, tmin=TminMeans$TminF, tmax=TmaxMeans$TmaxF, 
                  tmean=(TminMeans$TminF+TmaxMeans$TmaxF)/2, ppt=PptMeans$PptIn)
names(baseData)[1:3] <- c("yr", "mon","yrmon")

refData<-baseData[baseData$yr >= beginRefYr & baseData$yr <= endRefYr,]

# maybe should use PRISM year avgs instead i.e. month 14
pptAvg = with(baseData, tapply(ppt, yr, mean))  * 12  # xx/mo ->: xx/yr
tminAvg = with(baseData, tapply(tmin, yr, mean))
tmaxAvg = with(baseData, tapply(tmax, yr, mean))
tmeanAvg = with(baseData, tapply(tmean, yr, mean))

cYr <- BeginYr:EndYr
yrAvgs <- data.frame(cYr, pptAvg, tminAvg, tmaxAvg, tmeanAvg)
yrAvgs$tAvg <- (yrAvgs$tminAvg+yrAvgs$tmaxAvg)/2


## interesting to compare PRISM vs calcuated Tmean

pptRef <- data.frame(yrAvgs[yrAvgs$cYr >= beginRefYr & yrAvgs$cYr <= endRefYr, 2])
names(pptRef) <- "ppt"


######################  Periods of Analysis  ######################

p1_start  = beginRefYr
p1_end    = endRefYr
p2_start  = endRefYr
p2_end    = EndYr

yrAvgs$tmaxP1 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tminP1 <- yrAvgs$tminAvg
yrAvgs$tminP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmeanP1 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$pptP1 <- yrAvgs$pptAvg
yrAvgs$pptP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmaxP2 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tminP2 <- yrAvgs$tminAvg
yrAvgs$tminP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tmeanP2 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$pptP2 <- yrAvgs$pptAvg
yrAvgs$pptP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

########################################
# Data check - plot min and max by month
tmaxMon <- tapply(baseData$tmax, baseData$mon, mean)
tminMon <- tapply(baseData$tmin, baseData$mon, mean)
tmeanMon <- tapply(baseData$tmean, baseData$mon, mean)
pptMon  <- tapply(baseData$ppt, baseData$mon, mean)
pptMonSD <- tapply(baseData$ppt, baseData$mon, sd)

monAvg <- data.frame(cbind(tmaxMon, tminMon, pptMon, pptMonSD))
monAvg$mon <- seq(1:12)
monAvg$monNames <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
tmaxq <- tapply(baseData$tmax, baseData$mon, quantile)
tminq <- tapply(baseData$tmin, baseData$mon, quantile)
pptq <- tapply(baseData$ppt, baseData$mon, quantile)

for(i in 1:12){
  q <- tmaxq[[i]]
  monAvg$tmax25[i] <- q[2]  # 2 and 4 are 25th adn 75th quantile
  monAvg$tmax75[i] <- q[4]
  
  q <- tminq[[i]]
  monAvg$tmin25[i] <- q[2]
  monAvg$tmin75[i] <- q[4]
  
  q <- pptq[[i]]
  monAvg$ppt25[i] <- q[2]
  monAvg$ppt75[i] <- q[4]
}

PlotName <- "Avg Monthly Tmin Tmax Ppt"
OFName <- paste(PlotName, " ", SiteID, " ", Lat, " ", Lon, sep = "")	

plot1 <- paste('./figures/PRISM/', OFName)

png(paste(plot1, ".png", sep = ""), width=6.5*dpi, height=4.5*dpi, res=dpi)

par(mfrow=c(1,1), mgp=c(0,.5,0), mar=c(4,3.75,2,3.75))
attach(monAvg)
Ppt = barplot(pptMon, names.arg=monNames,
              ylim=c(0, max(monAvg$ppt75)+.5),
              axes=FALSE,
              border=NA,
              col=rgb(.678, .847, .902, alpha=0.6))
segments(Ppt, ppt25, Ppt, ppt75, col="dark gray")
axis(side=4)
par(new = T)
plot(tmax75~mon, 
     type="l", col="red", lty=2, lwd=2,
     xlab=NA,
     ylab=NA,
     xaxt='n',
     xlim=c(.5, 12.5),
     ylim=c(0,110), 
     ps = 2,
     main=paste(SiteID, "- Monthly Climate Means", sep="") 
)
lines(tmax25~mon, col="red", lty=2, lwd=2)
lines(tmaxMon~mon, col="red", lwd=3)
lines(tmin75~mon, col="blue", lty=2, lwd=2) 
lines(tmin25~mon, col="blue", lty=2, lwd=2)
lines(tminMon~mon, col="blue", lwd=3)

axis(side=2)
mtext(side=1, line=1.75, "Month")
mtext(side=2, line=2, expression(paste(Temperature, ~({}^o*F))))
mtext(side=4, line=2, "Precip (in)")
mtext(side=1, line=2.75, paste("Dashed lines/error bars = 25th-75th percentile ranges. Data range = ", BeginYr, "-", EndYr, ".", sep=""), cex=0.75, adj=0.5)
legend("topleft", legend=c("Tmax", "Tmin"), col=c("red", "blue"), lwd=c(2,2), cex=0.75, bty="n")
legend(.4, 103, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
detach(monAvg)
dev.off()	



#-------------------------------------------------#
############  Running average plots   #############
#-------------------------------------------------#

rTmin <- rollmean(tminAvg, rollLen)
rTmax <- rollmean(tmaxAvg, rollLen)
rTmean <- rollmean(tmeanAvg, rollLen)
rPpt  <- rollmean(pptAvg, rollLen)

rYr = seq(dataEndYr - length(rTmin)+1, dataEndYr)

rDat <- data.frame(cbind(rYr, rTmin, rTmax, rTmean, rPpt))
names(rDat)[1] <- "cYr"
rDat$yr <- rDat$cYr
yrAvgs <- merge(rDat, yrAvgs, all=TRUE)

##ggplot
PlotName <- "10-yr Running Means"

#Colors for running means
RMColors = scale_color_manual(name="", values=c("brown", "black", "#3366FF"))

a <- ggplot(aes(x=cYr), data=yrAvgs) + 
  geom_line(aes(y=tmaxAvg, group=1, col="Annual means"), na.rm=TRUE) + 
  geom_point(aes(y=tmaxAvg, col="Annual means"), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y=29, label="A")) +
  geom_smooth(method="lm", aes(y=tmaxAvg, group=2, col="Regression trend"), na.rm=TRUE)+ 
  geom_line(aes(y=rTmax, group=3, col=paste(rollLen, "-yr running mean", sep="")), size=1.5, na.rm=TRUE) +
  RMColors +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

b <- ggplot(aes(cYr, tminAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmin), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

c <- ggplot(aes(cYr, tmeanAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmean), size=1.5, colour="brown", na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

d <- ggplot(aes(cYr, pptAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rPpt), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Annual Means and Trends", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights=c(0.05, 1, .05))
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nData range = ", BeginYr, "-", EndYr, sep=""), 
             y=0.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

plot1 <- p3
print(p3)

#OFName = paste("./figures/PRISM/", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
#ggsave(OFName, width=6.5, height=8.5, dpi=dpi)