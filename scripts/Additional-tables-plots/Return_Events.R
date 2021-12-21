##### Testing return period / exceedance probability calculations
OutDir<-("./figures/additional")

#Height and width 
PlotWidth = 18
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
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

####################################################################################
############### Analysis on MACA data###########################

### CF1 future 
model1<-WB_GCMs$GCM[1]
Future_all$Year=NULL
Future_model1<-subset(Future_all,GCM %in% model1 & yr >= (Year-Range/2) & yr<= (Year+Range/2))

##CF1 baseline
Baseline_model1<-subset(Baseline_all, GCM %in% model1 & Year<1999)

# Annual max and plot for cF1
Base1_max<-aggregate(PrecipCustom~Year+GCM,Baseline_model1,max)
Future1_max<-aggregate(PrecipCustom~yr+GCM,Future_model1,max)

ggplot(Base1_max,aes(x=Year,y=PrecipCustom,colour=GCM)) + geom_point(aes(colour=GCM)) 
ggplot(Future1_max,aes(x=yr,y=PrecipCustom,colour=GCM)) + geom_point(aes(colour=GCM)) 

Base1_GCM<-split(Base1_max,Base1_max$GCM) #Splits df into array by GCM
Future1_GCM<-split(Future1_max,Future1_max$GCM)

for (i in 1:length(model1)){
  name=names(Base1_GCM)[i]
  b1<-Base1_GCM[[i]]; f1<-Future1_GCM[[i]]
  b1<-b1[order(-b1$PrecipCustom),]; f1<-f1[order(-f1$PrecipCustom),]
  b1$rank<-seq(1:nrow(b1)); f1$rank<-seq(1:nrow(f1))
  b1$return<- (nrow(b1)+1)/(b1$rank)
  f1$return<- (nrow(f1)+1)/(f1$rank)
  b1$EP <- 1/b1$return; f1$EP <- 1/f1$return
  Base1_GCM[[i]]<-b1
  Future1_GCM[[i]]<-f1
}
Base1<-ldply(Base1_GCM,data.frame)
Future1<-ldply(Future1_GCM,data.frame)

##CF1 baseline 20 year return
regression<-lm(PrecipCustom~log(return),data=Base1)
Base1$modeled<-predict(regression)

max100base1<-data.frame(return=seq(1,100,1))
max100base1$modeled<-predict(regression,newdata=max100base1)
max100base1$CF<-CFs[1]

return20base1<-data.frame(return=20)
return20base1$mod<-predict(regression,newdata=return20base1)

##CF1 future 20 year return
regression<-lm(PrecipCustom~log(return),data=Future1)
Future1$modeled<-predict(regression)

max100future1<-data.frame(return=seq(1,100,1))
max100future1$modeled<-predict(regression,newdata=max100future1)
max100future1$CF<-CFs[1]

return20future1<-data.frame(return=20)
return20future1$mod<-predict(regression,newdata=return20future1)
return20future1$CF<-CFs[1]

##################################################################
######## CF2 future and baseline
model2<-WB_GCMs$GCM[2]
Future_model2<-subset(Future_all,GCM %in% model2 & yr >= (Year-Range/2) & yr<= (Year+Range/2))

Baseline_model2<-subset(Baseline_all, GCM %in% model2 & Year<1999)

# Annual max and plot for cF2
Base2_max<-aggregate(PrecipCustom~Year+GCM,Baseline_model2,max)
Future2_max<-aggregate(PrecipCustom~yr+GCM,Future_model2,max)

Base2_GCM<-split(Base2_max,Base2_max$GCM) #Splits df into array by GCM
Future2_GCM<-split(Future2_max,Future2_max$GCM)

for (i in 1:length(model1)){
  name=names(Base2_GCM)[i]
  b2<-Base2_GCM[[i]]; f2<-Future2_GCM[[i]]
  b2<-b2[order(-b2$PrecipCustom),]; f2<-f2[order(-f2$PrecipCustom),]
  b2$rank<-seq(1:nrow(b2)); f2$rank<-seq(1:nrow(f2))
  b2$return<- (nrow(b2)+1)/(b2$rank)
  f2$return<- (nrow(f2)+1)/(f2$rank)
  b2$EP <- 1/b2$return; f2$EP <- 1/f2$return
  Base2_GCM[[i]]<-b2
  Future2_GCM[[i]]<-f2
}
Base2<-ldply(Base2_GCM,data.frame)
Future2<-ldply(Future2_GCM,data.frame)

##CF2 baseline 20 year return
regression<-lm(PrecipCustom~log(return),data=Base2)
Base2$modeled<-predict(regression)

max100base2<-data.frame(return=seq(1,100,1))
max100base2$modeled<-predict(regression,newdata=max100base2)
max100base2$CF<-CFs[2]

return20base2<-data.frame(return=20)
return20base2$mod<-predict(regression,newdata=return20base2)

##CF2 future 20 year return
regression<-lm(PrecipCustom~log(return),data=Future2)
Future2$modeled<-predict(regression)

max100future2<-data.frame(return=seq(1,100,1))
max100future2$modeled<-predict(regression,newdata=max100future2)
max100future2$CF<-CFs[2]

return20future2<-data.frame(return=20)
return20future2$mod<-predict(regression,newdata=return20future2)
return20future2$CF<-CFs[2]

#############################################################

#average of two baseline return 20 values
return20baseavg<-data.frame(return=20)
return20baseavg$mod<-((return20base1$mod + return20base2$mod)/2)
return20baseavg$CF<-"Historical"
                    

#average two baseline regression values from CF1 and 2
max100baseavg<-data.frame(return=seq(1,100,1))
max100baseavg$modeled<-((max100base1$modeled+max100base2$modeled)/2)
max100baseavg$CF<-"Historical"

######################################################

####bar plot of returns

#bind the return intv data together
allreturns<-rbind(return20future1, return20future2, return20baseavg)

#add levels for formatting
allreturns$CF<-factor(allreturns$CF, levels=c("Historical",CFs[1], CFs[2]))

#Bar graph 20-year return int for a 24-hour event
ggplot(allreturns, aes(x=CF, y=mod,fill=CF)) +
  geom_bar(stat="identity",position="dodge",colour="black") +
  BarPlotTheme +
  labs(title = paste(SiteID, " - 20-year recurrence interval",sep=""), 
       x ="20-year recurrence interval", y ="Precipitation (inches/day)") +
  scale_fill_manual(name="",values = colors3) 

ggsave("20yr-bar-plot.png", path=OutDir, width = PlotWidth, height = PlotHeight)


######line plot of return int regressions

#bind the regressions lines into one df
allregressions<-rbind(max100baseavg, max100future1, max100future2)

#add levels for formatting
allregressions$CF<-factor(allregressions$CF, levels=c("Historical",CFs[1], CFs[2]))

#line plots of regressions
ggplot(allregressions, aes(x=return, y=modeled, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity",colour="black") + 
  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme +
  labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))

ggsave("20yr-regressions.png", path=OutDir, width = PlotWidth, height = PlotHeight)


