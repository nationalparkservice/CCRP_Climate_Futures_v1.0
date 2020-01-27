#  FLI FBI phenology plots v0x.R  
# 2018_04_05

#  plot data from Monahan et al. early flowering in parks Ecosphere 
#  plot all parks, and highlight a specific park

# variable names changed to:  Park.Name Pcode FLI_day FBI_day FLI_pct FBI_pct FLI_max_delta
# FBI_max_delta FLI_rate FBI_rate"    

rm(list=ls())
DataDirName <- "C:/Users/achildress/Documents/RSS/"

pCode <- "PETE"
# print plots to screen or to file in working directory?
outDevice <- "file"
# outDevice <- "screen"    # use ONLY "screen" or "file"

setwd(DataDirName)
inFile <- paste(DataDirName, "FLI FBI table.csv", sep="")

dec2century <-  10   # days/decade to days/century

pData <- read.csv(inFile , header=T)
pNum <- 1:length(pData$FLI_rate)

resultInterp <- function(pctObs){    #  text associated with percentile of results
  resultText <- paste("Late (", pctObs,"%)", sep="")
  if(pctObs < 75)resultText <- paste("Average (", pctObs,"%)", sep="")
  if(pctObs < 25)resultText <-  paste("Early (", pctObs,"%)", sep="")
  if(pctObs < 5)resultText <-  paste("Extremely early (", pctObs,"%)", sep="")
  return(resultText)
}

# first leaf
lData <- pData[order(pData$FLI_rate),]
FLI <- lData$FLI_rate * dec2century
yFLI <- which(lData$Pcode==pCode)        # value of y axis of park

if(outDevice == "file")png(filename= paste(pCode, "FLI.png"))

plot(FLI, pNum, col = "light green", ylab = "", xlab="Days/century", main="First Leaf", bty="n", yaxt = "n", xlim = c(-25, 5), cex.axis=1.5, cex.lab=1.5)
points(FLI[yFLI],yFLI, col="dark green", pch=19, cex = 1.5)
text(FLI[yFLI]-5,yFLI+5, pCode, cex=1.5, adj=c(0,0))
abline(v=0, lty=3)

text(-8, 30, resultInterp(lData$FLI_pct[which(lData$Pcode == pCode)]), cex=1.1, pos = 4)
if(outDevice == "file")dev.off()
cat(yFLI, FLI[yFLI])

# first bloom   
bData <- pData[order(pData$FBI_rate),]
FBI <- bData$FBI_rate * dec2century
yFBI <- which(bData$Pcode==pCode)

if(outDevice == "file") png(filename= paste(pCode, "FBI.png"))

plot(FBI, pNum, col = "pink", ylab = "", xlab="Days/century", main="First Bloom", bty="n", yaxt = "n", xlim = c(-25, 5), cex.axis=1.5, cex.lab=1.5)
points(FBI[yFBI],yFBI, col="purple", pch=19, cex = 1.5)
text(FBI[yFBI]-5, yFBI+5, pCode, cex=1.5, adj=c(0.0))
text(-8, 30, resultInterp(bData$FBI_pct[which(bData$Pcode == pCode)]), cex=1.1, pos=4)
abline(v=0, lty=3)

if(outDevice == "file")dev.off()
cat(yFBI, FBI[yFBI])
pData[which(pData$Pcode == pCode),]
