
library(micromapST)
library(randomForest)
setwd('D:\\bin\\STAT-515\\Final_Project\\Datasets')
indiaData<-read.csv('cancer-2.csv')
colnames(indiaData)
panelDesc <- data.frame(
  type=c('mapmedian','id','dot','dot'),
  lab1=c('','','Percentage of people who has passed','Per Capita'),
  lab2=c('' ,'','higher secondary education','Income'),
  col1 = c(NA,NA,3,4)
)


#library(micromapST)
#fName = "Indian_Micromap36.pdf"
#pdf(file=fName,width=7.5,height=10)
par(mar=c(1,1,1,1))
svg(width = 7, height = 1000)
p<-micromapST(indiaData, panelDesc,
           rowNamesCol=1,
           rowNames='full',
           bordGrp="IndStatesNBG",bordDir="D:\\bin\\STAT-515\\Final_Project",
           sortVar=3,ascend=FALSE,
           title=c("Literacy and Crime Rate",
                   "of Indian States"),
           ignoreNoMatches=TRUE)
dev.off()
set.seed(1)
indian.rf=randomForest(GDP.2013.14. ~ .
, data = indiaData , na.action=na.roughfix,importance=TRUE)

varImpPlot(indian.rf,main="Important variables for Yield 2016")

plot(indian.rf)

as.numeric(levels(indiaData$GDP.2013.14.))[indiaData$GDP.2013.14.]
as.numeric(as.character(indiaData$GDP.2013.14.))
colnames(indiaData)


linear=lm(GDP.2013.14. ~ GER.HigherEdTotal, data = indiaData)
plot(linear)


RegressionPlots(linear)
shpDC<-download.file("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Transportation_WebMercator/MapServer/41/query?where=1%3D1&outFields=*&outSR=4326&f=json",
              method="auto",mode="wb")
