library(data.table)
allData_currPA <- fread("C:/Users/minunno/Documents/research/IBC-carbon/results/outSampleHcF1.2/summaryRes/allRes_currentPA.csv")
allData_currPA$V1 <- NULL
allData_10PA <- fread("C:/Users/minunno/Documents/research/IBC-carbon/results/outSampleHcF1.2_cons10run/summaryRes/allRes_10%PA.csv")
allData_10PA$V1 <- NULL

allData_currPA$period <- allData_10PA$period <- "NaN"
allData_currPA[year %in% 2:10]$period <- "2017-2025"
allData_10PA[year %in% 2:10]$period <- "2017-2025"
allData_currPA[year %in% 11:18]$period <- "2026-2033"
allData_10PA[year %in% 11:18]$period <- "2026-2033"
allData_currPA[year %in% 19:36]$period <- "2034-2050"
allData_10PA[year %in% 19:36]$period <- "2034-2050"

allData_10PA$management <- allData_currPA$management <- "all"
allData_10PA[cons==1]$management <- "protection areas"
allData_10PA[cons==0]$management <- "managed forests"
allData_currPA[cons==1]$management <- "protection areas"
allData_currPA[cons==0]$management <- "managed forests"

###calc table vars
allData_currPA[,Vharvested := (VroundWood+VenergyWood)*area/1e6]
allData_currPA[,Wharvested := (WroundWood+WenergyWood)*area/1e9 * 3.67]
allData_currPA[,NEEtot := -1*(NEP)*area/1e8 * 3.67]
allData_currPA[,WtreesGV := (GVw + WtotTrees)*area/1e9]
allData_currPA[,soilCtot := (soilC)*area/1e9]
allData_currPA[,VolGrowth := grossGrowth/1e6]

allData_10PA[,Vharvested := (VroundWood+VenergyWood)*area/1e6]
allData_10PA[,Wharvested := (WroundWood+WenergyWood)*area/1e9 * 3.67]
allData_10PA[,NEEtot := -1*(NEP)*area/1e8 * 3.67]
allData_10PA[,WtreesGV := (GVw + WtotTrees)*area/1e9]
allData_10PA[,soilCtot := (soilC)*area/1e9]
allData_10PA[,VolGrowth := grossGrowth/1e6]

namesVar <- names(allData_currPA)
noMeanVars <- c("harScen","year","harvInten","management","runsID","maakID","maakNames","period")
meanVars <- namesVar[which(!namesVar %in% noMeanVars)]
groups <- c("harScen","harvInten","management","maakNames","period")

datacurrPA <- allData_currPA[,lapply(.SD, mean,na.rm=T),.SDcols = meanVars,
                   by=groups]
datacurrPA <- datacurrPA[period!="NaN"]

datacurrPA$ProtAreas <- "current"

data10PA <- allData_10PA[,lapply(.SD, mean,na.rm=T),.SDcols = meanVars,
                                 by=groups]
data10PA <- data10PA[period!="NaN"]
data10PA$ProtAreas <- "10%"

dataByPerMeans <- rbind(datacurrPA,data10PA)
tableS2 <- dataByPerMeans[,.(harScen,harvInten,ProtAreas,
                              management,maakNames,Vharvested,Wharvested,NEEtot)]

tableS3 <- dataByPerMeans[,.(harScen,harvInten,ProtAreas,
                             management,maakNames,
                             VolGrowth,WtreesGV,soilCtot,
                             Vharvested,NEEtot)]

setwd("C:/Users/minunno/Documents/research/IBC-carbon/results/tables/")
write.csv(tableS2,file = "tableS2.csv")
write.csv(tableS3,file = "tableS3.csv")
