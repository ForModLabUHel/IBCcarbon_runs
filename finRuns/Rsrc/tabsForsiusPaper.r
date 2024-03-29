library(data.table)

fileRoot <- "country_clcutArFact1.2_addHarv2_landClassX1to2_mortMod13_"

pathX <- "C:/Users/minunno/Documents/research/IBC-carbon/results/outSampleHcF1.2/summaryRes/"
allData_currPA <- fread(paste0(pathX,"allRes_currentPA_",fileRoot,".csv"))
allData_currPA$V1 <- NULL
pathX <- "C:/Users/minunno/Documents/research/IBC-carbon/results/outSampleHcF1.2_cons10run/summaryRes/"
allData_10PA <- fread(paste0(pathX,"allRes_10%PA_",fileRoot,".csv"))
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
allData_currPA[,VolGrowth := grossGrowth*area/1e6]

allData_10PA[,Vharvested := (VroundWood+VenergyWood)*area/1e6]
allData_10PA[,Wharvested := (WroundWood+WenergyWood)*area/1e9 * 3.67]
allData_10PA[,NEEtot := -1*(NEP)*area/1e8 * 3.67]
allData_10PA[,WtreesGV := (GVw + WtotTrees)*area/1e9]
allData_10PA[,soilCtot := (soilC)*area/1e9]
allData_10PA[,VolGrowth := grossGrowth*area/1e6]

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
tableS2 <- dataByPerMeans[,.(period,harScen,harvInten,ProtAreas,area,
                              management,maakNames,Vharvested,Wharvested,NEEtot)]

tableS3 <- dataByPerMeans[,.(period,harScen,harvInten,ProtAreas,area,
                             management,maakNames,
                             VolGrowth,WtreesGV,soilCtot,
                             Vharvested,NEEtot)]

setwd("C:/Users/minunno/Documents/research/IBC-carbon/results/tables/")
write.csv(tableS2,file = paste0("tableS2",fileRoot,".csv"))
write.csv(tableS3,file = paste0("tableS3",fileRoot,".csv"))


# tableS3_v2 <- tableS3[maakNames=="country" & management %in% c("all","protection areas") & harScen=="Base"]
# write.csv(tableS3_v2,file = paste0("tableS3_v2",fileRoot,".csv"))


# tableS2_rcp4.5 <- tableS2[maakNames=="country" & management %in% c("all","protection areas") & harScen=="Base"]
# tableS3_rcp4.5 <- tableS3[maakNames=="country" & management %in% c("all","protection areas") & harScen=="Base"]
# write.csv(tableS2_rcp4.5,file = paste0("tableS2_rcp4.5",fileRoot,".csv"))
# write.csv(tableS3_rcp4.5,file = paste0("tableS3_rcp4.5",fileRoot,".csv"))






# library(data.table)
# dataX <- fread("C:/Users/minunno/Documents/research/IBC-carbon/results/tables/tableS2_rcp4.5country_clcutArFact1.2_addHarv2_landClassX1to2_mortMod13_.csv")
# dataX$V1 <- NULL
# dataX2 <- fread("C:/Users/minunno/Documents/research/IBC-carbon/results/tables/tableS3_rcp4.5country_clcutArFact1.2_addHarv2_landClassX1to2_mortMod13_.csv")
# dataX2$V1 <- NULL
# 
# areas <- fread("C:/Users/minunno/Documents/research/IBC-carbon/results/tables/areasFor.txt")
# 
# myData <- dataX[harScen=="Base" & !maakNames %in% c("Aland","country")]
# myData[maakNames=="PohjoisNASavo"]$maakNames <- "PohjoisSavo"
# myData[maakNames=="VarsnaisSuomi"]$maakNames <- "VarsinaisSuomi"
# 
# myData2 <- dataX2[harScen=="Base" & !maakNames %in% c("Aland","country")]
# myData2[maakNames=="PohjoisNASavo"]$maakNames <- "PohjoisSavo"
# myData2[maakNames=="VarsnaisSuomi"]$maakNames <- "VarsinaisSuomi"
# 
# regNames <- unique(areas$regNames)
# 
# for(i in 1:18){
#   for(protAreaX in c("current","10%")){
#     for(managX in c("all","protection areas","managed forests")){
#       xdat <- myData[ProtAreas==protAreaX & 
#                        maakNames==regNames[i]&
#                        management==managX]
#       
#       xdat2 <- myData2[ProtAreas==protAreaX & 
#                          maakNames==regNames[i]&
#                          management==managX]
#       
#       xarea <- areas[regNames==regNames[i] & 
#                        ProtAreas==protAreaX]
#       
#       if(managX=="all") areaX <- xarea$areaTot
#       if(managX=="protection areas") areaX <- xarea$consArea
#       if(managX=="managed forests") areaX <- xarea$manArea
#       
#       xdat[,Vharvested:=Vharvested/area*areaX]
#       xdat[,Wharvested:=Wharvested/area*areaX]
#       xdat[,NEEtot:=NEEtot/area*areaX]
#       
#       xdat2[,VolGrowth:=VolGrowth*areaX]
#       xdat2[,WtreesGV:=WtreesGV/area*areaX]
#       xdat2[,soilCtot:=soilCtot/area*areaX]
#       xdat2[,Vharvested:=Vharvested/area*areaX]
#       xdat2[,NEEtot:=NEEtot/area*areaX]
#       
#       
#       myData[ProtAreas==protAreaX & 
#                maakNames==regNames[i]&
#                management==managX] <- xdat
#       
#       myData2[ProtAreas==protAreaX & 
#                 maakNames==regNames[i]&
#                 management==managX] <- xdat2
#       
#     }
#   }
# }
# 
# 
# countryS2 <- myData[,.(Vharvested=sum(Vharvested),
#                        Wharvested=sum(Wharvested),
#                        NEEtot=sum(NEEtot)),
#                     by=.(period,harScen,
#                          harvInten,ProtAreas,
#                          management,maakNames)]
# 
# countryS3 <- myData2[,.(VolGrowth=sum(VolGrowth),
#                         WtreesGV=sum(WtreesGV),
#                         soilCtot=sum(soilCtot),
#                         Vharvested=sum(Vharvested),
#                         NEEtot=sum(NEEtot)),
#                      by=.(period,harScen,
#                           harvInten,ProtAreas,
#                           management,maakNames)]
# 
# write.csv(countryS2,file="tableS2_rcp4.5country_clcutArFact1.2_addHarv2_landClassX1to2_mortMod13_.csv")
# write.csv(countryS3,file="tableS3_rcp4.5country_clcutArFact1.2_addHarv2_landClassX1to2_mortMod13_.csv")
