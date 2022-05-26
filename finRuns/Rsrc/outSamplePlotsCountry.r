library(dplyr)
library(ggplot2)
library(data.table)

# minDharvX = 999
# landClassX = 1
# mortMod=3
# regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
# setwd("/scratch/project_2000994/PREBASruns/finRuns")
# run_settings <- "_addHarvNO_landClassX1_mortMod3"
# r_no=1
# regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")
# outDyr <- "outSampleHcF1.2_cons10run"

meanRegion <- data.table()
# areasCountry <- data.table()
# areasProtectCountry <- data.table()
# dataCountry <- data.table()
# dataProtectCountry <- data.table()
areaAllRegions <- NULL
for(r_no in regions){

  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  areaRegion <- sum(data.all$area,na.rm=T)
  areaAllRegions <- c(areaAllRegions,areaRegion)
  load(paste0(outDyr,"/r_no",r_no,run_settings,".rdata"))
  
  datAllScenNorm <- datAllScen
  datAllScenNormProtect <- datAllScenProtect
  setkey(areas,segID)
  setkey(datAllScenNorm,segID)
  setkey(areasProtect,segID)
  setkey(datAllScenNormProtect,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas)
  datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen","harvInten","cons")]
  # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  datAllScenNormProtect[, vars] <- 
    datAllScenNormProtect[ ,lapply(.SD, `*`, area*length(areasProtect$area)/sum(areasProtect$area)), .SDcols = vars]

  meanScenProtect <- 
    datAllScenNormProtect[ ,lapply(.SD, mean,na.rm=T),
                .SDcols = vars,by=.(harScen,year,harvInten)]
  
  meanScen <- 
    datAllScenNorm[ ,lapply(.SD, mean,na.rm=T), .SDcols = vars,by=.(harScen,year,harvInten)]
  
  meanScenProtect$region=r_no
  meanScen$region=r_no
  meanScenProtect$area=areaRegion
  meanScen$area=areaRegion
  meanScen <- rbind(meanScen,meanScenProtect)
  meanRegion <- rbind(meanRegion,meanScen)
print(r_no)  
}

countryArea <- sum(areaAllRegions)

  meanScenNorm <- meanRegion
    meanScenNorm[, vars] <- 
      meanRegion[ ,lapply(.SD, `*`, area/countryArea), .SDcols = vars]

    meanCountry <- meanScenNorm[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten)]

    meanCountry$CbalState=0
    meanCountry[year %in% 2:max(meanCountry$year)]$CbalState=
    -(meanCountry[year %in% 2:max(meanCountry$year),
                (WtotTrees+soilC+GVw+Wdb)] -
      meanCountry[year %in% 1:(max(meanCountry$year)-1),
                  (WtotTrees+soilC+GVw+Wdb)])*44/12/1e9*
      countryArea
    
    meanCountry[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)*
                    44/12*countryArea/1e9]
    meanCountry[year ==1]$CbalState=NA
    
    save(meanCountry,meanRegion,countryArea,
     file = paste0(outDyr,"/country",
                   run_settings,".rdata"))

    dataPlot <- meanCountry[harScen!="adaptTapio"]
pdf(paste0(outDyr,"/plots/plots_country.pdf"))
for(varX in vars){
  # i=i+1
  print(ggplot(dataPlot)+
  # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
  geom_line(aes(x = year+ 2016, y = get(varX), color = harScen,linetype=harvInten)) + 
  xlab("year") + ylab(varX))
# print(ggplot(meanCountry)+
#         # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
#         geom_line(aes(x = year+ 2016, y = get(varX)*countryArea/1e6,
#                       color = harScen)) + 
#         xlab("year") + ylab(varX))
}
print(ggplot(dataPlot)+
        # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
        # geom_line(aes(x = year+ 2016, y = CbalFluxes, color = harScen,linetype=harvInten)) + 
        geom_line(aes(x = year+ 2016, y = CbalState, color = harScen,linetype=harvInten)) +
        xlab("year") + ylab("C balance (State)"))
print(ggplot(dataPlot)+
        # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
        geom_line(aes(x = year+ 2016, y = CbalFluxes, color = harScen,linetype=harvInten)) +
        # geom_line(aes(x = year+ 2016, y = CbalState, color = harScen,linetype=harvInten)) +
        xlab("year") + ylab("C balance (Fluxes)"))
dev.off()


scens <- unique(meanRegion$harScen)
meanRegion$region <- as.factor(meanRegion$region)
meanRegion$regIDs <- meanRegion$region
meanRegion$region <- NULL
setkey(regionNames,regIDs)
setkey(meanRegion,regIDs)
regionNames$regIDs <- as.factor(regionNames$regIDs)
meanRegion <- merge(meanRegion,regionNames)

pdf(paste0(outDyr,"/plots/plots_ScenariosCountry.pdf"))
for(varX in vars){
  for(scenX in scens){
      # i=i+1
    print(ggplot(meanCountry[harScen==scenX])+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = get(varX), color = harvInten)) + 
      xlab("year") + ylab(varX)+ ggtitle(scenX))
  }
}
dev.off()

# for(r_no in 1:19){
# pdf(paste0("outSample/plots/plots_Scenarios_",
#            regionNames[r_no]$regNames,".pdf"))
# for(varX in vars){
#   for(scenX in scens){
#     # i=i+1
#     print(ggplot(meanRegion[harScen==scenX & regIDs==r_no])+
#             # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
#             geom_line(aes(x = year+ 2016, y = get(varX), color = harvInten)) + 
#             xlab("year") + ylab(varX)+ ggtitle(scenX))
#   }
# }
# dev.off()
# }
# 

write.csv(meanCountry,file=paste0(outDyr,"/plots/MeanCountryAllRuns.csv"))
write.csv(meanCountry[harScen=="Base" & harvInten=="Base"],
          file=paste0(outDyr,"/plots/MeanCountryBaseRuns.csv"))
write.csv(meanRegion,file=paste0(outDyr,"/plots/MeanRegionAllRuns.csv"))
write.csv(meanRegion[harScen=="Base" & harvInten=="Base"],
          file=paste0(outDyr,"/plots/MeanRegionBaseRuns.csv"))