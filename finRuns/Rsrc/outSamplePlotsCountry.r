library(dplyr)
library(ggplot2)
library(data.table)

minDharvX = 999
landClassX = 1
mortMod=3
regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
setwd("/scratch/project_2000994/PREBASruns/finRuns")
run_settings <- "_addHarvNO_landClassX1_mortMod3"
r_no=1
regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")


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
  load(paste0("outSample/r_no",r_no,run_settings,".rdata"))
  
  datAllScenNorm <- datAllScen
  datAllScenNormProtect <- datAllScenProtect
  setkey(areas,segID)
  setkey(datAllScenNorm,segID)
  setkey(areasProtect,segID)
  setkey(datAllScenNormProtect,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas)
  datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen")]
  # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  datAllScenNormProtect[, vars] <- 
    datAllScenNormProtect[ ,lapply(.SD, `*`, area*length(areasProtect$area)/sum(areasProtect$area)), .SDcols = vars]
  
  
  meanScenProtect <- 
    datAllScenNormProtect[ ,lapply(.SD, mean,na.rm=T), .SDcols = vars,by=.(harScen,year)]
  
  meanScen <- 
    datAllScenNorm[ ,lapply(.SD, mean,na.rm=T), .SDcols = vars,by=.(harScen,year)]
  
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

    meanCountry <- meanScenNorm[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year)]

save(meanCountry,meanRegion,
     file = paste0("outSample/country",
                   run_settings,".rdata"))

pdf(paste0("outSample/plots/plots_country.pdf"))
for(varX in vars){
  # i=i+1
print(ggplot(meanCountry)+
  # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
  geom_line(aes(x = year+ 2016, y = get(varX), color = harScen)) + 
  xlab("year") + ylab(varX))

# print(ggplot(meanCountry)+
#         # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
#         geom_line(aes(x = year+ 2016, y = get(varX)*countryArea/1e6,
#                       color = harScen)) + 
#         xlab("year") + ylab(varX))

}
dev.off()


scens <- unique(meanRegion$harScen)
meanRegion$region <- as.factor(meanRegion$region)
meanRegion$regIDs <- meanRegion$region
meanRegion$region <- NULL
setkey(regionNames,regIDs)
setkey(meanRegion,regIDs)
regionNames$regIDs <- as.factor(regionNames$regIDs)
meanRegion <- merge(meanRegion,regionNames)

pdf(paste0("outSample/plots/plots_Scenarios.pdf"))
for(varX in vars){
  for(scenX in scens){
      # i=i+1
    print(ggplot(meanRegion[harScen==scenX])+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = get(varX), color = regNames)) + 
      xlab("year") + ylab(varX)+ ggtitle(scenX))
    
  }
}
dev.off()

