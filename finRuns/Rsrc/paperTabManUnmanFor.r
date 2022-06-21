
#####process conservation areas


library(dplyr)
library(ggplot2)
library(data.table)

# minDharvX = 999
# landClassX = 1
# mortMod=3
# regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
setwd("/scratch/project_2000994/PREBASruns/finRuns")
source("Rsrc/fra/outSampleSettings.r")
run_settings <- paste0("_addHarvNO","_landClassX",range(landClassX)[1],
                       "to",range(landClassX)[2],"_mortMod",mortMod)
regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")

meanRegion <- data.table()
# areasCountry <- data.table()
# areasProtectCountry <- data.table()
# dataCountry <- data.table()
# dataProtectCountry <- data.table()
areaAllRegions <- NULL
for(r_no in 1:19){
  
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  areaRegion <- sum(data.all$area,na.rm=T)
  areaRegionManFor <- sum(data.all[cons==0]$area,na.rm=T)
  areaRegionUnmanFor <- sum(data.all[cons==1]$area,na.rm=T)
  
  areaAllRegions <- c(areaAllRegions,areaRegion)
  load(paste0(outDyr,"/r_no",r_no,run_settings,".rdata"))
  
  datAllScenNorm <- datAllScen
  datAllScenNormProtect <- datAllScenProtect
  setkey(areas,segID)
  setkey(data.all,segID)
  setkey(datAllScenNorm,segID)
  setkey(areasProtect,segID)
  setkey(datAllScenNormProtect,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas)
  datAllScenNorm <- merge(datAllScenNorm,data.all[,.(segID,cons)])
  
  
  datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
  load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsBuffer.rdata"))
  buffDat$segID <- buffDat$maakuntaID
  setkey(buffDat,segID)
  dataBuff <- which(data.all$segID %in% buffDat$segID)
  xx <- data.all[-dataBuff]
  
  setnames(xx,"N","nPix")
  buffDat$newCons=NULL
  buffDat$oldMaakID=NULL
  xx$Wbuffer=NULL
  buffDat[,area:=nPix*16*16/10000]
  
  protect.data.all <- rbind(xx,buffDat)
  areaRegion.protect <- sum(protect.data.all$area,na.rm=T) 
  areaRegionManFor.protect <- sum(protect.data.all[cons==0]$area,na.rm=T) * areaRegion/areaRegion.protect
  areaRegionUnmanFor.protect <- sum(protect.data.all[cons==1]$area,na.rm=T) * areaRegion/areaRegion.protect
  
  # datAllScen <- merge(datAllScen,data.all[,.(segID,area,cons)])
  datAllScenNormProtect <- merge(datAllScenNormProtect,protect.data.all[,.(segID,cons)])
  
  
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen","harvInten","cons")]
  # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  datAllScenNormProtect[, vars] <- 
    datAllScenNormProtect[ ,lapply(.SD, `*`, area*length(areasProtect$area)/sum(areasProtect$area)), .SDcols = vars]
  
  meanScenProtect <- 
    datAllScenNormProtect[ ,lapply(.SD, mean,na.rm=T),
                           .SDcols = vars,by=.(harScen,year,harvInten,cons)]
  
  meanScen <- 
    datAllScenNorm[ ,lapply(.SD, mean,na.rm=T), .SDcols = vars,by=.(harScen,year,harvInten,cons)]
  
  meanScenProtect$region=r_no
  meanScen$region=r_no
  
  meanScenProtect$area = 0
  meanScenProtect[cons==1]$area=areaRegionUnmanFor.protect
  meanScenProtect[cons==0]$area=areaRegionManFor.protect
  
  meanScen$area=0
  meanScen[cons==1]$area=areaRegionUnmanFor
  meanScen[cons==0]$area=areaRegionManFor
  
  meanScen <- rbind(meanScen,meanScenProtect)
  meanRegion <- rbind(meanRegion,meanScen)
  print(r_no)  
}




present <- 1:5
p17_25 <- 2:10
p26_33 <- 11:18
p34_50 <- 19:35


periods <- c("present","p17_25","p26_33","p34_50")
periodX <- "present"
tabRegion <- tabCountry <- data.table()
meanReg <- data.table()

# load(pathX)
protAreasX <- "current"
for(periodX in periods){
  res <- meanRegion[year %in% get(periodX),
                    .(volGrowth = mean(grossGrowth),
                      Wtrees=mean(WtotTrees,na.rm=F),
                      Wgv=mean(GVw,na.rm=F),
                      soilC=mean(soilC,na.rm=F),
                      area = mean(area),
                      Cbal=mean(-NEP*10+WenergyWood+WroundWood)
                    ),by=.(harScen,harvInten,region,cons)]
  res[,Cstock:= Wtrees + Wgv + soilC] 
  resTot <- meanRegion[year %in% get(periodX),
                       .(volGrowth_tot = mean(grossGrowth*area),
                         Wtrees_tot=mean(WtotTrees*area,na.rm=F),
                         Wgv_tot=mean(GVw*area,na.rm=F),
                         soilC_tot=mean(soilC*area,na.rm=F),
                         Cbal_tot=mean((-NEP*10+WenergyWood+WroundWood)*area)
                       ),by=.(harScen,harvInten,region,cons)]
  resTot[,Cstock_tot:= Wtrees_tot + Wgv_tot + soilC_tot] 
  res <- merge(res,resTot)
  res$period <- periodX
  res[,volGrowth_tot := volGrowth_tot/1e6]
  res[,Cstock_tot := Cstock_tot/1e9]
  res[,Cbal_tot := Cbal_tot/1e9]
  res$protAreas <- protAreasX
  tabRegion <- rbind(tabRegion,res)
}







for(periodX in periods){
  res <- meanRegion[year %in% get(periodX),
                    .(volGrowth = mean(grossGrowth),
                      Wtrees=mean(WtotTrees,na.rm=F),
                      Wgv=mean(GVw,na.rm=F),
                      soilC=mean(soilC,na.rm=F),
                      Cbal=mean(-NEP*10+WenergyWood+WroundWood),
                      area=mean(area)
                    ),by=.(harScen,harvInten,region)]
  res$period <- periodX
  res[,Cstock:= Wtrees + Wgv + soilC] 
  res[,volGrowth_tot := volGrowth*area/1e6]
  res[,Cstock_tot := Cstock*area/1e9]
  res[,Cbal_tot := Cbal*area/1e9]
  res$protAreas <- protAreasX
  tabRegion <- rbind(tabRegion,res)
}

load(pathXcons10)
protAreasX <- "+10%"
for(periodX in periods){
  res <- meanCountry[year %in% get(periodX),
                     .(volGrowth = mean(grossGrowth),
                       Wtrees=mean(WtotTrees,na.rm=F),
                       Wgv=mean(GVw,na.rm=F),
                       soilC=mean(soilC,na.rm=F),
                       Cbal=mean(-NEP*10+WenergyWood+WroundWood)
                     ),by=.(harScen,harvInten)]
  res$period <- periodX
  res[,Cstock:= Wtrees + Wgv + soilC] 
  res[,volGrowth_tot := volGrowth*countryArea/1e6]
  res[,Cstock_tot := Cstock*countryArea/1e9]
  res[,Cbal_tot := Cbal*countryArea/1e9]
  res$protAreas <- protAreasX
  tabCountry <- rbind(tabCountry,res)
}

for(periodX in periods){
  res <- meanRegion[year %in% get(periodX),
                    .(volGrowth = mean(grossGrowth),
                      Wtrees=mean(WtotTrees,na.rm=F),
                      Wgv=mean(GVw,na.rm=F),
                      soilC=mean(soilC,na.rm=F),
                      Cbal=mean(-NEP*10+WenergyWood+WroundWood),
                      area=mean(area)
                    ),by=.(harScen,harvInten,region)]
  res$period <- periodX
  res[,Cstock:= Wtrees + Wgv + soilC] 
  res[,volGrowth_tot := volGrowth*area/1e6]
  res[,Cstock_tot := Cstock*area/1e9]
  res[,Cbal_tot := Cbal*area/1e9]
  res$protAreas <- protAreasX
  tabRegion <- rbind(tabRegion,res)
}

tabCountry$period <- factor(tabCountry$period,levels = periods)
tabRegion$period <- factor(tabRegion$period,levels = periods)

setkey(tabRegion,"region")
tabRegion <- merge(tabRegion,regNames)


####plotFunction
createPlot <- function(varX,dataX){
  ylimX <- dataX[harScen!="adaptTapio",range(get(varX),na.rm=T)]
  p1 <- ggplot(dataX[harScen=="Base"]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ggtitle("Base") + ylab(varX)+ ylim(ylimX) + 
    geom_point(data=dataX[harScen=="NoHarv"],
               aes(x=period,y=get(varX),col=protAreas,shape="NoHarv"))
  
  harscenX <- "Mitigation"
  p2 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylim(ylimX) + ylab(varX)+ggtitle(harscenX)
  
  harscenX <- "adapt"
  p3 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylim(ylimX) + ylab(varX)+ggtitle(harscenX)
  
  harscenX <- "protect"
  p4 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylim(ylimX) + ylab(varX)+ggtitle(harscenX)
  
  allPlots <- ggarrange(p1,p2,p3,p4,common.legend = T)
  
  return(allPlots)}

plotList <- list()
for(varX in c("volGrowth_tot","Cstock_tot","Cbal_tot")){
  plotList[[varX]] <- createPlot(varX,tabCountry)  
}

pdf(file = "plotsCountry.pdf")
plotList$volGrowth_tot
plotList$Cstock_tot
plotList$Cbal_tot
dev.off()
write.csv(tabCountry,file="tabCountry.csv")

pdf(file = "plotsRegions.pdf")
regNames <- unique(tabRegion$regNames)
for(ij in 1:19){
  i <- regNames[ij]
  for(varX in c("volGrowth_tot","Cstock_tot","Cbal_tot")){
    plotList[[varX]] <- createPlot(varX,tabRegion[regNames==i])
    px <- annotate_figure(plotList[[varX]], top = text_grob(regNames[ij], 
                                                            color = "red", face = "bold", size = 14))
    print(px)
  }
}
dev.off()

write.csv(tabRegion,file="tabRegions.csv")


























countryArea <- sum(areaAllRegions)
xx <- unique(meanRegion[,.(cons,region,area,harScen)])
countryAreaX <-  xx[,.(areaCountry=sum(area)),by=.(cons,harScen)]

meanScenNorm <- meanRegion
meanScenNorm <- merge(meanScenNorm,countryAreaX)
meanScenNorm[, vars] <- 
  meanScenNorm[ ,lapply(.SD, `*`, area/areaCountry), .SDcols = vars]

meanCountry <- meanScenNorm[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten,cons)]

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
  print(ggplot(dataPlot[harScen=="Base"])+
          # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
          geom_line(aes(x = year+ 2016, y = get(varX), color = as.factor(cons),linetype=harvInten)) + 
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


#####compare runs +10% cons areas vs. actual situation
dat1 <- fread(file="outSampleHcF1.2/plots/MeanCountryAllRuns.csv")
dat1$cons = "actual"

dat2 <- fread(file="outSampleHcF1.2_cons10run/plots/MeanCountryAllRuns.csv")
dat2$cons = "+10%"
datAll <- rbind(dat1,dat2)
names(dat2)

scens2 <- scens[-which(scens%in%c("adaptTapio","NoHarv"))]
pdf(paste0(outDyr,"/plots/plots_ScenariosCountry2.pdf"))
for(varX in vars){
  for(scenX in scens2){
    # i=i+1
    print(ggplot(datAll[harScen==scenX])+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
            geom_line(aes(x = year+ 2016, y = get(varX), color = cons,linetype=harvInten)) +
            xlab("year") + ylab(varX)+ ggtitle(scenX))
  }
}
dev.off()
