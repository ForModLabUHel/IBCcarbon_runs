library(ggpubr)
library(data.table)
library(ggplot2)

setwd("C:/Users/minunno/Documents/research/IBC-carbon/plots/")
pathXcons10 <- "outSampleHcF1.2_cons10run/country_addHarvNO_landClassX1to2_mortMod3.rdata"
pathX <- "outSampleHcF1.2/country_addHarvNO_landClassX1to2_mortMod3.rdata"
regNames <- fread("maakunta_names.txt")
setnames(regNames,"regIDs","region")
setkey(regNames,"region")

present <- 1:5
p17_25 <- 2:10
p26_33 <- 11:18
p34_50 <- 19:35


periods <- c("present","p17_25","p26_33","p34_50")
periodX <- "present"
tabRegion <- tabCountry <- data.table()
meanReg <- data.table()

load(pathX)
protAreasX <- "current"
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
