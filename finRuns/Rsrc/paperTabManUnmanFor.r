
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
cons10run=F
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





library(dplyr)
library(ggplot2)
library(data.table)

# minDharvX = 999
# landClassX = 1
# mortMod=3
# regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
setwd("/scratch/project_2000994/PREBASruns/finRuns")
source("Rsrc/fra/outSampleSettings.r")
cons10run=T
run_settings <- paste0("_addHarvNO","_landClassX",range(landClassX)[1],
                       "to",range(landClassX)[2],"_mortMod",mortMod)
regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")

meanRegion <- data.table()
# areasCountry <- data.table()
# areasProtectCountry <- data.table()
# dataCountry <- data.table()
# dataProtectCountry <- data.table()
areaAllRegions <- NULL
for(r_no in c(1,3:19)){
  
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  if(cons10run){
    load(paste0("input/maakunta/maakunta_",r_no,"_IDsCons10.rdata"))
    
    cons10Dat$segID <- cons10Dat$maakuntaID
    setkey(cons10Dat,segID)
    idX <- which(data.all$segID %in% cons10Dat$segID)
    xx <- data.all[-idX]
    
    setnames(xx,"N","nPix")
    cons10Dat$newCons=NULL
    cons10Dat$oldMaakID=NULL
    xx$Wbuffer=NULL
    cons10Dat$Wbuffer=NULL
    cons10Dat[,area:=nPix*16*16/10000]
    
    new.data.all <- rbind(xx,cons10Dat)
    
    areaRegion <- sum(data.all$area,na.rm=T)
    areaRegionManFor <- sum(new.data.all[cons==0]$area,na.rm=T)*sum(data.all$area,na.rm=T)/sum(new.data.all$area,na.rm=T)
    areaRegionUnmanFor <- sum(new.data.all[cons==1]$area,na.rm=T)*sum(data.all$area,na.rm=T)/sum(new.data.all$area,na.rm=T)
    
    data.all <- new.data.all
    
  }else{
    areaRegion <- sum(data.all$area,na.rm=T)
    areaRegionManFor <- sum(data.all[cons==0]$area,na.rm=T)
    areaRegionUnmanFor <- sum(data.all[cons==1]$area,na.rm=T)
  }
  
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
  
  if(!cons10run){
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
    
    datAllScenNormProtect <- merge(datAllScenNormProtect,protect.data.all[,.(segID,cons)])
    
  }else{
    areaRegionManFor.protect <- areaRegionManFor 
    areaRegionUnmanFor.protect <- areaRegionUnmanFor 
    
    datAllScenNormProtect <- merge(datAllScenNormProtect,data.all[,.(segID,cons)])
  }
  
  
  
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
# tabRegion <- tabCountry <- data.table()
meanReg <- data.table()

# load(pathX)
protAreasX <- "cons10run"
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


# add region2 to +10%
reg2 <- tabRegion[region==2]
reg2$protAreas="cons10run"
tabRegion <- rbind(tabRegion,reg2)

# calculate country sums
tabCountry <- tabRegion[,.(volGrowth_tot = sum(volGrowth_tot),
                           Wtrees_tot = sum(Wtrees_tot),
                           Wgv_tot = sum(Wgv_tot),
                           soilC_tot = sum(soilC_tot),
                           Cbal_tot = sum(Cbal_tot),
                           Cstock_tot = sum(Cstock_tot)),
                        by=.(harScen,harvInten,cons,
                             period,protAreas)]

tabRegion[harScen=="protectTapio" & region==11]                

save(tabRegion,tabCountry,file="manUnmanForRes.rdata")


# make plots
tabCountry$period <- factor(tabCountry$period,levels = periods)
tabRegion$period <- factor(tabRegion$period,levels = periods)


varX="Cstock_tot"
dataX <- tabCountry
# ylimX <- dataX[,range(get(varX),na.rm=T)]
p1 <- ggplot(dataX[harScen=="Base" & cons==0]) +
  geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
  ggtitle("Base") + ylab(varX)#+ ylim(ylimX) 

p2 <- ggplot(dataX[harScen=="Base" & cons==1]) +
  geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
  ggtitle("Base") + ylab(varX)#+ ylim(ylimX) 




####plotFunction
createPlot <- function(varX,dataX){
  # ylimX <- dataX[,range(get(varX),na.rm=T)]
  p1 <- ggplot(dataX[harScen=="Base"]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ggtitle("Base") + ylab(varX)#+ ylim(ylimX) + 
  # geom_point(data=dataX[harScen=="NoHarv"],
  #            aes(x=period,y=get(varX),col=protAreas,shape="NoHarv"))
  
  harscenX <- "Mitigation"
  p2 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylab(varX)+ggtitle(harscenX) #ylim(ylimX) +
  
  harscenX <- "adapt"
  p3 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylab(varX)+ggtitle(harscenX) #ylim(ylimX) + 
  
  harscenX <- "protect"
  p4 <- ggplot(dataX[harScen==harscenX]) +
    geom_point(aes(x=period,y=get(varX),col=protAreas,shape=harvInten))+
    ylab(varX)+ggtitle(harscenX) #ylim(ylimX) + 
  
  allPlots <- ggarrange(p1,p2,p3,p4,common.legend = T)
  
  return(allPlots)}




####plotFunction
createPlotTapio <- function(dataX,
                            harscensIn = c("adaptTapio","protectTapio",
                                           "NoHarv","MitigationTapio","baseTapio")){
  # filter data
  dataX <- dataX[harScen %in% harscensIn]
  dataX <- dataX[!(harScen == "NoHarv" & protAreas=="+10%")]
  
  # ylimX <- dataX[,range(volGrowth_tot,na.rm=T)]
  p1 <- ggplot(dataX) +
    geom_point(aes(x=period,y=volGrowth_tot,col=protAreas,shape=harScen))+
    ggtitle("Tapio") + ylab("volGrowth_tot") #+ ylim(ylimX)
  # varX <- "Cstock_tot"
  # ylimX <- dataX[,range(Cstock_tot,na.rm=T)]
  p2 <- ggplot(dataX) +
    geom_point(aes(x=period,y=Cstock_tot,col=protAreas,shape=harScen))+
    ggtitle("Tapio") + ylab("Cstock_tot")#+ ylim(ylimX)
  # varX <- "Cbal_tot"
  # ylimX <- dataX[,range(Cbal_tot,na.rm=T)]
  p3 <- ggplot(dataX) +
    geom_point(aes(x=period,y=Cbal_tot,col=protAreas,shape=harScen))+
    ggtitle("Tapio") + ylab("Cbal_tot")#+ ylim(ylimX)
  
  allPlots <- ggarrange(p1,p2,p3,common.legend = T)
  
  return(allPlots)}





plotList <- list()
for(varX in c("volGrowth_tot","Cstock_tot","Cbal_tot")){
  plotList[[varX]] <- createPlot(varX,tabCountry)  
}


pdf(file = "manUnmanPlotsCountry.pdf")
print(plotList$volGrowth_tot)
print(plotList$Cstock_tot)
print(plotList$Cbal_tot)
print(createPlotTapio(tabCountry))
dev.off()



pdf(file = "manUnmanPlotsRegions.pdf")
regNames <- unique(tabRegion$regNames)
for(ij in 1:19){
  i <- regNames[ij]
  for(varX in c("volGrowth_tot","Cstock_tot","Cbal_tot")){
    plotList[[varX]] <- createPlot(varX,tabRegion[regNames==i])
    px <- annotate_figure(plotList[[varX]], top = text_grob(regNames[ij], 
                                                            color = "red", face = "bold", size = 14))
    print(px)
  }
  print(createPlotTapio(tabRegion[region==ij]))
}
dev.off()
