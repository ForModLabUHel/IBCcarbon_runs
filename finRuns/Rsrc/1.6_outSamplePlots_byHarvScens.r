library(dplyr)
library(ggplot2)
library(data.table)
setwd("/scratch/project_2000994/PREBASruns/finRuns/")
source("Rsrc/fra/maakuntaRuns/codes/outSampleSettings.r")

##settings
HSIminVal <- 0.5
harvScen <- "Base"
for(harvScen in c("Base", "protect",  "adapt", "Mitigation","TapioAndNoHarv")){ ## Base protect  adapt Mitigation TapioAndNoHarv

  # devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/outSamplePlotsCountry.r")
  #devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/outSamplePlotsRegions.r")
  # for(harvScen in c("Base", "protect",  "adapt", "Mitigation", "TapioAndNoHarv")){
  
  
  # minDharvX = 999
  # landClassX = 1
  # mortMod=3
  # regions <- 1:19 #c(8, 3,9,10,13,14,15,19)
  setwd("/scratch/project_2000994/PREBASruns/finRuns")
  run_settings <- paste0("_clcutArFact",clcutArFact,
                         "_addHarv",compHarvX,"_landClassX",range(landClassX)[1],
                         "to",range(landClassX)[2],"_mortMod",mortMod)
  
  regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")
  # outDyr <- "outSampleHcF1.2_cons10run"
  
  meanRegion <- meanRegionMan <- meanRegionUnman <- data.table()
  # areasCountry <- data.table()
  # areasProtectCountry <- data.table()
  # dataCountry <- data.table()
  # dataProtectCountry <- data.table()
  strangeSites <- NULL
  areaAllRegions <- NULL
  areaAllRegionsMan <- NULL
  areaAllRegionsUnman <- NULL
  for(r_no in 1:19){
    
    devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
    source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
    
    areaRegion <- sum(data.all$area,na.rm=T)
    areaRegionMan <- sum(data.all[cons==0]$area,na.rm=T)
    areaRegionUnMan <- sum(data.all[cons==1]$area,na.rm=T)
    areaAllRegions <- c(areaAllRegions,areaRegion)
    areaAllRegionsMan <- c(areaAllRegionsMan,areaRegionMan)
    areaAllRegionsUnman <- c(areaAllRegionsUnman,areaRegionUnMan)
    
    load(paste0(outDyr,"/r_no",r_no,run_settings,"_",harvScen,".rdata"))
    ###update HSIttwo index
    #  datAllScen$BArecD <- datAllScen$DeadWoodVolume/datAllScen$V * datAllScen$BA
    #  datAllScen$HSIttwo <- mapply(HSIttwo,datAllScen$BArecD,datAllScen$V)
    #  save(areas,datAllScen,file=paste0(outDyr,"/r_no",r_no,run_settings,"_",harvScen,".rdata"))
    
    
    if(harvScen=="protect" | cons10run){
      if(cons10run & r_no!=2){
        load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsCons10.rdata"))
        buffDat <- cons10Dat
      }else{
        load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/maakunta_",r_no,"_IDsBuffer.rdata"))  
      }
      
      buffDat <- buffDat[landclass %in% landClassX]
      buffDat$segID <- buffDat$maakuntaID
      setkey(buffDat,segID)
      dataBuff <- which(data.all$segID %in% buffDat$segID)
      xx <- data.all[-dataBuff]
      
      setnames(xx,"N","nPix")
      buffDat$newCons=NULL
      buffDat$oldMaakID=NULL
      xx$Wbuffer=NULL
      buffDat$Wbuffer=NULL
      buffDat[,area:=nPix*16*16/10000]
      
      protect.data.all <- rbind(xx,buffDat)
      areaRegion.protect <- sum(protect.data.all$area,na.rm=T) 
      areaRegionMan <- sum(protect.data.all[cons==0]$area,na.rm=T) * areaRegion/areaRegion.protect
      areaRegionUnMan <- sum(protect.data.all[cons==1]$area,na.rm=T) * areaRegion/areaRegion.protect
      areaAllRegionsMan[r_no] <- areaRegionMan
      areaAllRegionsUnman[r_no] <- areaRegionUnMan
      
      if(harvScen=="protect"){
        datAllScenNorm <- datAllScenProtect
        areas <- areasProtect
      }else{
        datAllScenNorm <- datAllScen
      }
      
      setkey(areas,segID)
      setkey(datAllScenNorm,segID)
      datAllScenNorm <- merge(datAllScenNorm,areas)
      # datAllScenNorm <- merge(datAllScenNorm,data.all[,.(segID,cons)])
      # areas <- merge(areas,protect.data.all[,.(segID,cons)],by="segID")
    }else{
      datAllScenNorm <- datAllScen
      
      setkey(areas,segID)
      setkey(datAllScenNorm,segID)
      datAllScenNorm <- merge(datAllScenNorm,areas)
      # datAllScenNorm <- merge(datAllScenNorm,data.all[,.(segID,cons)])
      # areas <- merge(areas,data.all[,.(segID,cons)],by="segID")
    }
    
    
    ####filter indicators
    datAllScenNorm[HSIcaper<HSIminVal]$HSIcaper <- 0
    datAllScenNorm[HSIhg<HSIminVal]$HSIhg <- 0
    datAllScenNorm[HSIttwo<HSIminVal]$HSIttwo <- 0
    datAllScenNorm[HSIlswo<HSIminVal]$HSIlswo <- 0
    datAllScenNorm[HSIltt<HSIminVal]$HSIltt <- 0
    datAllScenNorm[HSIfs<HSIminVal]$HSIfs <- 0
    
    datAllScenNormMan <- datAllScenNorm[cons==0]
    datAllScenNormUnMan <- datAllScenNorm[cons==1]
    
    areaSample <- sum(areas$area)
    ###use the ratio between managed areas / all forests and unmanagedArea and all forest area from the region instead of the sample 
    areaSampleMan <- sum(areas$area)*areaRegionMan/areaRegion #sum(areas[cons==0]$area) 
    areaSampleUnman <- sum(areas$area)*areaRegionUnMan/areaRegion #sum(areas[cons==1]$area)
    
    # datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
    vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen","harvInten","cons")]
    # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
    datAllScenNorm[, vars] <- 
      datAllScenNorm[ ,lapply(.SD, `*`, area/areaSample), .SDcols = vars]
    datAllScenNormMan[, vars] <- 
      datAllScenNormMan[,lapply(.SD, `*`, area/areaSampleMan), .SDcols = vars]
    datAllScenNormUnMan[, vars] <- 
      datAllScenNormUnMan[,lapply(.SD, `*`, area/areaSampleUnman), .SDcols = vars]
    
    
    meanScen <- 
      datAllScenNorm[ ,lapply(.SD, sum,na.rm=T), .SDcols = vars,by=.(harScen,year,harvInten)]
    meanScenMan <- 
      datAllScenNormMan[ ,lapply(.SD, sum,na.rm=T), .SDcols = vars,by=.(harScen,year,harvInten)]
    meanScenUnman <- 
      datAllScenNormUnMan[ ,lapply(.SD, sum,na.rm=T), .SDcols = vars,by=.(harScen,year,harvInten)]
    
    # meanScenProtect$region=r_no
    meanScen$region=r_no
    meanScenMan$region <- r_no
    meanScenUnman$region <- r_no
    # meanScenProtect$area=areaRegion
    meanScen$area=areaRegion
    meanScenMan$area <- areaRegionMan
    meanScenUnman$area <- areaRegionUnMan
    
    # meanScen <- rbind(meanScen,meanScenProtect)
    meanRegion <- rbind(meanRegion,meanScen)
    meanRegionMan <- rbind(meanRegionMan,meanScenMan)
    meanRegionUnman <- rbind(meanRegionUnman,meanScenUnman)
    print(r_no)  
  }
  
  countryArea <- sum(areaAllRegions)
  countryAreaMan <- sum(areaAllRegionsMan)
  countryAreaUnman <- sum(areaAllRegionsUnman)
  
  meanScenNorm <- meanRegion
  meanScenNormMan <- meanRegionMan
  meanScenNormUnman <- meanRegionUnman
  meanScenNorm[, vars] <- 
    meanRegion[ ,lapply(.SD, `*`, area/countryArea), .SDcols = vars]
  meanScenNormMan[, vars] <- 
    meanRegionMan[ ,lapply(.SD, `*`, area/countryAreaMan), .SDcols = vars]
  meanScenNormUnman[, vars] <- 
    meanRegionUnman[ ,lapply(.SD, `*`, area/countryAreaUnman), .SDcols = vars]
  
  meanCountry <- meanScenNorm[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten)]
  meanCountryMan <- meanScenNormMan[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten)]
  meanCountryUnman <- meanScenNormUnman[ ,lapply(.SD, sum), .SDcols = vars,by=.(harScen,year,harvInten)]
  
  meanCountry$CbalState=0
  meanCountryMan$CbalState=0
  meanCountryUnman$CbalState=0
  
  meanCountry[year %in% 2:max(meanCountry$year)]$CbalState=
    -(meanCountry[year %in% 2:max(meanCountry$year),
                  (WtotTrees+soilC+GVw+Wdb)] -
        meanCountry[year %in% 1:(max(meanCountry$year)-1),
                    (WtotTrees+soilC+GVw+Wdb)])*44/12/1e9*countryArea
  meanCountryMan[year %in% 2:max(meanCountryMan$year)]$CbalState=
    -(meanCountryMan[year %in% 2:max(meanCountryMan$year),
                     (WtotTrees+soilC+GVw+Wdb)] -
        meanCountryMan[year %in% 1:(max(meanCountryMan$year)-1),
                       (WtotTrees+soilC+GVw+Wdb)])*44/12/1e9*countryAreaMan
  meanCountryUnman[year %in% 2:max(meanCountryUnman$year)]$CbalState=
    -(meanCountryUnman[year %in% 2:max(meanCountryUnman$year),
                       (WtotTrees+soilC+GVw+Wdb)] -
        meanCountryUnman[year %in% 1:(max(meanCountryUnman$year)-1),
                         (WtotTrees+soilC+GVw+Wdb)])*44/12/1e9*countryAreaUnman
  
  meanCountry[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)*
                44/12*countryArea/1e9]
  meanCountry[year ==1]$CbalState=NA
  meanCountryMan[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)*
                   44/12*countryAreaMan/1e9]
  meanCountryMan[year ==1]$CbalState=NA
  meanCountryUnman[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)*
                     44/12*countryAreaUnman/1e9]
  meanCountryUnman[year ==1]$CbalState=NA
  
  save(meanCountry,meanRegion,countryArea,
       meanCountryMan,meanRegionMan,countryAreaMan,
       meanCountryUnman,meanRegionUnman,countryAreaUnman,
       file = paste0(outDyr,"/summaryRes/country",
                     run_settings,"_",harvScen,".rdata"))
  
  # dataPlot <- meanCountry[harScen!="adaptTapio"]
  dataPlot <- meanCountry
  pdf(paste0(outDyr,"/summaryRes/plots_country_",harvScen,".pdf"))
  for(varX in vars){
    # i=i+1
    print(ggplot(dataPlot)+
            # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
            geom_line(aes(x = year+ 2015, y = get(varX), color = harScen,linetype=harvInten)) + 
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
          geom_line(aes(x = year+ 2015, y = CbalState, color = harScen,linetype=harvInten)) +
          xlab("year") + ylab("C balance (State)"))
  print(ggplot(dataPlot)+
          # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
          geom_line(aes(x = year+ 2015, y = CbalFluxes, color = harScen,linetype=harvInten)) +
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
  
  pdf(paste0(outDyr,"/summaryRes/plots_ScenariosCountry_",harvScen,".pdf"))
  for(varX in vars){
    for(scenX in scens){
      # i=i+1
      print(ggplot(meanCountry[harScen==scenX])+
              # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
              geom_line(aes(x = year+ 2015, y = get(varX), color = harvInten)) + 
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
  
  write.csv(meanCountry,file=paste0(outDyr,"/summaryRes/MeanCountry_",harvScen,".csv"))
  write.csv(meanRegion,file=paste0(outDyr,"/summaryRes/MeanRegion_",harvScen,".csv"))
  write.csv(meanCountryMan,file=paste0(outDyr,"/summaryRes/MeanCountryMan_",harvScen,".csv"))
  write.csv(meanRegionMan,file=paste0(outDyr,"/summaryRes/MeanRegionMan_",harvScen,".csv"))
  write.csv(meanCountryUnman,file=paste0(outDyr,"/summaryRes/MeanCountryUnman_",harvScen,".csv"))
  write.csv(meanRegionUnman,file=paste0(outDyr,"/summaryRes/MeanRegionUnman_",harvScen,".csv"))
  
  
  #####compare runs +10% cons areas vs. actual situation
  # dat1 <- fread(file=paste0(outDyr,"/summaryRes/MeanCountry_",harvScen,".csv"))
  # dat1$cons = "actual"
  # 
  # dat2 <- fread(file="outSampleHcF1.2_cons10run/plots/MeanCountryAllRuns.csv")
  # dat2$cons = "+10%"
  # datAll <- rbind(dat1,dat2)
  # names(dat2)
  # 
  # scens2 <- scens#[-which(scens%in%c("adaptTapio","NoHarv"))]
  # pdf(paste0(outDyr,"/plots/plots_ScenariosCountry2.pdf"))
  # for(varX in vars){
  #   for(scenX in scens2){
  #     # i=i+1
  #     print(ggplot(datAll[harScen==scenX])+
  #             # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
  #             geom_line(aes(x = year+ 2015, y = get(varX), color = cons,linetype=harvInten)) +
  #             xlab("year") + ylab(varX)+ ggtitle(scenX))
  #   }
  # }
  # dev.off()
  print(which(is.na(meanCountry),arr.ind=T))
  print(which(meanCountry==Inf,arr.ind=T))
  print(which(meanCountry==-Inf,arr.ind=T))
}

