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
setwd("/scratch/project_2000994/PREBASruns/finRuns")
run_settings <- paste0(#"_clcutArFact",clcutArFact,
          "_addHarv",compHarvX,"_landClassX",range(landClassX)[1],
          "to",range(landClassX)[2],"_mortMod",mortMod)

regionNames <- fread("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_names.txt")

for(r_no in 1:19){
  # load(paste0("/scratch/project_2000994/PREBASruns/finRuns/outSample/r_no",r_no,".rdata"))
  load(paste0(outDyr,"/r_no",r_no,run_settings,".rdata"))
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  regArea <- sum(data.all$area)
  simYears <- max(datAllScen$year)
  
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
  
  
  # CBal
  datAllScenNorm$CbalState=0
  datAllScenNorm[year %in% 2:simYears]$CbalState=
    -(datAllScenNorm[year %in% 2:simYears,
                     (WtotTrees+soilC+GVw)] -
        datAllScenNorm[year %in% 1:(simYears-1),
                       (WtotTrees+soilC+GVw)])#*44/12/1e9*regArea
  datAllScenNorm[year %in% 1]$CbalState=NA
  
  datAllScenNorm[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)]#*
  # 44/12*regArea/1e9]
  
  datAllScenNormProtect$CbalState=0
  datAllScenNormProtect[year %in% 2:simYears]$CbalState=
    -(datAllScenNormProtect[year %in% 2:simYears,
                            (WtotTrees+soilC+GVw)] -
        datAllScenNormProtect[year %in% 1:(simYears-1),
                              (WtotTrees+soilC+GVw)])#*44/12/1e9*regArea
  datAllScenNormProtect[year %in% 1]$CbalState=NA
  
  datAllScenNormProtect[,CbalFluxes:=(-NEP*10+WenergyWood+WroundWood)]#*
  # 44/12*regArea/1e9]
  # vars <- c("CbalFluxes","CbalState",vars)
  
  plot.list <- list()
  i=0
  for(varX in c("CbalState","CbalFluxes")){
    i=i+1
    sumryX <- datAllScenNorm[,.(median(get(varX),na.rm=T),
                                mean(get(varX),na.rm=T),
                                quantile(get(varX),probs=0.25,na.rm=T),
                                quantile(get(varX),probs=0.75,na.rm=T)),
                             by=.(year, harScen,harvInten)]
    setnames(sumryX,c("V1","V2","V3","V4"),
             c("medi","mean","q0.25","q0.75"))
    
    sumryXProtect <- datAllScenNormProtect[,
                                           .(median(get(varX),na.rm=T),
                                             mean(get(varX),na.rm=T),
                                             quantile(get(varX),probs=0.25,na.rm=T),
                                             quantile(get(varX),probs=0.75,na.rm=T)),
                                           by=.(year, harScen,harvInten)]
    setnames(sumryXProtect,c("V1","V2","V3","V4"),
             c("medi","mean","q0.25","q0.75"))
    
    sumryX <- rbind(sumryX,sumryXProtect)
    
    plot.list[[i]] <- ggplot(sumryX)+
      # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen,linetype=harvInten)) +
      geom_line(aes(x = year+ 2016, y = mean, color = harScen,linetype=harvInten)) +
      xlab("year") + ylab(varX)
    
    print(varX)
  }
  # funX <-c("NEP","GPPtrees","npp","grossGrowth",
  #          "soilC","V","age","WroundWood",
  #          "VroundWood","Litter_fol","Litter_fr","Litter_fWoody", 
  #          "Litter_cWoody","DeadWoodVolume","D","BA",
  #          "H","Vmort","domSp","ageDom", 
  #          "Vdec","WenergyWood","VenergyWood","GVgpp",
  #          "GVw","WtotTrees" )
  for(varX in vars){
    i=i+1
    sumryX <- datAllScenNorm[,.(median(get(varX),na.rm=T),
                                mean(get(varX),na.rm=T),
                                quantile(get(varX),probs=0.25,na.rm=T),
                                quantile(get(varX),probs=0.75,na.rm=T)),
                             by=.(year, harScen,harvInten)]
    setnames(sumryX,c("V1","V2","V3","V4"),
             c("medi","mean","q0.25","q0.75"))
    
    sumryXProtect <- datAllScenNormProtect[,
                                           .(median(get(varX),na.rm=T),
                                             mean(get(varX),na.rm=T),
                                             quantile(get(varX),probs=0.25,na.rm=T),
                                             quantile(get(varX),probs=0.75,na.rm=T)),
                                           by=.(year, harScen,harvInten)]
    setnames(sumryXProtect,c("V1","V2","V3","V4"),
             c("medi","mean","q0.25","q0.75"))
    
    sumryX <- rbind(sumryX,sumryXProtect)
    
    plot.list[[i]] <- ggplot(sumryX)+
      # ggplot(sumryX[harScen=="Base"])+
      geom_line(aes(x = year+ 2016, y = q0.25, color = harScen,linetype=harvInten)) +
      geom_line(aes(x = year+ 2016, y = q0.75, color = harScen,linetype=harvInten)) +
      geom_line(aes(x = year+ 2016, y = medi, color = harScen,linetype=harvInten)) +
      xlab("year") + ylab(varX)
    
    i=i+1
    
    plot.list[[i]] <- ggplot(sumryX)+
      # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen,linetype=harvInten)) +
      geom_line(aes(x = year+ 2016, y = mean, color = harScen,linetype=harvInten)) +
      xlab("year") + ylab(varX)
    
    print(varX)
  }
  
  pdf(paste0(outDyr,"/plots",r_no,".pdf"))
  for(i in 1:length(plot.list)) print(plot.list[[i]])
  dev.off()
  
}
