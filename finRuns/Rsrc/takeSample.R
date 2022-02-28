library(data.table)

# load("C:/Users/checcomi/Documents/research/IBC-carbon/test/data.all_maakunta_5.rdata")
if(!exists("r_no")) r_no <- 5 
if(!exists("sampleID")) sampleID=3
if(!exists("harvestscenarios")) harvestscenarios <- "Base"

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

age <- data.all$age
ageClass <- seq(0,quantile(data.all$age,0.99)
                ,by=10)

nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
sampleXin <- ops[[sampleID]]
hist(age,freq=0)
hist(ops[[sampleID]]$age,add=T,col=2,freq=0)


for(i in 1:length(ageClass)){
  if (i<length(ageClass)) sampleXin[age %between% c(ageClass[i],ageClass[i+1]),class:=i]
  if (i==length(ageClass)) sampleXin[age > ageClass[i],class:=i]
}

tabX <- sampleXin[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(ageClass) + classNew]



sampleXyoung <- data.table()
nSample <- round(nrow(sampleXin)/length(ageClass))
for(i in 1:length(ageClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  


sampleXuni <- data.table()
nSample <- round(nrow(sampleXin)/length(ageClass))
for(i in 1:length(ageClass)){
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleXin$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleXin$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleXin$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleXin$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)




######BA based

# library(data.table)

baClass <- seq(0,quantile(data.all$ba,0.99)
                ,by=3)

sampleXin <- ops[[sampleID]]
hist(data.all$ba,freq=0)
hist(ops[[sampleID]]$ba,add=T,col=2,freq=0)

for(i in 1:length(baClass)){
  if (i<length(baClass)) sampleXin[ba %between% c(baClass[i],baClass[i+1]),class:=i]
  if (i==length(baClass)) sampleXin[ba > baClass[i],class:=i]
}

tabX <- sampleXin[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(baClass) + classNew]

sampleXyoung <- data.table()
nSample <- round(nrow(sampleXin)/length(baClass))
for(i in 1:length(baClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  


sampleXuni <- data.table()
nSample <- round(nrow(sampleXin)/length(baClass))
for(i in 1:length(baClass)){
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleXin$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleXin$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleXin$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleXin$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)

sampleToRun <- "sampleXuni"
harvestscenarios <- "Base"
scens <- c("Base", "Low", "NoHarv", "MaxSust")
           # "adapt","adaptNoAdH","adaptTapio",
           # "Mitigation","MitigationNoAdH")
           # "protect","protectNoAdH")

for(sampleToRun in c("sampleXuni","sampleXyoung")){
  datAllScen <- data.table()
  sampleXrun <- get(sampleToRun)  
  # setkey(sampleX,segID)
  ####run Base
  for(harvestscenarios in scens){
    # harvestscenarios="Base"
    
    if(harvestscenarios=="Base"){
      outType="testRun"
      sampleX=sampleXrun
      initSoilCin=NA
      source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/runAlternative.r")
      initSoilCin=initSoilC
    }else{
      modRun <- runModelSampleIn(outType="testRun",sampleX=sampleXrun,initSoilC=initSoilC)
    }
    
    region <- modRun$region
    rm(modRun); gc()
    datAll <- data.table()
    segID <- region$siteInfo[,1]
    for(i in 1:length(varSel)){
      # i=2
      datX <- outProcFun(region,varSel[i],funX[i])
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year",varNames[varSel[i]]))
      if(i ==1){
        datAll <- datX
      }else{
        setkey(datX,segID,year)
        setkey(datAll,segID,year)
        datAll <- merge(datAll,datX)
      }
      # print(varNames[varSel[i]])
    }
    ####proc Spec vars
    ###dominant Species
    datX <- domFun(region,varX="species")  
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","domSp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ###age dominant species
    datX <- domFun(region,varX="age")
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","ageDom"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ###deciduous Volume Vdec
    datX <- vDecFun(region)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","Vdec"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####WenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,2],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####VenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","VenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####GVgpp
    datX <- data.table(segID=segID,region$GVout[,,3])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVgpp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####GVw
    datX <- data.table(segID=segID,region$GVout[,,4])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVw"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####Wtot
    datX <- data.table(segID=segID,apply(region$multiOut[,,c(24,25,31,32,33),,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WtotTrees"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    datAll$year <- as.numeric(as.character(datAll$year))
    datAll$maakID <- r_no 
    datAll$harScen <- harvestscenarios
    datAllScen <- rbind(datAllScen,datAll)
    
    print(harvestscenarios)
    # rm(list=setdiff(ls(), c(toMem,"toMem"))); gc()
  }
  
  areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
  
  save(datAllScen,areas,
       file=paste0("outSample/r_no",r_no,"_",sampleToRun,".rdata"))
  
}





######Make plots
library(dplyr)
library(ggplot2)

for(sampleToRun in c("sampleXuni","sampleXyoung")){
  r_no=5
  load(paste0("/scratch/project_2000994/PREBASruns/finRuns/outSample/r_no"
              ,r_no,"_",sampleToRun,".rdata"))
  
  datAllScenNorm <- datAllScen
  # datAllScenNormProtect <- datAllScenProtect
  setkey(areas,segID)
  setkey(datAllScenNorm,segID)
  # setkey(areasProtect,segID)
  # setkey(datAllScenNormProtect,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas)
  # datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen")]
  # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  # datAllScenNormProtect[, vars] <- 
    # datAllScenNormProtect[ ,lapply(.SD, `*`, area*length(areasProtect$area)/sum(areasProtect$area)), .SDcols = vars]
  
  plot.list <- list()
  i=0
  for(varX in vars){
    i=i+1
    sumryX <- datAllScenNorm %>%   
      group_by(year, harScen) %>%
      summarise(medi = median(get(varX),na.rm=T),
                q0.25 = quantile(get(varX),probs=0.25,na.rm=T),
                q0.75 = quantile(get(varX),probs=0.75,na.rm=T))
    # 
    # sumryXProtect <- datAllScenNormProtect %>%   
      # group_by(year, harScen) %>%
      # summarise(medi = median(get(varX),na.rm=T),
      #           q0.25 = quantile(get(varX),probs=0.25,na.rm=T),
      #           q0.75 = quantile(get(varX),probs=0.75,na.rm=T))
    
    # sumryX <- rbind(sumryX,sumryXProtect)
    plot.list[[i]] <- ggplot(sumryX)+
      geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen)) +
      xlab("year") + ylab(varX)
    
    i=i+1
    
    plot.list[[i]] <- ggplot(sumryX)+
      # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen)) + 
      xlab("year") + ylab(varX)
  }
  
  pdf(paste0("outSample/plots",r_no,"_",sampleToRun,".pdf"))
  for(i in 1:length(plot.list)) print(plot.list[[i]])
  dev.off()
}


