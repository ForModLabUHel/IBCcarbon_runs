library(dplyr)
library(ggplot2)
library(data.table)

# load("C:/Users/checcomi/Documents/research/IBC-carbon/test/data.all_maakunta_5.rdata")
if(!exists("r_no")) r_no <- 4
if(!exists("sampleID")) sampleID=5
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
  nSample <- as.numeric(tabX[classNew==i]$N)
  if(i<length(ageClass)) sampleNew <- 
      data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- 
      data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  

sampleXuni <- data.table()
nSample <- round(nrow(sampleXin)/length(ageClass))
for(i in 1:length(ageClass)){
  if(i<length(ageClass)) sampleNew <- 
      data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- 
      data.all[age > ageClass[i]][sample(nSample,replace = T)]
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

varSel <- c(varSel,14)
funX <- c(funX,"baWmean")
sampleToRun <- "sampleXin"
harvestscenarios <- "Base"
scens <- c("Base", "NoHarv", "Low", "MaxSust")
           # "adapt","adaptNoAdH","adaptTapio",
           # "Mitigation","MitigationNoAdH")
           # "protect","protectNoAdH")

for(sampleToRun in c("sampleXuni","sampleXyoung","sampleXin")){
  datAllScen <- data.table()
  sampleXrun <- get(sampleToRun)
  # setkey(sampleXrun,NULL)
  
  ####run Base
  for(harvestscenarios in scens){
    # harvestscenarios="NoHarv"
    
    if(harvestscenarios=="Base"){
      modRun <- runModelSampleIn(outType="testRun",sampleX=sampleXrun,initSoilC=NA)
      initSoilC=modRun$initSoilC
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
        datAll <- cbind(datAll, datX[,3])
      }
      print(i)
    }
    ####proc Spec vars
    ###dominant Species
    datX <- domFun(region,varX="species")  
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","domSp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ###age dominant species
    datX <- domFun(region,varX="age")
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","ageDom"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ###deciduous Volume Vdec
    datX <- vDecFun(region)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","Vdec"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ####WenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,2],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ####VenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","VenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ####GVgpp
    datX <- data.table(segID=segID,region$GVout[,,3])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVgpp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ####GVw
    datX <- data.table(segID=segID,region$GVout[,,4])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVw"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    ####Wtot
    datX <- data.table(segID=segID,apply(region$multiOut[,,c(24,25,31,32,33),,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WtotTrees"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- cbind(datAll, datX[,3])
    
    datAll$year <- as.numeric(as.character(datAll$year))
    datAll$maakID <- r_no 
    datAll$harScen <- harvestscenarios
    datAllScen <- rbind(datAllScen,datAll)
    
    print(harvestscenarios)
    # rm(list=setdiff(ls(), c(toMem,"toMem"))); gc()
  }
  
  areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
  
  
  fileName = paste0("outSample/r_no",r_no,"_",sampleToRun)
  if(exists("addFileName")) fileName = paste0(fileName,"_",addFileName)
  save(datAllScen,areas,
       file=paste0(fileName,".rdata"))

}


