r_no=5
sampleID=3
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
# harvestscenarios <- "Base"
scens <- c("Base", "Low", "NoHarv", "MaxSust",
    "protect","protectNoAdH",
    "adapt","adaptNoAdH","adaptTapio",
    "Mitigation","MitigationNoAdH")

datAllScen <- data.table()
toMem <- ls()

for(harvestscenarios in scens){

  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)

  set.seed(1)
  ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  
  toMem <- ls()
  
  modRun <- runModel(sampleID,outType="testRun")
    
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
save(datAllScen,areas, file=paste0("outSample/r_no",r_no,".rdata"))
