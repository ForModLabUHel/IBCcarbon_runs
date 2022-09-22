if(!exists("minDharvX")) minDharvX <- 999
if(!exists("landClassX")) landClassX <- 1
if(!exists("mortMod")) mortMod <- 3
if(!exists("r_no")) r_no <- 4
if(!exists("sampleID")) sampleID=3
if(!exists("outDyr")) outDyr="outSample"

harvIntensities <- c("Base","MaxSust","Low")

fileName <- paste0(outDyr,"/r_no",r_no,"_clcutArFact",clcutArFact,
                   "_addHarv",compHarvX,"_landClassX",range(landClassX)[1],
                   "to",range(landClassX)[2],"_mortMod",mortMod)

toMem <- ls()

####run Base scenario & intensity
if(harvScen=="Base"){
  harvInten = "Base"
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
  set.seed(1)
  ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  # toMem <- ls()
  modRun <- runModel(sampleID,outType="testRun",forceSaveInitSoil=T,
                     harvScen=harvScen,harvInten=harvInten,compHarvX = compHarvX,
                     cons10run=cons10run,landClassUnman=landClassUnman)
  
  
  reStartMod <- list()
  reStartMod$siteInfo <- modRun$region$siteInfo
  reStartMod$GVout <- modRun$region$GVout
  reStartMod$multiOut <- modRun$region$multiOut
  reStartMod$initClearcut <- modRun$region$initClearcut
  reStartSoil = modRun$region$soilC
  save(reStartMod,reStartSoil,file=paste("restartRun_",r_no,".rdata"))
  toMem <- c(toMem,"reStartSoil","reStartMod")
  region <- modRun$region
  rm(modRun); gc()
  datAll <- data.table()
  segID <- region$siteInfo[,1]
  for(i in 1:length(varSel)){
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
  ####BioIndicators
  bioInd <- calBioIndices(region)
  # HSIcaper
  datX <- data.table(segID=segID,bioInd$HSIcaper)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIcaper"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  # HSIhg
  datX <- data.table(segID=segID,bioInd$HSIhg)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIhg"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  # HSIttwo
  datX <- data.table(segID=segID,bioInd$HSIttwo)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIttwo"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  # HSIlswo
  datX <- data.table(segID=segID,bioInd$HSIlswo)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIlswo"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  # HSIltt
  datX <- data.table(segID=segID,bioInd$HSIltt)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIltt"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  # HSIfs
  datX <- data.table(segID=segID,bioInd$HSIfs)
  setnames(datX,c("segID",1:region$maxYears))
  datX <- melt(datX,"segID")
  setnames(datX,c("variable","value"),c("year","HSIfs"))
  setkey(datX,segID,year)
  setkey(datAll,segID,year)
  datAll <- merge(datAll,datX)
  
  ####put data all together
  datAll$year <- as.numeric(as.character(datAll$year))
  datAll$maakID <- r_no 
  datAll$harScen <- harvScen
  datAll$harvInten <- harvInten
  datAllScen <- datAll
  print(paste0("harvest scenario ", harvScen))
  print(paste0("harvest intensity ", harvInten))
  
  toMem <- c(toMem,"datAllScen")
  rm(list=setdiff(ls(), c(toMem,"toMem"))); gc()
  ###run Base scenario with other intensities
  scens <- "Base"
  # datAllScen <- data.table()
  # toMem <- ls()
  for(harvInten in c("Low","MaxSust")){
    for(harvScen in scens){
      source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
      source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
      nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
      set.seed(1)
      ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
      # toMem <- ls()
      modRun <- runModel(sampleID,outType="testRun",compHarvX = compHarvX,
                         harvScen=harvScen,harvInten=harvInten,
                         cons10run=cons10run,landClassUnman=landClassUnman,
                         outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                         funPreb = reStartRegionPrebas,reStartYear = 7)
      region <- modRun$region
      rm(modRun); gc()
      datAll <- data.table()
      segID <- region$siteInfo[,1]
      for(i in 1:length(varSel)){
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
      ####BioIndicators
      bioInd <- calBioIndices(region)
      # HSIcaper
      datX <- data.table(segID=segID,bioInd$HSIcaper)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIcaper"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIhg
      datX <- data.table(segID=segID,bioInd$HSIhg)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIhg"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIttwo
      datX <- data.table(segID=segID,bioInd$HSIttwo)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIttwo"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIlswo
      datX <- data.table(segID=segID,bioInd$HSIlswo)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIlswo"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIltt
      datX <- data.table(segID=segID,bioInd$HSIltt)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIltt"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIfs
      datX <- data.table(segID=segID,bioInd$HSIfs)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIfs"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      
      datAll$year <- as.numeric(as.character(datAll$year))
      datAll$maakID <- r_no 
      datAll$harScen <- harvScen
      datAll$harvInten <- harvInten
      datAllScen <- rbind(datAllScen,datAll)
      
      print(paste0("harvest scenario ", harvScen))
      print(paste0("harvest intensity ", harvInten))
    }
  }
  print("Base runs completed")
  areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
  save(datAllScen,areas,file=paste0(fileName,"_",harvScen,".rdata"))
  
  Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
  f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
  
  Sys.chmod(list.dirs(outDyr), "0777",use_umask=FALSE)
  f <- list.files(outDyr, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
  
}

if(harvScen=="protect"){
  harvScenTorun <- harvScen
  load(paste("restartRun_",r_no,".rdata"))
  datAllScen <- data.table()
  harvIntenXs <- c("Base","Low","MaxSust","Base")
  harvScenXs <- c("protect","protect","protect","protectTapio")
  # toMem <- ls()
  for(i in 1:4){
    harvInten <- harvIntenXs[i]
    harvScen <- harvScenXs[i]
    
    # for(harvInten in harvIntensities){
    # for(harvScen in scens){
    source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
    source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    set.seed(1)
    ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
    # toMem <- ls()
    modRun <- runModel(sampleID,outType="testRun",compHarvX = compHarvX,
                       harvScen=harvScen,harvInten=harvInten,
                       cons10run=cons10run,landClassUnman=landClassUnman,
                       outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                       funPreb = reStartRegionPrebas,reStartYear = 7)
    region <- modRun$region
    rm(modRun); gc()
    datAll <- data.table()
    segID <- region$siteInfo[,1]
    for(i in 1:length(varSel)){
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
    ####BioIndicators
    bioInd <- calBioIndices(region)
    # HSIcaper
    datX <- data.table(segID=segID,bioInd$HSIcaper)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIcaper"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIhg
    datX <- data.table(segID=segID,bioInd$HSIhg)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIhg"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIttwo
    datX <- data.table(segID=segID,bioInd$HSIttwo)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIttwo"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIlswo
    datX <- data.table(segID=segID,bioInd$HSIlswo)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIlswo"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIltt
    datX <- data.table(segID=segID,bioInd$HSIltt)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIltt"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIfs
    datX <- data.table(segID=segID,bioInd$HSIfs)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIfs"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    datAll$year <- as.numeric(as.character(datAll$year))
    datAll$maakID <- r_no
    datAll$harScen <- harvScen
    datAll$harvInten <- harvInten
    datAllScen <- rbind(datAllScen,datAll)
    
    print(paste0("harvest scenario ", harvScen))
    print(paste0("harvest intensity ", harvInten))
    # }
    # }
  }
  areasProtect <- data.table(segID=region$siteInfo[,1],area=region$areas)
  datAllScenProtect <- datAllScen
  print("protect runs completed")
  save(datAllScenProtect,areasProtect,file=paste0(fileName,"_",harvScenTorun,".rdata"))
  
  Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
  f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
  
  Sys.chmod(list.dirs(outDyr), "0777",use_umask=FALSE)
  f <- list.files(outDyr, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
}


######run adapt and Mitigation
if(harvScen %in% c("adapt","Mitigation")){
  load(paste("restartRun_",r_no,".rdata"))
  datAllScen <- data.table()

  # scens <- c("adapt",
  #          "Mitigation")
# toMem <- ls()
for(harvInten in harvIntensities){
  # for(harvScen in scens){
    source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
    source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    set.seed(1)
    ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
    # toMem <- ls()
    modRun <- runModel(sampleID,outType="testRun",compHarvX = compHarvX,
                       harvScen=harvScen,harvInten=harvInten,
                       cons10run=cons10run,landClassUnman=landClassUnman,
                       outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                       funPreb = reStartRegionPrebas,reStartYear = 7)

    region <- modRun$region
    rm(modRun); gc()
    datAll <- data.table()
    segID <- region$siteInfo[,1]
    for(i in 1:length(varSel)){
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
    ####BioIndicators
    bioInd <- calBioIndices(region)
    # HSIcaper
    datX <- data.table(segID=segID,bioInd$HSIcaper)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIcaper"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIhg
    datX <- data.table(segID=segID,bioInd$HSIhg)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIhg"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIttwo
    datX <- data.table(segID=segID,bioInd$HSIttwo)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIttwo"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIlswo
    datX <- data.table(segID=segID,bioInd$HSIlswo)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIlswo"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIltt
    datX <- data.table(segID=segID,bioInd$HSIltt)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIltt"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    # HSIfs
    datX <- data.table(segID=segID,bioInd$HSIfs)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","HSIfs"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)

    datAll$year <- as.numeric(as.character(datAll$year))
    datAll$maakID <- r_no
    datAll$harScen <- harvScen
    datAll$harvInten <- harvInten
    datAllScen <- rbind(datAllScen,datAll)

    print(paste0("harvest scenario ", harvScen))
    print(paste0("harvest intensity ", harvInten))
  # }
}
areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
print(paste0(harvScen," runs completed"))
save(datAllScen,areas,file=paste0(fileName,"_",harvScen,".rdata"))

Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs(outDyr), "0777",use_umask=FALSE)
f <- list.files(outDyr, all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
}


if(harvScen =="TapioAndNoHarv"){
  harvScenTorun <- harvScen
  load(paste("restartRun_",r_no,".rdata"))
  datAllScen <- data.table()

  scens <- c("NoHarv",
             "adaptTapio","MitigationTapio","baseTapio")
  datAllScen <- data.table()
  # toMem <- ls()
  for(harvInten in "Base"){
    for(harvScen in scens){
      source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
      source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
      nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
      set.seed(1)
      ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
      # toMem <- ls()
      modRun <- runModel(sampleID,outType="testRun",compHarvX = compHarvX,
                         harvScen=harvScen,harvInten=harvInten,
                         cons10run=cons10run,landClassUnman=landClassUnman,
                         outModReStart = reStartMod, initSoilCreStart = reStartSoil,
                         funPreb = reStartRegionPrebas,reStartYear = 7)
      region <- modRun$region
      rm(modRun); gc()
      datAll <- data.table()
      segID <- region$siteInfo[,1]
      for(i in 1:length(varSel)){
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
      ####BioIndicators
      bioInd <- calBioIndices(region)
      # HSIcaper
      datX <- data.table(segID=segID,bioInd$HSIcaper)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIcaper"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIhg
      datX <- data.table(segID=segID,bioInd$HSIhg)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIhg"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIttwo
      datX <- data.table(segID=segID,bioInd$HSIttwo)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIttwo"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIlswo
      datX <- data.table(segID=segID,bioInd$HSIlswo)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIlswo"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIltt
      datX <- data.table(segID=segID,bioInd$HSIltt)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIltt"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      # HSIfs
      datX <- data.table(segID=segID,bioInd$HSIfs)
      setnames(datX,c("segID",1:region$maxYears))
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year","HSIfs"))
      setkey(datX,segID,year)
      setkey(datAll,segID,year)
      datAll <- merge(datAll,datX)
      
      datAll$year <- as.numeric(as.character(datAll$year))
      datAll$maakID <- r_no
      datAll$harScen <- harvScen
      datAll$harvInten <- harvInten
      datAllScen <- rbind(datAllScen,datAll)
      
      print(paste0("harvest scenario ", harvScen))
      print(paste0("harvest intensity ", harvInten))
    }
  }
  areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
  print(paste0(harvScenTorun," runs completed"))
  save(datAllScen,areas,file=paste0(fileName,"_",harvScenTorun,".rdata"))
  
  Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
  f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
  
  Sys.chmod(list.dirs(outDyr), "0777",use_umask=FALSE)
  f <- list.files(outDyr, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)
  
}
  





