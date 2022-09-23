
## ---------------------------------------------------------------------
## FUNCTIONS
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## MAIN SCRIPT: uncRun for random segments, uncSeg for random values for segments
## ---------------------------------------------------------------------
runModel <- function(sampleID, outType="dTabs",
                     harvScen,harvInten,easyInit=FALSE,
                     forceSaveInitSoil=F, cons10run = F,
                     procDrPeat=F,coeffPeat1=-240,coeffPeat2=70,
                     coefCH4 = 0.34,#g m-2 y-1
                     coefN20_1 = 0.23,coefN20_2 = 0.077,#g m-2 y-1
                     landClassUnman=NULL,compHarvX = 0,
                     funPreb = regionPrebas,
                     initSoilCreStart=NULL,
                     outModReStart=NULL,reStartYear=1){
  # outType determines the type of output:
  # dTabs -> standard run, mod outputs saved as data.tables 
  # testRun-> test run reports the mod out and initPrebas as objects
  # ststDeadW -> initialize the dead Wood volume;
  # uncRun -> reports the output table for the regional uncertainty run
  # uncSeg -> reports the list of output table for the segment uncertainty run
  # cons10run -> flag for conservation areas 10% run
  
  # print(date())
  print(paste("start sample ID",sampleID))
  mortModFlag <- mortMod  
  ###flag for soil initialization
  if(is.null(initSoilCreStart)){
    initilizeSoil=T
    initSoilC <- NULL
  }else{
    initilizeSoil=F
    initSoilC <- initSoilCreStart[,1,,,]
  }
   
  procInSample=F
  ####in the protection scenarios consider buffer to protection areas
  ####if cons10run == TRUE run the model considering 10% area is conservation area according to zonation results
  if(harvScen %in% c("protect","protectNoAdH","protectTapio") & cons10run==FALSE ){
    # sampleX$cons[sampleX$Wbuffer==1] <- 1
    load(paste0("input/maakunta/maakunta_",r_no,"_IDsBuffer.rdata"))
    xDat <- buffDat
    procInSample = T
    initilizeSoil = F
  }
  if(cons10run){
    load(paste0("input/maakunta/maakunta_",r_no,"_IDsCons10.rdata"))
    xDat <- cons10Dat
    procInSample = T
    initilizeSoil = F
  }
  if(procInSample){
    if(is.null(initSoilC)){
      if(identical(landClassX,1:3)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to3.rdata"))
      if(identical(landClassX,1:2)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to2.rdata"))
      if(identical(landClassX,1)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1.rdata"))
    }
    setnames(xDat,"nPix","N")
    xDat[,area:=N*16^2/10000]
    
    # setkey(ops[[sampleID]],maakuntaID)
    # setkey(xDat,maakuntaID)
    posX <- which(ops[[sampleID]]$maakuntaID %in% xDat$maakuntaID)
    maakX <- ops[[sampleID]][posX]$maakuntaID
    myXdat <- semi_join(xDat[maakuntaID %in% maakX],ops[[sampleID]],by="maakuntaID")
    for(ijf in 1:nrow(myXdat)){
      ops[[sampleID]][maakuntaID ==myXdat$maakuntaID[ijf]]$area <- myXdat$area[ijf]
      ops[[sampleID]][maakuntaID ==myXdat$maakuntaID[ijf]]$N <- myXdat$N[ijf]
    }
    
    selX <- xDat[!maakuntaID %in% maakX &
                   oldMaakID %in% maakX]
    ops[[sampleID]][,oldMaakID:=maakuntaID]
    
    selX$newCons <- NULL
    selX$Wbuffer <- NULL
    ops[[sampleID]]$Wbuffer <- NULL
    
    sampleX <- rbind(ops[[sampleID]],selX)
    sampleX$segID <- sampleX$maakuntaID
    x0 <- which(sampleX$N==0)    
    sampleX <- sampleX[-x0]
    segIDs <- sampleX$segID
    if(is.null(initSoilCreStart) | 
       (harvScen %in% c("protect","protectNoAdH","protectTapio") & 
        !cons10run)){
      initSoilC <- abind(initSoilC,initSoilC[posX,,,],along=1)
      initSoilC <- initSoilC[-x0,,,]
      
      if(!is.null(initSoilCreStart)){
        initSoilCreStart <- abind(initSoilCreStart,initSoilCreStart[posX,,,,],along=1)
        initSoilCreStart <- initSoilCreStart[-x0,,,,]
      }
      if(!is.null(outModReStart)){
        outModReStart$multiOut <- abind(outModReStart$multiOut,outModReStart$multiOut[posX,,,,],along=1)
        outModReStart$multiOut <- outModReStart$multiOut[-x0,,,,]
        outModReStart$GVout <- abind(outModReStart$GVout,outModReStart$GVout[posX,,],along=1)
        outModReStart$GVout <- outModReStart$GVout[-x0,,]
        outModReStart$siteInfo <- abind(outModReStart$siteInfo,outModReStart$siteInfo[posX,],along=1)
        outModReStart$siteInfo <- outModReStart$siteInfo[-x0,]
        outModReStart$siteInfo[,1] <- segIDs
        outModReStart$multiOut[,,1,,1] <- array(segIDs,dim=dim(outModReStart$multiOut[,,1,,1])) 
        outModReStart$initClearcut <- abind(outModReStart$initClearcut,outModReStart$initClearcut[posX,],along=1)
        outModReStart$initClearcut <- outModReStart$initClearcut[-x0,]
      }
    }
    
    # data.all <- rbind(data.all[!maakuntaID %in% xDat$maakuntaID],xDat)
  }else{
    sampleX <- ops[[sampleID]]
  }
  
  if(outType=="uncRun"){
    area_tot <- sum(data.all$area) # ha
    sampleX[,area := 16^2/10000] 
    cA <- 1/nrow(sampleX) #area_tot/nrow(sampleX) 
    harvestLims <- harvestLimsr[sampleID,]
    print(paste("sampleID",sampleID,"harvestLims ="))
    print(harvestLims)
  } else {
    sampleX[,area := N*16^2/10000] 
  }
  sampleX[,id:=climID]
  HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
  nSample = nrow(sampleX)#200#nrow(data.all)
  
  # leave unmaned land classes in landClassUnman
  if(!is.null(landClassUnman)) sampleX[landclass %in% landClassUnman]$cons=1
  
  ## ---------------------------------------------------------
  i = 0
  rcpfile = rcps
  #if(outType != "uncRun"){
  if(!outType %in% c("uncRun","uncSeg")){
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
    if(rcpfile=="CurrClim"){
      #####process data considering only current climate###
      # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
      maxRday <- max(dat$rday)
      xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
      dat = rbind(dat,dat,dat)
      dat[,rday:=xday]
    }
  }
  ## Loop regions -------------------------------------------------------
  # for (r_no in regions) {
  # print(date())
  # print(paste("Region", r_no) )
  # r_no=7
  ## Load samples from regions; region-files include every 1000th pixel
  ## Pixel data are from 16 m x 16 m cells, but all numbers are per unit area.
  ## Model also produces per values  per hectar or m2.
  ## Note also that some of the pixels are non-forest (not metsamaa, kitumaa, joutomaa)
  ## or not inside Finland (32767) or may be cloudcovered (32766).
  
  # data.all = fread(paste(regiondatapath, "data.proc.", r_no, ".txt", sep=""))
  # data.all = fread(paste("data.proc.", r_no, ".txt",sep=""))
  # dat = dat[id %in% data.all[, unique(id)]]
  gc()
  ## Prepare the same initial state for all harvest scenarios that are simulated in a loop below
  data.sample = sample_data.f(sampleX, nSample)
  if(rcpfile=="CurrClim") data.sample$id <- data.sample$CurrClimID
  areas <- data.sample$area
  totAreaSample <- sum(data.sample$area)
  
  clim = prep.climate.f(dat, data.sample, startingYear, nYears)
  
  Region = nfiareas[ID==r_no, Region]
  
  ###set parameters
  # if(outType %in% c("uncRun","uncSeg")){
  HcFactor <- 1
  if(outType %in% c("uncRun","uncSeg")){
    pCrobasX <- pCROBASr[[sampleID]]
    pPRELES <- as.numeric(pPRELr[sampleID,])
    pYAS <- as.numeric(pYASr[sampleID,])
    HcFactor <- HcFactorr[sampleID] 
    print(paste("sampleID",sampleID,"HcFactor =",HcFactor))
  }
  ## Second, continue now starting from soil SS
  initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                     startingYear = startingYear,domSPrun=domSPrun,
                                     harv=harvScen, HcFactorX=HcFactor,
                                     initSoilC=initSoilCreStart,
                                     outModReStart=outModReStart)
  
  if(outType %in% c("uncRun","uncSeg")){
    initPrebas$pPRELES <- pPRELES
    initPrebas$pYASSO <- pYAS
  }
  
  opsna <- which(is.na(initPrebas$multiInitVar))
  initPrebas$multiInitVar[opsna] <- 0.
  
  ##### if the mortality model flag is 13 uses 
  ##### mortMod=1 (reineke) for managed forests
  ##### mortMod=3 (reineke + empirical model) for unmanaged forests
  if(mortMod==13){
    initPrebas$mortMod <- c(1,3)#rep(1,dim(initPrebas$multiOut)[1])
    # initPrebas$mortMod[initPrebas$ClCut==0] <- 3
    mortModFlag=3
  }
  
  
  ### for adapt and protect scenario Replanting schemes 
  ### do not replant pine in sitetypes 1 and 2
  ### do not replant spruce in sitetypes higher than 3
  ### ensure minimum 20% birch at replanting
  if(harvScen %in% c("adapt","protect","protectNoAdH","protectTapio",
                     "adaptNoAdH","adaptTapio")){
    sitesXs <- which(initPrebas$siteInfo[,3]>3)
    jj <- which(initPrebas$initCLcutRatio[sitesXs,2]>0.)
    initPrebas$initCLcutRatio[sitesXs[jj],2] <- 0.
    
    sitesXs <- which(initPrebas$siteInfo[,3]<3)
    jj <- which(initPrebas$initCLcutRatio[sitesXs,1]>0.)
    initPrebas$initCLcutRatio[sitesXs[jj],1] <- 0.
    
    xx <- 1/rowSums(initPrebas$initCLcutRatio)
    initPrebas$initCLcutRatio <- sweep(initPrebas$initCLcutRatio,MARGIN = 1,xx, `*`)
    jj <- which(is.na(rowSums(initPrebas$initCLcutRatio)))
    initPrebas$initCLcutRatio[jj,] <- 0.
    initPrebas$initCLcutRatio[jj,3] <- 1
    jj <- which(initPrebas$initCLcutRatio[,3]<0.2)
    xx <- 1-(0.2 - initPrebas$initCLcutRatio[jj,3])
    initPrebas$initCLcutRatio[jj,1:2] <- 
      sweep(initPrebas$initCLcutRatio[jj,1:2],MARGIN=1,xx, `*`)
    initPrebas$initCLcutRatio[jj,3] <- 0.2
  }
  
  ##here mix years for weather inputs for Curr Climate
  if(rcpfile=="CurrClim"){
    #if(outType=="uncRun"){
    if(outType %in% c("uncRun","uncSeg")){
      resampleYear <- resampleYears[sampleID,] 
      #sample(1:nYears,nYears,replace=T)
    }else{
      set.seed(10)
      resampleYear <- sample(1:nYears,nYears)
    } 
    initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
    initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
    initPrebas$weather <- initPrebas$weather[,resampleYear,,]
    initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
  }
  
  
  # Loop management scenarios ------------------------------------------------
  # for(harvScen in harvScen) { ## MaxSust fails, others worked.
  # print(date())
  # print(harvScen)
  i = i + 1
  # print(paste(i, (length(harvScen)*length(rcps)*length(regions)), sep="/"))
  # harvScen ="Base"
  
  ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
  if(regSets!="maakunta"){
    Region = nfiareas[ID==r_no, Region]
    if(harvScen=="NoHarv"){
      initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      HarvLim1 = 0
      harvInten = "NoHarv"
    }else if(harvScen=="Tapio"){
      HarvLim1 = 0
    }else{
      HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "1990-2013"]
      HarvLim0  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "2015-2024"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- rep(as.numeric(HarvLim),10)
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "2025-2034"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "2035-2044"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "2045-2054"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harvScen & Area == Region, "2055-2064"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),44))
    }
    ## In the model, harvests are always per hectar units. If 1000 pixels (nSample)
    ## are simulated it corresponds to 1000 hectars, although pixels are only 16x16 m2.
    ## Therefore, we need to apply the areal fraction of removals scenarios
    ## nfiareas are in 1000 ha, model takes Harvlim in m3, while removals from Mela are 1000 m3
    #      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
    if(year1harv==1){
      HarvLim1 <- HarvLimX
      if(harvInten == "Low"){ HarvLim1 <- HarvLimX * 0.6}
      if(harvInten == "MaxSust"){HarvLim1 <- HarvLimX * 1.2}
      if(harvScen == "NoHarv"){
        HarvLim1 <- HarvLimX * 0.
        initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
        harvInten = harvScen
      }
    }else{
      roundWood <- HarvLim1 * roundTotWoodRatio
      enWood <- HarvLim1 - roundWood
      HarvLim1 <- cbind(roundWood,enWood)
    }
  }else{
    HarvLim1 <- HarvLimMaak*1000*sum(areas)/sum(data.all$area)
    if(harvInten == "Low"){ HarvLim1 <- HarvLim1 * 0.6}
    if(harvInten == "MaxSust"){HarvLim1 <- HarvLim1 * 1.2}
    if(harvScen == "NoHarv"){
      HarvLim1 <- HarvLim1 * 0.
      initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      harvInten = harvScen
    }
  }          
  
  ###calculate clearcutting area for the sample
  #if(!is.na(cutArX)){
  print("calculating clearcutting areas")
  clcutArX <- clcutAr * sum(areas)/sum(data.all$area)
  clcutArX <- cbind(clcutArX[1:nYears],0.)
  tendX <- tendingAr * sum(areas)/sum(data.all$area)
  tendX <- cbind(tendX[1:nYears],0.)
  fThinX <- firstThinAr * sum(areas)/sum(data.all$area)
  fThinX <- cbind(fThinX[1:nYears],0.)
  cutArX <- cbind(clcutArX,tendX)
  cutArX <- cbind(cutArX,fThinX)
  if(harvInten == "Low"){ cutArX <- cutArX * 1}
  if(harvInten == "MaxSust"){cutArX <- cutArX * 1.2}
  if(harvScen == "NoHarv"){cutArX <- cutArX * 0.}
  
  # }else{
  #   cutArX <- NA
  # } 
  # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
  # HarvLim1 <- rep(0,2)
  # save(initPrebas,HarvLim1,file=paste0("test1",harvScen,".rdata"))
  # region <- regionPrebas(initPrebas)
  ###run PREBAS
  if(initilizeSoil){
    if(!(harvScen =="Base" & harvInten == "Base")){
      if(outType!="uncRun"){
        if(!harvScen %in% c("protect","protectNoAdH","protectTapio")){
          if(is.null(initSoilC)){
            if(identical(landClassX,1:3)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to3.rdata"))
            if(identical(landClassX,1:2)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to2.rdata"))
            if(identical(landClassX,1)) load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1.rdata"))
          }
        }
      }else{ # if UncRun or uncSeg
        load(paste0("initSoilCunc/forCent",r_no,"/initSoilC_",outType,"_",sampleID,".rdata"))
      }
      # initPrebas$yassoRun <- rep(1,initPrebas$nSites)
      # initPrebas$soilC[,1,,,] <- initSoilC
    }
  }
  initPrebas$yassoRun <- rep(1,initPrebas$nSites)
  nx <- dim(initSoilC)[3]
  if(!is.null(initSoilC)) initPrebas$soilC[,1,,,1:nx] <- initSoilC
  
  print(paste0("harvest scenario ", harvScen))
  print(paste0("harvest intensity ", harvInten))
  HarvLimX <- HarvLim1[1:nYears,]
  
  if(harvScen %in% c("adapt","adaptNoAdH","adaptTapio")){
    if(harvScen=="adaptNoAdH"){
      compHarvX=0.
    }
    ###set parameters to decrease rotation length of 25% (start)
    load(paste0("input/",regSets,"/pClCut_adapt/ClCutplots_maak",r_no,".rdata"))
    ClcutX <- updatePclcut(initPrebas,pClCut)
    initPrebas$inDclct <- ClcutX$inDclct
    initPrebas$inAclct <- ClcutX$inAclct
    initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
    ###set parameters to decrease rotation length of 25% (end)
    if(harvScen=="adaptTapio"){
      region <- funPreb(initPrebas,compHarv=compHarvX,
                    fertThin = fertThin,nYearsFert = nYearsFert,
                     startSimYear=reStartYear)
    }else{
      region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                             cutAreas = cutArX,compHarv=compHarvX,
                             fertThin = fertThin,nYearsFert = nYearsFert,
                     startSimYear=reStartYear)
    }
  }else if(harvScen %in% c("Mitigation","MitigationNoAdH","MitigationTapio")){
    if(harvScen=="MitigationNoAdH"){
      compHarvX=0.
    }
    thinFact = 0.2 ##thinning factor used in the additional harvests to compensate
    HarvLimX[,2]=0.
    initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
###set parameters to increase rotation length of 25% (start)
    load(paste0("input/",regSets,"/pClCut_mitigation/ClCutplots_maak",r_no,".rdata"))
    ClcutX <- updatePclcut(initPrebas,pClCut)
    initPrebas$inDclct <- ClcutX$inDclct
    initPrebas$inAclct <- ClcutX$inAclct
    initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
###set parameters to increase rotation length of 25% (end)
    if(harvScen=="MitigationTapio"){
      region <- funPreb(initPrebas,compHarv=compHarvX,
                     startSimYear=reStartYear,thinFact=thinFact)
    }else{
      region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                             cutAreas =cutArX,compHarv=compHarvX,
                             ageHarvPrior = ageHarvPriorX,
                     startSimYear=reStartYear,thinFact=thinFact)
    }
  }else if(harvScen %in% c("protect","protectNoAdH","protectTapio")){
    if(harvScen=="protectNoAdH"){
      compHarvX=0.
    }
    thinFact = 0.2 ##thinning factor used in the additional harvests to compensate
    ####no energy cuts
    HarvLimX[,2]=0.
    initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
    
    ###set parameters to increase rotation length of 25% (start)
    load(paste0("input/",regSets,"/pClCut_mitigation/ClCutplots_maak",r_no,".rdata"))
    ClcutX <- updatePclcut(initPrebas,pClCut)
    initPrebas$inDclct <- ClcutX$inDclct
    initPrebas$inAclct <- ClcutX$inAclct
    initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
    ###set parameters to increase rotation length of 25% (end)
    
    if(harvScen=="protectTapio"){
      region <- funPreb(initPrebas,
                           compHarv=compHarvX,oldLayer = 1,
                     startSimYear=reStartYear,thinFact=thinFact)
    }else{
      region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                           cutAreas =cutArX,compHarv=compHarvX,
                           ageHarvPrior = ageHarvPriorX,
                           oldLayer = 1,thinFact=thinFact,
                     startSimYear=reStartYear)
    }
  }else{
    if(harvScen=="baseTapio"){
      region <- funPreb(initPrebas,compHarv=compHarvX,
                     startSimYear=reStartYear)
    }else{
      ##Don't pass minDharvX if NA
      if (is.na(minDharvX)) {
        region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                               cutAreas =cutArX,compHarv=compHarvX,
                       startSimYear=reStartYear)
      } else {
        region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                               minDharv = minDharvX,cutAreas =cutArX,
                               compHarv=compHarvX,
                       startSimYear=reStartYear)
      }
      
    }
  }
  
  print(paste("runModel",sampleID,"completed"))
  ##calculate steady state carbon from prebas litter 
  if(harvScen=="Base" & harvInten =="Base" & initilizeSoil){
    initSoilC <- stXX_GV(region, 1)
    print(paste("initSoilC",sampleID))
    if(outType!="testRun" | forceSaveInitSoil){
      if(!outType %in% c("uncRun","uncSeg")){
        if(identical(landClassX,1:3)) save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to3.rdata"))
        if(identical(landClassX,1:2)) save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1to2.rdata"))
        if(identical(landClassX,1)) save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,"_LandClass1.rdata"))
      } else {
        save(initSoilC,file=paste0("initSoilCunc/forCent",r_no,"/initSoilC_",outType,"_",sampleID,".rdata"))
      }
    }
    ###run yasso (starting from steady state) using PREBAS litter
    # region <- yassoPREBASin(region,initSoilC)
    initPrebas$yassoRun <- rep(1,initPrebas$nSites)
    initPrebas$soilC[,1,,,] <- initSoilC
    if (is.na(minDharvX)) {
      region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                             cutAreas =cutArX,compHarv=compHarvX,
                     startSimYear=reStartYear)
    } else {
      region <- funPreb(initPrebas, HarvLim = as.numeric(HarvLimX),
                             minDharv = minDharvX,cutAreas =cutArX,
                             compHarv=compHarvX,
                     startSimYear=reStartYear)
    }
    # out <- region$multiOut[,,,,1]
  }
  print(paste("all runs done",sampleID))
  
  #####process drained Peat
  if(procDrPeat){
    siteDrPeat1 <- which(sampleX$pseudoptyp==400 & region$siteInfo[,3]<4)
    siteDrPeat2 <- which(sampleX$pseudoptyp==400 & region$siteInfo[,3]>=4)
    
    ###CH4 <- N20
    # converts coeef to ha
    coefCH4 = coefCH4/1000*10000 #g m-2 y-1 -> kg ha-1
    coefN20_1 = coefN20_1/1000*10000 #g m-2 y-1 -> kg ha-1
    coefN20_2 = coefN20_2/1000*10000 #g m-2 y-1 -> kg ha-1
    region$CH4emisDrPeat_kgyear = coefCH4*region$areas[siteDrPeat1] +
      coefCH4*region$areas[siteDrPeat2]
    region$N2OemisDrPeat_kgyear = coefN20_1*region$areas[siteDrPeat1] +
      coefN20_2*region$areas[siteDrPeat2]
    
    region$multiOut[siteDrPeat1,,46,,1] = 0.
    region$multiOut[siteDrPeat1,,46,,1] = region$multiOut[siteDrPeat1,,18,,1] - 
      region$multiOut[siteDrPeat1,,26,,1]/10 - region$multiOut[siteDrPeat1,,27,,1]/10 - 
      region$multiOut[siteDrPeat1,,28,,1]/10 - region$multiOut[siteDrPeat1,,29,,1]/10
    region$multiOut[siteDrPeat1,,46,1,1] = region$multiOut[siteDrPeat1,,46,1,1] + 
      coeffPeat1 +  region$GVout[siteDrPeat1,,5]
    
    region$multiOut[siteDrPeat2,,46,,1] = 0.
    region$multiOut[siteDrPeat2,,46,,1] = region$multiOut[siteDrPeat2,,18,,1] - 
      region$multiOut[siteDrPeat2,,26,,1]/10 - region$multiOut[siteDrPeat2,,27,,1]/10 - 
      region$multiOut[siteDrPeat2,,28,,1]/10 - region$multiOut[siteDrPeat2,,29,,1]/10
    region$multiOut[siteDrPeat2,,46,1,1] = region$multiOut[siteDrPeat2,,46,1,1] + 
      coeffPeat2 +  region$GVout[siteDrPeat2,,5]
  }
  #####start initialize deadWood volume
  ## identify managed and unmanaged forests
  manFor <-  which(sampleX$cons==0)
  unmanFor <- which(sampleX$cons==1)
  if(outType=="ststDeadW"){
    unmanDeadW <- initDeadW(region,unmanFor,yearsDeadW)
    manDeadW <- initDeadW(region,manFor,yearsDeadW)
    save(unmanDeadW,manDeadW,file=paste0("initDeadWVss/reg",
                                         r_no,"_deadWV_mortMod",mortMod,".rdata"))
    return("deadWood volume at steady state saved")
  }else{
    load(paste0("initDeadWVss/reg",
                r_no,"_deadWV_mortMod",mortModFlag,".rdata"))
    region$multiOut[manFor,,8,1:3,1] <- region$multiOut[manFor,,8,1:3,1] + 
      aperm(replicate(length(manFor),(manDeadW$ssDeadW[1:nYears,])),c(3,1:2))
    region$multiOut[unmanFor,,8,1:3,1] <- region$multiOut[unmanFor,,8,1:3,1] + 
      aperm(replicate(length(unmanFor),(unmanDeadW$ssDeadW[1:nYears,])),c(3,1:2))
  }
  ####end initialize deadWood Volume
  
  if(outType=="testRun") return(list(region = region,initPrebas=initPrebas))
  if(outType=="dTabs"){
    runModOut(sampleID, sampleX,region,r_no,harvScen,harvInten,rcpfile,areas,
              colsOut1,colsOut2,colsOut3,varSel,sampleForPlots)
    return("all outs saved")  
  } 
  if(outType=="uncRun"){
    uncTab <- UncOutProc(varSel=varSel,#c(46,39,30,37), 
                         funX=funX,#rep("sum",4),
                         modOut=region,sampleID=sampleID,
                         finPeats=finPeats,sampleX=sampleX,
                         EC1=EC1[sampleID],EC2=EC2[sampleID])
    #uncTab <- UncOutProc(varSel=c(46,39,30,37), funX=rep("sum",4),modOut=region)
    #print(uncTab)
    return(uncTab)
  } 
  if(outType=="uncSeg"){
    uncSegTab <- UncOutProcSeg(varSel=varSel, funX=funX,
                               modOut=region,sampleX,colsOut1,colsOut2,colsOut3)
    return(uncSegTab)
  }
  # rm(list=c("region","initPrebas")); gc()
  # rm(list=setdiff(ls(), c(toMem,"toMem")))
  # rm(out); gc()
  # }###harvest loop
  # } ###region loop
  # }rcps loop
  print(paste("end sample ID",sampleID))
  rm(list=setdiff(ls(), c(toMem,"toMem"))); gc()
  
  #print(uncRun)
  # }
}

runModOut <- function(sampleID, sampleX,modOut,r_no,harvScen,harvInten,rcpfile,areas,
                      colsOut1,colsOut2,colsOut3,varSel,sampleForPlots){
  ####create pdf for test plots 
  if(sampleID==sampleForPlots){
    pdf(paste0("plots/testPlots_",r_no,"_",
               harvScen,"_",rcpfile,".pdf"))
    out <- modOut$multiOut
    save(out,file = paste0("outputDT/forCent",r_no,"/testData.rdata"))
    rm(out);gc()
  } 
  marginX= 1:2#(length(dim(out$annual[,,varSel,]))-1)
  nas <- data.table()
  
  for (ij in 1:length(varSel)) {
    # print(varSel[ij])
    if(funX[ij]=="baWmean"){
      outX <- data.table(segID=sampleX$segID,baWmean(modOut,varSel[ij]))
    }
    if(funX[ij]=="sum"){
      outX <- data.table(segID=sampleX$segID,apply(modOut$multiOut[,,varSel[ij],,1],marginX,sum))
    }
    ####test plot
    # print(outX)
    if(sampleID==sampleForPlots){testPlot(outX,varNames[varSel[ij]],areas)}
    
    p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
    p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
    p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
    
    pX <- data.table(p1,p2[,2],p3[,2]) # can be the same segment multiple times
    
    ##check for NAs
    nax <- data.table(segID=unique(which(is.na(pX),arr.ind=T)[,1]))
    if(nrow(nax)>0){
      nax$var <- varNames[varSel[ij]]
      nax$sampleID <- sampleID
      nas <- rbind(nas,nax)
    } 
    
    assign(varNames[varSel[ij]],pX)
    
    save(list=varNames[varSel[ij]],
         file=paste0("outputDT/forCent",r_no,"/",
                     varNames[varSel[ij]],
                     "_harscen",harvScen,
                     "_harInten",harvInten,"_",
                     rcpfile,"_","sampleID",sampleID,".rdata"))
    rm(list=varNames[varSel[ij]]); gc()
    # save NAs
    if(nrow(nas)>0){
      save(nas,file=paste0("NAs/NAs_forCent",r_no,
                           "_","sampleID",sampleID,
                           "_harscen",harvScen,
                           "_harInten",harvInten,"_",
                           rcpfile,".rdata"))        
    }
  }
  ####process and save special variales
  print(paste("start special vars",sampleID))
  specialVarProc(sampleX,modOut,r_no,harvScen,harvInten,rcpfile,sampleID,
                 colsOut1,colsOut2,colsOut3,areas,sampleForPlots)
}



sample_data.f = function(data.all, nSample) {
  cloudpixels = data.all[, sum(ba==32766)]
  nonforest = data.all[, sum(ba==32767)]
  forest = data.all[, sum(ba< 32766)]
  AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
  AREA_1000ha = AREA / 10000 / 1000
  
  ## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
  data.all = data.all[ba < 32766]
  data.all = data.all[!is.na(cons)]
  
  ## REDUCE SAMPLE FOR TESTING ---------------------------------------
  smp = floor(seq(1, dim(data.all)[1], len=nSample))
  data.sample = data.all[smp]
  
  # summary(data.sample[, 3:11])
  
  for (col in colnames(data.sample)[c(3, 5:11)]) set(data.sample, j=col,
                                                     value=as.double(data.sample[[col]]))
  
  ## -----------------------------------------------------------------
  
  
  ## AVOID ZERO CASES
  
  data.sample$dbh = as.double(data.sample$dbh)
  
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert ==1, decid:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert <= 3 & fert > 1, spruce:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert >= 4, pine:=1  ]
  siteX <- union(which(data.sample$ba <=0.041),which(data.sample$h<= 15))
  siteX <- union(siteX,which(data.sample$dbh<=0.5))
  data.sample$nTree <- data.sample$ba/(pi/4*( data.sample$dbh/100)^2)
  siteNN <- which(data.sample$nTree>5000)
  siteX <- union(siteX,siteNN)
  data.sample[siteX,h:=15]
  data.sample[siteX,dbh:=0.5]
  data.sample[siteX,ba:=0.0431969]
  data.sample
}



# StartingYear = climate data that detrermines simulation period must have year greater than this.
create_prebas_input.f = function(r_no, clim, data.sample, nYears,
                                 startingYear=0,domSPrun=0,
                                 harv, HcFactorX=HcFactor, 
                                 outModReStart=NULL,initSoilC=NULL
                                 ) { 
  # dat = climscendataset
  #domSPrun=0 initialize model for mixed forests according to data inputs 
  #domSPrun=1 initialize model only for dominant species 
  nSites <- nrow(data.sample)
  areas <- data.sample$area
  ###site Info matrix. nrow = nSites, cols: 1 = siteID; 2 = climID; 3=site type;
  ###4 = nLayers; 5 = nSpecies;
  ###6=SWinit;   7 = CWinit; 8 = SOGinit; 9 = Sinit
  
  siteInfo <- matrix(c(NA,NA,NA,160,0,0,20,3,3,413,0.45,0.118),nSites,12,byrow = T)
  #siteInfo <- matrix(c(NA,NA,NA,3,3,160,0,0,20),nSites,9,byrow = T)
  siteInfo[,1] <- data.sample$segID
  siteInfo[,2] <- as.numeric(data.sample[,id])
  siteInfo[,3] <- data.sample[,fert]
  
  # litterSize <- matrix(0,3,3)
  # litterSize[1,1:2] <- 30
  # litterSize[1,3] <- 10
  # litterSize[2,] <- 2
  
  ###Initialise model
  # initVardension nSites,variables, nLayers
  # variables: 1 = species; 2 = Age; 3 = H; 4=dbh; 5 = ba; 6 = Hc
    initVar <- array(NA, dim=c(nSites,7,3))
    data.sample[,baP:= (ba * pine/(pine+spruce+decid))]
    data.sample[,baSP:= (ba * spruce/(pine+spruce+decid))]
    data.sample[,baB:= (ba * decid/(pine+spruce+decid))]
    data.sample[,dbhP:= dbh]
    data.sample[,dbhSP:= dbh]
    data.sample[,h:= h/10]
    data.sample[,hP:= h]
    data.sample[,hSP:= h]
    
    data.sample[,N:=ba/(pi*(dbh/2)^2/10000)]
    
    initVar[,1,] <- as.numeric(rep(1:3,each=nSites))
    initVar[,2,] <- round(as.numeric(data.sample[,age]))
    initVar[,3,] <- as.numeric(data.sample[,h])
    # initVar[,3,][which(initVar[,3,]<1.5)] <- 1.5  ####if H < 1.5 set to 1.5
    initVar[,4,] <- as.numeric(data.sample[,dbh])
    
    if(domSPrun==1){
      ##initialize model only for dominant species##
      initVar[,5,] = 0.
      ix = unlist(data.sample[, which.max(c(pine, spruce, decid)), by=1:nrow(data.sample)] [, 2])
      for(jx in 1:nSites) initVar[jx,5,ix[jx]] = as.numeric(data.sample[, ba])[jx]
    } else{
      ###initialize model for mixed forest runs
      initVar[,5,1] <- as.numeric(data.sample[,(ba * pine/(pine+spruce+decid))])
      initVar[,5,2] <- as.numeric(data.sample[,(ba * spruce/(pine+spruce+decid))])
      initVar[,5,3] <- as.numeric(data.sample[,(ba * decid/(pine+spruce+decid))])
      
      if(TRUE){ #### if true will vary H and D of pine and spruce using siteType
        
        ###increase spruceP dbh 10% for spruceP sitetype 1:2
        minDelta <- 0.75
        data.sample[pine>0. & spruce >0. & fert<2.5,X:=pmax(minDelta,(ba-1.1*baSP-baB)/baP)]
        data.sample[pine>0. & spruce >0. & fert<2.5,dbhSP:=1.1*dbh]
        data.sample[pine>0. & spruce >0. & fert<2.5 & X==minDelta,dbhSP:=dbh*(ba-minDelta* baP-baB)/baSP]
        data.sample[pine>0. & spruce >0. & fert<2.5,dbhP:=X*dbh]
        data.sample[pine>0. & spruce >0. & fert<2.5 & dbhP<0.5,dbhSP:=pmax(0.5,((ba-(0.5/dbh)*baP-baB)/baSP))]
        data.sample[pine>0. & spruce >0. & fert<2.5 & dbhP<0.5,dbhP:=0.5]
        
        # data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhSP:=dbh * (ba - 0.9*baP - baB)/baSP]
        # data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhP:=pmax(0.9*dbh,0.3)]
        
        ####increase spruce h 10% for spruce sitetype 1:2
        data.sample[pine>0. & spruce >0. & fert<2.5, X:=pmax(minDelta,(ba-1.1*baSP-baB)/baP)]
        data.sample[pine>0. & spruce >0. & fert<2.5,hSP:=1.1*h]
        data.sample[pine>0. & spruce >0. & fert<2.5 & X==minDelta,hSP:=h*(ba-minDelta* baP-baB)/baSP]
        data.sample[pine>0. & spruce >0. & fert<2.5, hP:=X*h]
        data.sample[pine>0. & spruce >0. & fert<2.5 & hSP<1.5,hSP:=1.5]
        data.sample[pine>0. & spruce >0. & fert<2.5 & hP<1.5,hP:=1.5]
        
        # data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hSP:=h * (ba - 0.9*baP - baB)/baSP]
        # data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hP:=pmax(0.9*h,1.3)]
        #  
        ####increase spruce dbh 5% for spruce sitetype 3
        data.sample[pine>0. & spruce >0. & fert==3, X:=pmax(minDelta,(ba-1.05*baSP-baB)/baP)]
        data.sample[pine>0. & spruce >0. & fert==3, dbhP:=X*dbh]   
        data.sample[pine>0. & spruce >0. & fert==3, dbhSP:=1.05*dbh]
        data.sample[pine>0. & spruce >0. & fert==3 & X==minDelta,dbhSP:=dbh*(ba-minDelta* baP-baB)/baSP]
        data.sample[pine>0. & spruce >0. & fert==3 & dbhP<0.5,dbhSP:=pmax(1.5,((ba-(0.5/dbh)*baP-baB)/baSP)*dbh)]
        data.sample[pine>0. & spruce >0. & fert==3 & dbhP<0.5,dbhP:=0.5]
        
        # data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhSP:=pmin(25,(dbh * (ba - 0.95*baP - baB)/baSP))]
        # data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhP:=pmax(0.95*dbh,0.3)]
        
        ####increase spruce h 5% for spruce sitetype 3
        data.sample[pine>0. & spruce >0. & fert==3, X:=pmax(minDelta,(ba-1.05*baSP-baB)/baP)]
        data.sample[pine>0. & spruce >0. & fert==3, hP:=X*h]
        data.sample[pine>0. & spruce >0. & fert==3, hSP:=1.05*h]
        data.sample[pine>0. & spruce >0. & fert==3 & X==minDelta,hSP:=h*(ba-minDelta* baP-baB)/baSP]
        data.sample[pine>0. & spruce >0. & fert==3 & hSP<1.5, hSP:=1.5]
        data.sample[pine>0. & spruce >0. & fert==3 & hP<1.5, hP:=1.5]
        
        # data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hSP:=pmin(30.,(h * (ba - 0.95*baP - baB)/baSP))]
        # data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hP:=pmax(0.95*h,1.3)]
        
        ####increase pine dbh 10% for sitetype >= 4
        data.sample[pine>0. & spruce >0. & fert>3.5, X:=pmax(minDelta,(ba-1.1*baP-baB)/baSP)]
        data.sample[pine>0. & spruce >0. & fert>3.5, dbhSP:=X*dbh]
        data.sample[pine>0. & spruce >0. & fert>3.5, dbhP:=1.1*dbh]
        data.sample[pine>0. & spruce >0. & fert>3.5 & X==minDelta,dbhP:=dbh*(ba-minDelta*baSP-baB)/baP]
        data.sample[pine>0. & spruce >0. & fert>3.5 & dbhSP<0.5,dbhP:=pmax(1.5,((ba-(0.5/dbh)*baSP-baB)/baP)*dbh)]
        data.sample[pine>0. & spruce >0. & fert>3.5 & dbhSP<0.5,dbhSP:=0.5]
        # data.sample[pine>0. & spruce >0. & fert>3.5 & baP <= baSP,dbhP:=dbh * (ba - 0.9*baSP - baB)/baP]
        # data.sample[pine>0. & spruce >0. & fert>3.5 & baP <= baSP,dbhSP:=pmax(0.9*dbh,0.3)]
        ####increase pine h 10% for sitetype >= 4
        data.sample[pine>0. & spruce >0. & fert>3.5, X:=pmax(minDelta,(ba-1.1*baP-baB)/baSP)]
        data.sample[pine>0. & spruce >0. & fert>3.5,hSP:=X*h]
        data.sample[pine>0. & spruce >0. & fert>3.5,hP:=1.1*h]
        data.sample[pine>0. & spruce >0. & fert>3.5 & X==minDelta,hP:=h*(ba-minDelta*baSP-baB)/baP]
        data.sample[pine>0. & spruce >0. & fert>3.5 & hP<1.5,hP:=1.5]
        data.sample[pine>0. & spruce >0. & fert>3.5 & hSP<1.5,hSP:=1.5]
        # data.sample[pine>0. & spruce >0. & fert>3.5 & baP <= baSP,hP:=h * (ba - 0.9*baSP - baB)/baP]
        # data.sample[pine>0. & spruce >0. & fert>3.5 & baP <= baSP,hSP:=pmax(0.9*h,1.3)]
        
        initVar[,3,1] <- as.numeric(data.sample[,hP])
        initVar[,3,2] <- as.numeric(data.sample[,hSP])
        initVar[,4,1] <- as.numeric(data.sample[,dbhP])
        initVar[,4,2] <- as.numeric(data.sample[,dbhSP])
      }
    }
  
  # initVar[,6,] <- as.numeric(data.sample[,hc])
  
  if(harv %in% c("adapt","protect","protectNoAdH","protectTapio",
                 "adaptNoAdH","adaptTapio")){
    ####always the 3 species layers in this two scenarios
    ###check which BA ==0. and set to 0 the rest of the variable
    NoPine <- which(initVar[,5,1]==0.)
    NoSpruce <- which(initVar[,5,2]==0.)
    NoDecid <- which(initVar[,5,3]==0.)
    
    # siteInfo[NoPine,8] <- siteInfo[NoPine,8] - 1
    # siteInfo[NoSpruce,8] <- siteInfo[NoSpruce,8] - 1
    # siteInfo[NoDecid,8] <- siteInfo[NoDecid,8] - 1
    
    initVar[NoPine,3:6,1] <- 0.
    initVar[NoSpruce,3:6,2] <- 0.
    initVar[NoDecid,3:6,3] <- 0.
    # initVar[NoSpruce,,2] <- initVar[NoSpruce,,3]
    # initVar[NoPine,,1:2] <- initVar[NoPine,,2:3]
    
    # nLay1 <- which(siteInfo[,8]==1)
    # nLay2 <- which(siteInfo[,8]==2)
    # initVar[nLay1,c(1,3:6),2:3] <- 0
    # initVar[nLay2,c(1,3:6),3] <- 0
  }else{
    NoPine <- which(initVar[,5,1]==0.)
    NoSpruce <- which(initVar[,5,2]==0.)
    NoDecid <- which(initVar[,5,3]==0.)
    
    siteInfo[NoPine,8] <- siteInfo[NoPine,8] - 1
    siteInfo[NoSpruce,8] <- siteInfo[NoSpruce,8] - 1
    siteInfo[NoDecid,8] <- siteInfo[NoDecid,8] - 1
    
    initVar[NoPine,3:6,1] <- 0.
    initVar[NoSpruce,3:6,2] <- 0.
    initVar[NoDecid,3:6,3] <- 0.
    initVar[NoSpruce,,2] <- initVar[NoSpruce,,3]
    initVar[NoPine,,1:2] <- initVar[NoPine,,2:3]
    
    nLay1 <- which(siteInfo[,8]==1)
    nLay2 <- which(siteInfo[,8]==2)
    initVar[nLay1,3:6,2:3] <- 0
    initVar[nLay2,3:6,3] <- 0
  }
  
  if (FALSE) {
    dat = dat[id %in% data.sample[, unique(id)]]
    
    if(rcps!= "CurrClim.rdata"){
      # dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
      # dat[, DOY:= as.numeric(format(pvm, "%j"))]
      dat[, Year:= as.numeric(floor(rday/366)+1971)]
      dat = dat[Year >= startingYear]
      dat[DOY==366, DOY:=365]
    }
    PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
                       value.var="PAR")[, -1])
    TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
                        value.var="TAir")[, -1])
    VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
                       value.var="VPD")[, -1])
    Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
                          value.var="Precip")[, -1])
    CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
                       value.var="CO2")[, -1])
  }
  siteInfo[, 2]  = match(as.numeric(siteInfo[, 2]), as.numeric(rownames(clim[[1]])))
  # if(harv %in% c("protect","protectNoAdH","protectTapio")){
  #   siteInfo[, 8] <- siteInfo[, 8]+1
  #   oldLayer <- initVar[,,1]
  #   oldLayer[,4] <- 1
  #   initVar <- abind(initVar,oldLayer,along=3)
  # }
  # siteInfo[, 2]  = match(siteInfo[,2], unique(dat$id))
  
  defaultThin=as.numeric(1-data.sample[, cons])
  energyCut <- ClCut <- as.numeric(1-data.sample[, cons])
  ## Set to match climate data years
  if(!exists("ftTapioParX")) ftTapioParX = ftTapio
  if(!exists("tTapioParX")) tTapioParX = tTapio
  initVar[,6,] <- aaply(initVar,1,findHcNAs,pHcM)[,6,]*HcFactorX
  initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),siteInfo=siteInfo,
                              # litterSize = litterSize,#pAWEN = parsAWEN,
                              pCROBAS = pCrobasX,
                              defaultThin=defaultThin,
                              ClCut = ClCut, areas =areas,
                              energyCut = energyCut, 
                              ftTapioPar = ftTapioParX,
                              tTapioPar = tTapioParX,
                              multiInitVar = as.array(initVar),
                              PAR = clim$PAR[, 1:(nYears*365)],
                              TAir=clim$TAir[, 1:(nYears*365)],
                              VPD=clim$VPD[, 1:(nYears*365)],
                              Precip=clim$Precip[, 1:(nYears*365)],
                              CO2=clim$CO2[, 1:(nYears*365)],
                              yassoRun = 1,
                              mortMod = mortMod)
  
  if(!is.null(outModReStart)){

  ####set the mortality model
    ###reineke for managed forests
    ### reineke + empirical mod for conservation areas
    if(mortMod==13){
      initPrebas$mortMod = c(1,3)#rep(1,nrow(data.sample))
      # initPrebas$mortMod[data.sample$cons==1] <- 3 
    }
    if(!is.null(outModReStart$multiOut)){
      initPrebas$multiOut[,,,1:3,] <- outModReStart$multiOut
      initPrebas$multiOut[,,8,,] = 0
      initPrebas$GVout <- outModReStart$GVout
    } 
    if(!is.null(outModReStart$siteInfo)) initPrebas$siteInfo <- outModReStart$siteInfo
    if(!is.null(outModReStart$initClearcut)) initPrebas$initClearcut <- outModReStart$initClearcut
  }
  if(!is.null(initSoilC)) initPrebas$soilC[,,,,1:3] <- initSoilC
  
  return(initPrebas)
}

yasso.mean.climate.f = function(dat, data.sample, startingYear, nYears){
  dat = dat[id %in% data.sample[, unique(id)]]
  dat[, DOY:=rep(1:365, len=dim(dat)[1])]
  dat[, Year:=rep(1980:2099, each=365)]
  #dat[, Year:= as.numeric(format(pvm, "%Y"))]
  dat = dat[Year >= startingYear & Year <= startingYear+nYears]
  dat[, pvm:= as.Date(paste(Year, '-01-01', sep="")) - 1 + DOY ]
  #dat[, DOY:= as.numeric(format(pvm, "%j"))]
  dat[, Mon:= as.numeric(format(pvm, "%m"))]
  #dat[DOY==366, DOY:=365]
  Tmean = dat[, mean(TAir), by = Year]
  Tsum = dat[, sum(ifelse(TAir>5, TAir-5, 0)), by=.(id, Year)][, mean(V1), by=Year]
  PAR = dat[, mean(PAR), by = Year]
  VPD = dat[, mean(VPD), by = Year]
  CO2 = dat[, mean(CO2), by = Year]
  Precip = dat[, sum(Precip), by = .(id, Year)][, mean(V1), by=Year]
  Tampl = dat[, .(mean(TAir)), by = .(id, Year, Mon)][, (max(V1)-min(V1))/2, by=Year]
  
  out = cbind(Tmean, Precip[, -1], Tampl[, -1], CO2[, -1], PAR[, -1], VPD[, -1], Tsum[, -1])
  colnames(out) = c('Year','Tmean','Precip','Tampl', 'CO2', "PAR", "VPD", "Tsum5")
  out
}


prep.climate.f = function(dat, data.sample, startingYear, nYears){
  dat = dat[id %in% data.sample[, unique(id)]]
  # if(rcps== "CurrClim.rdata"){
  #   dat[, Year:= as.numeric(floor(rday/366)+1971)]
  #   dat = dat[Year >= startingYear]
  #   
  # }else{
  dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
  dat[, DOY:= as.numeric(format(pvm, "%j"))]
  dat[, Year:= as.numeric(format(pvm, "%Y"))]
  dat = dat[Year >= startingYear]
  dat[DOY==366, DOY:=365]
  # }
  id = dat[,unique(id)]
  PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
                     value.var="PAR")[, -1])
  TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
                      value.var="TAir")[, -1])
  VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
                     value.var="VPD")[, -1])
  Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
                        value.var="Precip")[, -1])
  CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
                     value.var="CO2")[, -1])
  list(PAR=PARtran, TAir=TAirtran, VPD=VPDtran, 
       Precip=Preciptran, CO2=CO2tran,id=id)
}


# simSummary.f = function(region=region, r_no, nYears, startingYear, rcpfile, harvScen) {
#   
#   out = region[['multiOut']]
#   VOL = out[, , 30, , 1]
#   VOL = apply(VOL, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   VOL = apply(VOL, 2, mean)
#   ## Multiply by area (tha)
#   VOL_INAREA = VOL * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
#   ## at the beginning 207.7 mill m3, vrt 189.9 according to NFI (for region 7 = Keski-Suomi)
#   
#   Vmort = out[, , 42, , 1]
#   Vmort = apply(Vmort, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   Vmort = apply(Vmort, 2, mean)
#   Vmort_INAREA = Vmort * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
#   
#   
#   ## WHY THIS IS NOT THE SAME AS har?
#   Vharvested = out[, , 37, , 1]
#   Vharvested = apply(Vharvested, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   Vharvested = apply(Vharvested, 2, mean)
#   Vharvested_INAREA = Vharvested * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
#   
#   grossgrowth = out[, , 43, , 1]
#   grossgrowth = apply(grossgrowth, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   grossgrowth = apply(grossgrowth, 2, mean)
#   grossgrowth_INAREA = grossgrowth * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
#   
#   dbh = out[, , 12, , 1]
#   dbh = apply(dbh, c(1,2), mean)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   dbh = apply(dbh, 2, mean)
#   
#   age = out[, , 7, , 1]
#   age = apply(age, c(1,2), mean)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   age = apply(age, 2, mean)
#   
#   gpp = out[, , 10, , 1]
#   gpp = apply(gpp, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   gpp = apply(gpp, 2, mean)
#   #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
#   
#   
#   npp = out[, , 18, , 1]
#   npp = apply(npp, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   npp = apply(npp, 2, mean)
#   #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
#   
#   
#   nep = out[, , 46, , 1]
#   nep = apply(nep, c(1,2), sum, na.rm=TRUE)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   nep = apply(nep, 2, mean)
#   
#   
#   B_tree = out[, , 35, , 1]
#   B_tree = apply(B_tree, c(1,2), sum)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   B_tree = apply(B_tree, 2, mean)
#   
#   lproj = out[, , 21, , 1]
#   lproj = apply(lproj, c(1,2), mean)
#   ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
#   lproj = apply(lproj, 2, mean)
#   data.table(r_no, rcpfile, harvScen, year=startingYear + (1:nYears),
#              VOL, VOL_INAREA, Vharvested, Vmort, Vmort_INAREA,
#              grossgrowth_INAREA, dbh, age, gpp, npp, nep, B_tree, lproj)
# }
# 
# # this function create maps in tif format from raw data.
# createTif <- function(climate, management, yearOut, variable, species, startingYear){
#   simYear <- yearOut - startingYear
#   
#   files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
#   
#   outX <- data.table()
#   ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
#   
#   for(i in 1:length(files)){
#     sampleID <- paste0("sample",i,".")
#     
#     fileX <- files[grep(sampleID,files,fixed = T)]
#     
#     load(paste0("output/",fileX))
#     
#     out <- data.table(out$annual[,simYear,variable,])
#     
#     set.seed(1)
#     sampleX <- ops[[i]]
#     sampleX[,area := N*16^2/10000]
#     sampleX[,id:=climID]
#     
#     outX <- rbind(outX,cbind(sampleX$segID,out))
#     print(i)
#   }
#   
#   
#   
#   setnames(outX,c("segID","pine","spruce","birch"))
#   
#   outX[, tot := rowSums(.SD), .SDcols = c("pine","spruce","birch")]
#   
#   
#   outXY <- merge(kokeIDsTab,outX,all = T)
#   
#   ###create raster 
#   rastX <- rasterFromXYZ(outXY[,c("x","y",species),with=F])
#   crs(rastX) <- crs(kokeShp)
#   
#   rastName <- paste0("outRast/",climate,"_",management,"_var",varNames[variable],
#                      "_spec",species,"_year",yearOut,".tif")
#   writeRaster(rastX,filename = rastName)
# }
# 
# # this function create maps in tif format from data.tables selecting one year or the average of a time priod if yearOut is a vector of years
# createTifFromDT <- function(climate, management, yearOut, variable, species, startingYear){
#   simYear <- yearOut - startingYear
#   fileDT=paste0("outputDT/",varNames[variable],"_",management,"_",climate,".rdata")  
#   load(fileDT)
#   
#   outX <- t(get(varNames[variable]))
#   if (length(simYear)==1) outX <- outX[simYear,]
#   if (length(simYear)>1) outX <- colMeans(outX[simYear,],na.rm = T)
#   
#   segID <- areas <-numeric(0)
#   set.seed(1)
#   ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
#   for(i in 1:115){
#     # set.seed(1)
#     sampleX <- ops[[i]]
#     sampleX[,area := N*16^2/10000]
#     sampleX[,id:=climID]
#     segID <- c(segID,sampleX$segID)
#     areas <- c(areas,sampleX$area)
#     # print(i)
#   }
#   outX <- data.table(cbind(segID,areas,outX))
#   
#   setnames(outX,c("segID","areas",varNames[variable]))
#   
#   # outX[, tot := rowSums(.SD), .SDcols = c("pine","spruce","birch")]
#   
#   outXY <- merge(kokeIDsTab,outX,all = T)
#   
#   ###create raster 
#   rastX <- rasterFromXYZ(outXY[,c("x","y",varNames[variable]),with=F])
#   crs(rastX) <- crs(kokeShp)
#   
#   rastName <- paste0("outRast/",climate,"_",management,"_var",varNames[variable],
#                      "_spec",species,"_year",min(yearOut),"_",max(yearOut),".tif")
#   writeRaster(rastX,filename = rastName,overwrite=T)
# }
# 
# 
# 
# ##function to compile all data and create data.table 
# createDT <- function(climate, management,variable, species, startingYear){
#   
#   files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
#   
#   for (ij in variable) assign(varNames[ij],data.table())
#   VenergyWood <- WenergyWood <- data.table()
#   
#   # segID <- areas <-numeric(0)
#   
#   for(i in 1:length(files)){
#     sampleID <- paste0("sample",i,".")
#     
#     fileX <- files[grep(sampleID,files,fixed = T)]
#     
#     load(paste0("output/",fileX))
#     
#     ###sum harvests
#     if(i==1){
#       harvest <- out$harvest
#     }else{
#       harvest <- harvest+out$harvest  
#     }
#     
#     VenergyWood <- rbind(VenergyWood,apply(out$energyWood[,,,1],1:2,sum))
#     WenergyWood <- rbind(WenergyWood,apply(out$energyWood[,,,2],1:2,sum))
#     
#     marginX= 1:2#(length(dim(out$annual[,,variable,]))-1)
#     for (ij in variable) {
#      varIndx <- match(varNames[ij],varNames[varSel])  
#      assign(varNames[ij],data.table(rbind(eval(parse(text = varNames[ij])),
#                  apply(out$annual[,,varIndx,],marginX,sum))))
#     }    
#     print(i)
#   }
#   
#   ###proc and save total harvests
#   totHarvest <- data.table(harvest)
#   setnames(totHarvest,c("roundWood","energyWood"))
#   save(totHarvest,file=paste0("outputDT/","totHarvest","_",management,"_",climate,".rdata"))
#   
#   save(VenergyWood,file=paste0("outputDT/","VenergyWood","_",management,"_",climate,".rdata"))
#   save(WenergyWood,file=paste0("outputDT/","WenergyWood","_",management,"_",climate,".rdata"))
#   
#   
#   for(ij in variable) save(list=varNames[ij],file=paste0("outputDT/",varNames[ij],"_",management,"_",climate,".rdata"))
# }
# 
# ##function to compile all data and create data.table by species
# createDTbySp <- function(climate, management,variable, species, startingYear){
#   
#   files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
#   
#   for (ij in variable){
#     assign(paste0(varNames[ij],1),data.table())
#     assign(paste0(varNames[ij],2),data.table())
#     assign(paste0(varNames[ij],3),data.table())
#   }
#   # segID <- areas <-numeric(0)
#   
#   for(i in 1:length(files)){
#     sampleID <- paste0("sample",i,".")
#     
#     fileX <- files[grep(sampleID,files,fixed = T)]
#     
#     load(paste0("output/",fileX))
#     
#     marginX= 1:2#(length(dim(out$annual[,,variable,]))-1)
#     for (ij in variable){
#       assign(paste0(varNames[ij],1),
#              data.table(rbind(eval(parse(text = paste0(varNames[ij],1))),
#                               out$annual[,,ij,1])))
#       assign(paste0(varNames[ij],2),
#              data.table(rbind(eval(parse(text = paste0(varNames[ij],2))),
#                               out$annual[,,ij,2])))
#       assign(paste0(varNames[ij],3),
#              data.table(rbind(eval(parse(text = paste0(varNames[ij],3))),
#                               out$annual[,,ij,3])))
#     } 
#     
#     print(i)
#   }
#   
#   for(ij in variable){
#     save(list=c(paste0(varNames[ij],1),paste0(varNames[ij],2),paste0(varNames[ij],3)),
#          file=paste0("outputDT/",varNames[ij],"_",management,"_",climate,"_bySpecies.rdata"))
#   } 
# }
# 
# 
# # this function compute the annual totals of the region from data.tables 
# aTOTfromDT <- function(yearOut, variable, species="tot", startingYear){
#   simYear <- yearOut - startingYear
#   segID <- areas <-numeric(0)
#   set.seed(1)
#   ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
#   for(i in 1:115){
#     sampleX <- ops[[i]]
#     sampleX[,area := N*16^2/10000]
#     sampleX[,id:=climID]
#     segID <- c(segID,sampleX$segID)
#     areas <- c(areas,sampleX$area)
#     # print(i)
#   }
#   files <- list.files("outputDT/",pattern=paste0(varNames[variable],"_"))
#   if (species=="tot") files <- files[-grep("bySpecies",files)]
#   allOut <- data.table()
#   for(i in 1:length(files)){
#     load(paste0("outputDT/",files[i]))
#     
#     dats <- strsplit(files[i], "[_.]+")
#     if(variable %in% 32:33) dats[[1]] <- dats[[1]][-2]
#     climate=dats[[1]][3]
#     management=dats[[1]][2]
#     outX <- get(varNames[variable])*areas
#     outX <- colMeans(outX,na.rm=T)
#     outX <- data.table(cbind(outX,climate,management))
#     outX[,year:=yearOut]
#     allOut <- rbind(allOut,outX)
#   }
#   
#   setnames(allOut,c(varNames[variable],"climate","management","year"))
#   allOut$climate <- factor(allOut$climate)
#   allOut$management <- factor(allOut$management)
#   allOut[,1] <- as.numeric(unlist(allOut[,1]))
#   
#   fwrite(allOut,file= paste0("plots/",varNames[variable],"_DT.txt"))  
#   
#   p <- ggplot(data=allOut, 
#               aes_string(x="year", y=varNames[variable])) +
#     # scale_shape_manual(values=1:nlevels(countryTot$harvScenario)) +
#     labs(title = varNames[variable])+
#     # geom_smooth() +
#     xlab("Year") +
#     ylab("") +
#     geom_point(aes(colour=management, shape = climate,group=interaction(management, climate))) +
#     geom_line(aes(colour=management, group=interaction(management, climate)))
#   
#   pdf(file=paste0("plots/",varNames[variable], ".pdf"))
#   print(p)
#   dev.off()
# }
# 
# ###compute total biomass from DTs
# Wtot <- function(manClim){
#   files <- paste0("outputDT/",varNames[c(24:25,31:33)],manClim)
#   for(i in 1:5) load(files[i])
#   Wtot <- Wstem + W_croot + wf_STKG + Wbranch + WfineRoots
#   save(Wtot,file = paste0("outputDT/","Wtot",manClim))
# }


# createPlotfromDT <- function(path, variable){
#   DT <- fread(paste0(path,varNames[variable],"_DT.txt"))
#   p <- ggplot(data=DT, 
#               aes_string(x="year", y=varNames[variable])) +
#     # scale_shape_manual(values=1:nlevels(countryTot$harvScenario)) +
#     labs(title = varNames[variable])+
#     # geom_smooth() +
#     xlab("Year") +
#     ylab("") +
#     geom_point(aes(colour=management, shape = climate,group=interaction(management, climate))) +
#     geom_line(aes(colour=management, group=interaction(management, climate)))
#   
#   png(file=paste0("plots/",varNames[variable], ".png"),width = 500,height = 500)
#   print(p)
#   dev.off()
# }

calMean <- function(varX,hscenX,areas){
  load(paste0("outputDT/",varX,"_",hscenX,"_CurrClim.rdata"))
  varAreas <- get(varX)*areas
  # Vareas <- Vareas[-siteX]
  totX <- colSums(varAreas,na.rm = T)
  meanX <- totX/sum(areas)#co
  return(meanX)
}



specialVarProc <- function(sampleX,region,r_no,harvScen,harvInten,rcpfile,sampleID,
                           colsOut1,colsOut2,colsOut3,areas,sampleForPlots){
  nYears <-  max(region$nYears)
  nSites <-  max(region$nSites)
  ####process and save special variables: 
  ###dominant Species
  outX <- domFun(region,varX="species")  
  ####test plot
  if(sampleID==sampleForPlots){testPlot(outX,"domSpecies",areas)}
  ###take the most frequent species in the periods
  p1 <- outX[,.(per1 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut1,by=segID]
  p2 <- outX[,.(per2 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut2,by=segID]
  p3 <- outX[,.(per3 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut3,by=segID]
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  domSpecies <- pX
  save(domSpecies,file=paste0("outputDT/forCent",r_no,"/domSpecies",
                              "_harscen",harvScen,
                              "_harInten",harvInten,"_",
                              rcpfile,"_",
                              "sampleID",sampleID,".rdata"))
  # rm(domSpecies); gc()
  ###age dominant species
  outX <- domFun(region,varX="age")
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  domAge <- pX
  
  save(domAge,file=paste0("outputDT/forCent",r_no,"/domAge",
                          "_harscen",harvScen,
                          "_harInten",harvInten,"_",
                          rcpfile,"_",
                          "sampleID",sampleID,".rdata"))
  ###deciduous Volume Vdec
  outX <- vDecFun(region)
  if(sampleID==sampleForPlots){testPlot(outX,"Vdec",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  Vdec <- pX
  save(Vdec,file=paste0("outputDT/forCent",r_no,"/Vdec",
                        "_harscen",harvScen,
                        "_harInten",harvInten,"_",
                        rcpfile,"_",
                        "sampleID",sampleID,".rdata"))
  
  ####WenergyWood
  outX <- data.table(segID=sampleX$segID,apply(region$multiEnergyWood[,,,2],1:2,sum))
  if(sampleID==sampleForPlots){testPlot(outX,"WenergyWood",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  WenergyWood <- merge(pX,p3)
  save(WenergyWood,file=paste0("outputDT/forCent",r_no,"/WenergyWood",
                               "_harscen",harvScen,
                               "_harInten",harvInten,"_",
                               rcpfile,"_",
                               "sampleID",sampleID,".rdata"))
  ####VenergyWood
  outX <- data.table(segID=sampleX$segID,apply(region$multiEnergyWood[,,,1],1:2,sum))
  if(sampleID==sampleForPlots){testPlot(outX,"VenergyWood",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  VenergyWood <- merge(pX,p3)
  save(VenergyWood,file=paste0("outputDT/forCent",r_no,
                               "/VenergyWood","_harscen",harvScen,
                               "_harInten",harvInten,"_",
                               rcpfile,"_",
                               "sampleID",sampleID,".rdata"))
  ####GVgpp
  outX <- data.table(segID=sampleX$segID,region$GVout[,,3])
  if(sampleID==sampleForPlots){testPlot(outX,"GVgpp",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  GVgpp <- merge(pX,p3)
  save(GVgpp,file=paste0("outputDT/forCent",r_no,
                         "/GVgpp",
                         "_harscen",harvScen,
                         "_harInten",harvInten,"_",
                         rcpfile,"_",
                         "sampleID",sampleID,".rdata"))
  ####GVw
  outX <- data.table(segID=sampleX$segID,region$GVout[,,4])
  if(sampleID==sampleForPlots){testPlot(outX,"GVw",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  GVw <- merge(pX,p3)
  save(GVw,file=paste0("outputDT/forCent",r_no,
                       "/GVw","_harscen",harvScen,
                       "_harInten",harvInten,
                       "_",rcpfile,"_",
                       "sampleID",sampleID,".rdata"))
  ####Wtot
  outX <- data.table(segID=sampleX$segID,apply(region$multiOut[,,c(24,25,31,32,33),,1],1:2,sum))
  if(sampleID==sampleForPlots){testPlot(outX,"Wtot",areas)}
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  Wtot <- merge(pX,p3)
  save(Wtot,file=paste0("outputDT/forCent",r_no,"/Wtot",
                        "_harscen",harvScen,
                        "_harInten",harvInten,"_",
                        rcpfile,"_",
                        "sampleID",sampleID,".rdata"))
  rm(domSpecies,domAge,Vdec,WenergyWood,Wtot,pX,p1,p2,p3); gc()
  if(sampleID==sampleForPlots){dev.off()}
  
} 



####test plot
testPlot <- function(outX,titleX,areas){
  cc <- data.table(rbind(cbind(1:nYears,apply(outX[,2:(nYears+1)],2,min,na.rm=T),"min"),
                         cbind(1:nYears,apply(outX[,2:(nYears+1)],2,max,na.rm=T),"max"),
                         cbind(1:nYears,apply(outX[,2:(nYears+1)],2,median,na.rm=T),"median"),
                         cbind(1:nYears,apply(outX[,2:(nYears+1)],2,mean,na.rm=T),"aritMean"),
                         cbind(1:nYears,apply((outX[,2:(nYears+1)]*areas/sum(areas)),2,sum,na.rm=T),"regionMean")))
  setnames(cc,c("simYear","value","metric"))
  # cc$metric=as.factor(cc$metric)
  cc$metric=factor(cc$metric)
  cc$value=as.double(cc$value)
  cc$simYear <- as.double(cc$simYear)
  cc <- cc[order(simYear)]
  testP <- ggplot(data=cc, aes(x=simYear, y=value, col=metric,group=metric)) +
    geom_line()+
    geom_point() + ggtitle(titleX)
  print(testP)
}


####Function to process NEP for drained peatlands (used in 2.1_procNep.r)
processPeat <- function(peatXf, fertf, npp_lit, nepf, peatval, fertval) {
  # peatXf = raster with peat soils
  # fertf = raster with soilType
  # npp_lit = raster of npp - litterfall (NEP= NPP - coeffSoil - lit)
  # nepf= raster with nep
  # peatval = ID to identify the drained peatlands -> tells which peat soil you want to treat
  # fertval = soilType ID -> tells which siteType you want to treat
  
  # rasters may be off by a couple pixels, resize:
  if (any(dim(fertf) < dim(peatXf))) {peatXf <- crop(peatXf,fertf)} 
  if (any(dim(peatXf) < dim(fertf))) {fertf <- crop(fertf,peatXf)}
  if (any(dim(fertf) < dim(npp_lit))) {npp_lit <- crop(npp_lit,fertf)} 
  if (any(dim(peatXf) < dim(npp_lit))) {npp_lit <- crop(npp_lit,peatXf)}
  if (any(dim(fertf) < dim(nepf))) {nepf <- crop(nepf,fertf)} 
  if (any(dim(peatXf) < dim(nepf))) {nepf <- crop(nepf,peatXf)}
  # mask out pixels where peatXf == peatval and fertx == fertval
  drPeatNeg <- peatXf == peatval & fertf == fertval  ###selecting the pixels that match the conditions of peat and siteType
  drPeatNeg[drPeatNeg==0] <- NA  ### assign NA to the remaining pixels
  drPeat <- mask(npp_lit, drPeatNeg)  ###raster with only the pixel of interest
  
  ###calculate the new NEP according to the siteType (fertval)
  if (fertval < 3) {         
    drPeat <- drPeat - 240  
  } else if (fertval >= 3) {
    drPeat <- drPeat + 70
  }
  return(merge(drPeat,nepf))
}



#####functions to calculate Mortality related metrics as in 
###15Silva Fennica vol. 54 no. 5 article id 10414 · Siipilehto et al. · Stand-level mortality models for Nordic boreal ...
pMort <- function(modOut,ageClass, rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  pMortX <- rep(0.,length(endX))
  
  for(i in 1:length(startX)){
    ageX <-rowMeans(modOut[,startX[i]:endX[i],7,1,1])
    cX <- which(ageX %in% ageClass)
    # outX <- modOut[cX,,,,]
    mortX <- data.table(which(modOut[cX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
    nMort <- length(unique(mortX$site))
    pMortX[i] <- nMort/length(cX)
  }
  return(pMortX)
}

###Function to calculate the probability of a mortality (pM) event occuring
# Arguments: 
# modOut = output array from a PREBAS multisite runs: $multiOut
# rangeYear = number of years  for which to calculate pM
# sp = species/layer for which to calculate pM it can be a vector for combinations of species
# pureFor = proportion of Basal area to consider as pure stands
# mixFor = it works only for mixed forests, it is the minimum proportion of basal area for the species of interest

pMort2 <- function(modOut,ageClass, rangeYear=5,sp,pureFor,mixFor){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  pMortX <- nSites <- rep(0.,length(endX))
  
  for(i in 1:length(startX)){
    ageX <-rowMeans(modOut[,startX[i]:endX[i],7,1,1])
    pBA <- apply(modOut[,startX[i]:endX[i],13,,1],c(1,3),mean)
    pBA <- pBA/rowSums(pBA)
    if(length(sp)==1){
      selX <- which(ageX %in% ageClass & pBA[,sp]>pureFor)
    }else{
      selX <- which(ageX %in% ageClass & rowSums(pBA[,sp])>mixFor &
                      pBA[,1]<pureFor & pBA[,2]<pureFor)  
    }
    
    # outX <- modOut[cX,,,,]
    mortX <- data.table(which(modOut[selX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
    nMort <- length(unique(mortX$site))
    pMortX[i] <- nMort/length(selX)
    nSites[i] <- length(selX)
  }
  return(list(pMort=pMortX,nSites=nSites))
}


###function to calculate the mortality probability along some variable classes
# Arguments: 
# modOut = output array from a PREBAS multisite runs
# rangeYear = number of years  for which to calculate pM
# minX = minimum value for the variable class
# maxX = maximum value for the variable class
# stepX = class step
# varX = variable ID of PREBAS output (see varNames)
# funX = function to use to aggregate the data (mean or sum) mean for age and DBH, sum for BA, stemNumber
pMortVarX <- function(modOut,minX,maxX,stepX,varX,funX,rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  seqX <- seq(minX,maxX,by=stepX)
  nClass <- length(seqX)+1
  pMortX <- nData <- matrix(0.,length(endX),nClass)
  for(i in 1:length(startX)){
    varXs<-apply(modOut[,startX[i]:endX[i],varX,,1],1:2,funX)
    varXs <- rowMeans(varXs)
    for(ij in 1:nClass){
      if(ij==1) cX <- which(varXs <= seqX[ij])
      if(ij>1 & ij<nClass) cX <- which(varXs <= seqX[ij] & varXs > seqX[ij-1])
      if(ij==nClass) cX <- which(varXs > seqX[ij-1])
      # outX <- modOut[cX,,,,]
      if(length(cX)>0.){
        mortX <- data.table(which(modOut[cX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nData[i,ij] <- length(cX)
        pMortX[i,ij] <- nMort/length(cX)
      }
    }
  }
  return(list(pMort=pMortX,nData=nData,classes=seqX))
}


###function to calculate basal area of dead trees along some variable classes
# Arguments: 
# modOut = output array from a PREBAS multisite runs: $multiOut
# rangeYear = number of years  for which to calculate pM
# minX = minimum value for the variable class
# maxX = maximum value for the variable class
# stepX = class step
# varX = variable ID of PREBAS output (see varNames)
# funX = function to use to aggregate the data (mean or sum) mean for age and DBH, sum for BA, stemNumber
baMortVarX <- function(modOut,minX,maxX,stepX,varX,funX,rangeYear=5){
  nYears <- dim(modOut)[2]
  nMort <- modOut[,2:nYears,42,,1]/modOut[,1:(nYears-1),30,,1]*modOut[,1:(nYears-1),17,,1]
  nMort[which(is.na(nMort))] <- 0.
  baMort <- nMort * modOut[,1:(nYears-1),35,,1]
  baTot <- apply(modOut[,1:(nYears-1),13,,1],1:2,sum)
  
  endX <- rangeYear:(nYears-1)
  startX <- endX-(rangeYear-1)
  seqX <- seq(minX,maxX,by=stepX)
  nClass <- length(seqX)+1
  baTotX <- baMortX <- nData <- matrix(0.,length(endX),nClass)
  # oo <- modOut
  modOut <- modOut[,2:nYears,,,]
  for(i in 1:length(startX)){
    varXs<-apply(modOut[,startX[i]:endX[i],varX,,1],1:2,funX)
    varXs <- rowMeans(varXs)
    for(ij in 1:nClass){
      if(ij==1) cX <- which(varXs <= seqX[ij])
      if(ij>1 & ij<nClass) cX <- which(varXs <= seqX[ij] & varXs > seqX[ij-1])
      if(ij==nClass) cX <- which(varXs > seqX[ij-1])
      # outX <- modOut[cX,,,,]
      if(length(cX)>0.){
        baX <- sum(baMort[cX,startX[i]:endX[i],])/length(cX)
        baTx <- sum(baTot[cX,startX[i]:endX[i]])/rangeYear/length(cX)
        nData[i,ij] <- length(cX)
        baMortX[i,ij] <- baX
        baTotX[i,ij] <- baTx
      }
    }
  }
  return(list(baMort=baMortX,nData=nData,classes=seqX,
              baTot=baTotX))
}


###function to calculate the mortality probability for species proportion
# Arguments: 
# modOut = output array from a PREBAS multisite runs
# rangeYear = number of years  for which to calculate pM
# minX = minimum species cover
# maxX = maximum species cover
# stepX = class step
pMortSpecies <- function(modOut,minX=0.1,maxX=0.9,stepX=0.1,rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  seqX <- seq(minX,maxX,by=stepX)
  nClass <- length(seqX)+1
  pMortXpine <- nDataPine <- 
    pMortXspruce <- nDataSpruce <- 
    pMortXbirch <- nDataBirch <- matrix(0.,length(endX),nClass)
  totBA <- apply(modOut[,,13,,1],1:2,sum)
  pBApine <- modOut[,,13,1,1]/totBA
  pBAspruce <- modOut[,,13,2,1]/totBA
  pBAbirch <- modOut[,,13,3,1]/totBA
  for(i in 1:length(startX)){
    subPine <-rowMeans(pBApine[,startX[i]:endX[i]],na.rm=T)
    subSpruce <-rowMeans(pBAspruce[,startX[i]:endX[i]],na.rm=T)
    subBirch <-rowMeans(pBAbirch[,startX[i]:endX[i]],na.rm=T)
    for(ij in 1:nClass){
      if(ij==1){
        cXpine <- which(subPine <= seqX[ij])
        cXspruce <- which(subSpruce <= seqX[ij])
        cXbirch <- which(subBirch <= seqX[ij])
      } 
      if(ij>1 & ij<nClass){
        cXpine <- which(subPine <= seqX[ij] & subPine > seqX[ij-1])
        cXspruce <- which(subSpruce <= seqX[ij] & subSpruce > seqX[ij-1])
        cXbirch <- which(subBirch <= seqX[ij] & subBirch > seqX[ij-1])
      } 
      if(ij==nClass){
        cXpine <- which(subPine > seqX[ij])
        cXspruce <- which(subSpruce > seqX[ij])
        cXbirch <- which(subBirch > seqX[ij])
      } 
      # outX <- modOut[cX,,,,]
      if(length(cXpine)>0.){
        mortX <- data.table(which(modOut[cXpine,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataPine[i,ij] <- length(cXpine)
        pMortXpine[i,ij] <- nMort/length(cXpine)
      }
      if(length(cXspruce)>0.){
        mortX <- data.table(which(modOut[cXspruce,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataSpruce[i,ij] <- length(cXspruce)
        pMortXspruce[i,ij] <- nMort/length(cXspruce)
      }
      if(length(cXbirch)>0.){
        mortX <- data.table(which(modOut[cXbirch,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataBirch[i,ij] <- length(cXbirch)
        pMortXbirch[i,ij] <- nMort/length(cXbirch)
      }
    }
  }
  return(list(pMortPine=pMortXpine,nDataPine=nDataPine,
              pMortSpruce=pMortXspruce,nDataSpruce=nDataSpruce,
              pMortBirch=pMortXbirch,nDataBirch=nDataBirch))
}


#### function to calculate the new parameters of the ClearCuts
#### increasing or decreasing the rotation length
#### out=multi prebas run output
### fact is the factor used to increase the rotation (percentage)
########or to decrease (# negative number of years)  
calNewDclcut <- function(out,
                         ClCut_pine,
                         ClCut_spruce,
                         ClCut_birch,
                         fact=0.25){
  newClCut_pine <- ClCut_pine
  newClCut_spruce <- ClCut_spruce 
  newClCut_birch <- ClCut_birch
  
  nSites <- dim(out$multiOut)[1]
  ETSmean = round(mean(out$multiOut[,,5,1,1]))
  domSp <- rep(NA,nSites)
  domX <- apply(out$multiOut[,,13,,1],1:2, which.max)
  domPos <- apply(domX,1,FUN=function(x) which.max(table(x)))
  for(i in 1:nSites) domSp[i] <- out$multiOut[i,1,4,domPos[i],1]
  siteType <- out$siteInfo[,3]
  
  sitesP3 <- which(siteType<=3 & domSp==1)
  sitesP4 <- which(siteType==4 & domSp==1)
  sitesP5 <- which(siteType>=5 & domSp==1)
  sitesSP2 <- which(siteType<=2 & domSp==2)
  sitesSP3 <- which(siteType>=3 & domSp==2)
  sitesB2 <- which(siteType<=2 & domSp==3)
  sitesB3 <- which(siteType>=3 & domSp==3)
  
  pdf(paste0(pathX,"ClCutplots_maak",r_no,".pdf"))
  for(j in 1:7){
    sites <- get(spSite[j])
    spX <- spXs[j]
    dClcut <- get(tabX[j])[indX[j],c(1,3)]
    aClcut <- get(tabX[j])[indX[j],c(2,4)]
    
    dataX <- data.table(age=as.vector(out$multiOut[sites,,7,spX,1]),
                        d=as.vector(out$multiOut[sites,,12,spX,1]))
    
    dataX <- dataX[age>0. & d>0]
    
    
    ###update the age of clearcut according to the factor (could be percentage(if increasing) or # years(if decreasing))
    if(fact<2 & fact>0.){
      newAge <- (1+fact) * aClcut
    }else{
      newAge <- aClcut + fact 
    } 

    
    if(nrow(dataX) > 10){ ####check if there are enough data to fit the model
    ###fit age vs D model
      fitMod = nlsLM(d ~ a*(1-exp(b*age))^c,
                     start = list(a = 60,b=-0.07,c=1.185),
                     data = dataX)

      ###generate D data using the model      
      modD <- data.table(age=seq(0,300,0.5))    
      modD$d=predict(fitMod, list(age = modD$age))
      
      px <- coef(fitMod)
      a=px[1];b=px[2];c=px[3]
      # 
      dd=dClcut

      ###using the fitted model and inverting it
      ###calculate the age at which 
      ###the dClcut dbh correspond
      aa= log(1-(dd/a)^(1/c))/b
      
      d2 <- predict(fitMod, list(age = aa))
      if(any(is.na(d2))){ ####if there are NAs use the ratio between the age new and aClcut to calculate the new dbh for clearcut
        d2[is.na(aa)] <- newAge[is.na(aa)]/aClcut[is.na(aa)]*dClcut[is.na(aa)]
      }
    }else{
      ####if there are not data to fit the age vs dbh model increase or decrease DBH according to the factor
      if(fact<2 & fact>0.){
        d2 <- dClcut*(1+(fact*0.5))
      }else{
        d2 <- dClcut*0.9
      } 
    }
    # predict(fitMod, list(age = aa))
    # 
###create Plots for testing     
    dataX[,plot(age,d,pch='.',ylim=c(0,45),xlim=c(0,300))]
    lines(modD$age,modD$d,col=4)
    points(aClcut,dClcut,col=3,pch=c(1,20))
    points(newAge,d2,col=2,pch=c(1,20))
    legend("bottomright",cex=0.8,
           pch=c(20,20,NA),
           legend=c("tapio","new","pch is for siteTypes"),
           col= c(3,2,NA)
    )

    if(tabX[j]=="ClCut_pine") newClCut_pine[indX[j],c(1,3)] <- d2
    if(tabX[j]=="ClCut_spruce") newClCut_spruce[indX[j],c(1,3)] <- d2
    if(tabX[j]=="ClCut_birch") newClCut_birch[indX[j],c(1,3)] <- d2
    
    print(j)
  }
  dev.off()
  if(fact<2 & fact>0.){
    newClCut_pine[,c(2,4)] <- ClCut_pine[,c(2,4)]*(1+fact)
    newClCut_spruce[,c(2,4)] <- ClCut_spruce[,c(2,4)]*(1+fact)
    newClCut_birch[,c(2,4)] <- ClCut_birch[,c(2,4)]*(1+fact)
  }else{
    newClCut_pine[,c(2,4)] <- ClCut_pine[,c(2,4)]+fact
    newClCut_spruce[,c(2,4)] <- ClCut_spruce[,c(2,4)]+fact
    newClCut_birch[,c(2,4)] <- ClCut_birch[,c(2,4)]+fact
  } 
  return(list(ClCut_pine=newClCut_pine,
              ClCut_spruce=newClCut_spruce,
              ClCut_birch=newClCut_birch))
}

updatePclcut <- function(initPrebas,pClCut){
  nSites <- initPrebas$nSites
  ClCut <- initPrebas$ClCut
  inDclct <- initPrebas$inDclct
  ETSmean <- rowMeans(initPrebas$ETSy)
  ETSthres <- 1000
  climIDs <- initPrebas$siteInfo[,2]
  siteType <- initPrebas$siteInfo[,3]
  inDclct <- initPrebas$inDclct
  inAclct <- initPrebas$inAclct
  for(i in 1: nSites){
    if(ClCut[i]==1) inDclct[i,] <-
        c(ClCutD_Pine(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_pine),
          ClCutD_Spruce(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_spruce),
          ClCutD_Birch(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_birch),
          0,0,0,0,0,0,0)  ###"fasy","pipi","eugl","rops","popu",'eugrur','piab(DE)')
    if(ClCut[i]==1) inAclct[i,] <-
        c(ClCutA_Pine(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_pine),
          ClCutA_Spruce(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_spruce),
          ClCutA_Birch(ETSmean[climIDs[i]],ETSthres,siteType[i],pClcut= pClCut$ClCut_birch),
          80,50,13,30,50,13,120)  ###"fasy","pipi","eugl","rops","popu",'eugrur','piab(DE)')
  }
  return(list(inDclct=inDclct,inAclct=inAclct))
}

#returns a the dominant species or the age of dominant species for each site at each year
###varX="species" -> returns the dominant species
###varX="age" -> returns the age of dominant layer
domFun <- function(modOut,varX="species"){
  nSites <- modOut$nSites
  nYears <- modOut$maxYears
  segID <- modOut$siteInfo[,1]
  
  oo <- as.vector(apply(modOut$multiOut[,,30,1:3,1],1:2,which.max))  
  oo <- cbind(rep(1:nSites,nYears),
              rep(1:nYears,each=nSites),
              oo)
  if(varX=="species") domX <- matrix(modOut$multiOut[,,4,1:3,1][oo],
                                     nSites,nYears)
  if(varX=="age") domX <- matrix(modOut$multiOut[,,7,1:3,1][oo],
                                 nSites,nYears)
  outX <- data.table(segID=segID,domX)
}


###retunrs the Volume of deciduous
##modOut -> multiPREBAS output
vDecFun <- function(modOut){
  segID <- modOut$siteInfo[,1]
  oo <- data.table(which(modOut$multiOut[,,4,,1]==3,arr.ind=T))
  setnames(oo,c("site","year","layer"))
  vx <-modOut$multiOut[,,30,,1][as.matrix(oo)]
  oo$Vdec <- vx
  setkey(oo,site,year)
  ff <- oo[,sum(Vdec),by=.(site,year)]
  VdecMat <- matrix(0,modOut$nSites,modOut$maxYears)
  VdecMat[as.matrix(ff[,1:2])] <- unlist(ff[,3])
  outX <- data.table(segID=segID,VdecMat)
}


#####extract model output as baweighted mean or sum according to funX
##modOut -> multiPREBAS output
##varSel -> variable ID 
##funX -> function to summarize the output accross layers (sum or BA weighted mean (baWmean))
outProcFun <- function(modOut,varSel,funX="baWmean"){
  segID <- modOut$siteInfo[,1]
  marginX <- 1:2
  if(funX=="baWmean"){
    outX <- data.table(segID=segID,baWmean(modOut,varSel))
  }
  if(funX=="sum"){
    outX <- data.table(segID=segID,apply(modOut$multiOut[,,varSel,,1],marginX,sum))
  }
  setnames(outX,c("segID",1:modOut$maxYears))
  return(outX)
}



