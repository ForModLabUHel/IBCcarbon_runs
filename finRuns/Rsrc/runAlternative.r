# runModelSampleIn <- function(outType="testRun",
#                      sampleX,initSoilCin=NA){
  # outType determines the type of output:
  # dTabs -> standard run, mod outputs saved as data.tables 
  # testRun-> test run reports the mod out and initPrebas as objects
  # ststDeadW -> initialize the dead Wood volume;
  # uncRun -> reports the output table for the regional uncertainty run
  # uncSeg -> reports the list of output table for the segment uncertainty run
  
  # print(date())
  print(paste("start sample ID",sampleID))
  harscen = harvestscenarios

  if(outType=="uncRun"){
    area_tot <- sum(data.all$area) # ha
    sampleX[,area := 16^2/10000] 
    cA <- 1/nrow(sampleX) #area_tot/nrow(sampleX) 
  } else {
    sampleX[,area := N*16^2/10000] 
  }
  sampleX[,id:=climID]
  HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
  nSample = nrow(sampleX)#200#nrow(data.all)
  ## ---------------------------------------------------------
  i = 0
  rcpfile = rcps
  load(paste(climatepath, rcpfile,".rdata", sep=""))  
  #if(outType != "uncRun"){
  if(!outType %in% c("uncRun","uncSeg")){
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
  if(outType %in% c("uncRun","uncSeg")){
    pCrobasX <- pCROBASr[[sampleID]]
  }
  ## Second, continue now starting from soil SS
  initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                     startingYear = startingYear,domSPrun=domSPrun,
                                     harv=harscen)
  
  
  opsna <- which(is.na(initPrebas$multiInitVar))
  initPrebas$multiInitVar[opsna] <- 0.
  
  ### for adapt and protect scenario Replanting schemes 
  ### do not replant pine in sitetypes 1 and 2
  ### do not replant spruce in sitetypes higher than 3
  ### ensure minimum 20% birch at replanting
  if(harscen %in% c("adapt","protect","protectNoAdH",
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
  
  # initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
  # initSoil[,,1,,1] <- initSoilCstst[[r_no]]
  # initSoil <- aperm(initSoil,c(4,5,1:3))
  # initPrebas$soilC <- initSoil
  # if(exists("soilCststXX")) initPrebas$soilC[,1,,,] <- soilCststXX[[sampleID]]$soilC
  
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
  # for(harscen in harvestscenarios) { ## MaxSust fails, others worked.
  # print(date())
  # print(harscen)
  i = i + 1
  # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
  # harscen ="Base"
  
  ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
  if(regSets!="maakunta"){
    Region = nfiareas[ID==r_no, Region]
    if(harscen=="NoHarv"){
      initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      HarvLim1 = 0
    }else if(harscen=="Tapio"){
      HarvLim1 = 0
    }else{
      HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
      HarvLim0  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2015-2024"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- rep(as.numeric(HarvLim),10)
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2025-2034"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2035-2044"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2045-2054"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2055-2064"]
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
      if(harscen == "Low"){ HarvLim1 <- HarvLimX * 0.6}
      if(harscen == "MaxSust"){HarvLim1 <- HarvLimX * 1.2}
      if(harscen == "NoHarv"){
        HarvLim1 <- HarvLimX * 0.
        initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      }
    }else{
      roundWood <- HarvLim1 * roundTotWoodRatio
      enWood <- HarvLim1 - roundWood
      HarvLim1 <- cbind(roundWood,enWood)
    }
  }else{
    HarvLim1 <- HarvLimMaak*1000*sum(areas)/sum(data.all$area)
    if(harscen == "Low"){ HarvLim1 <- HarvLim1 * 0.6}
    if(harscen == "MaxSust"){HarvLim1 <- HarvLim1 * 1.2}
    if(harscen == "NoHarv"){HarvLim1 <- HarvLim1 * 0.
    initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
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
  # }else{
  #   cutArX <- NA
  # } 
  # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
  # HarvLim1 <- rep(0,2)
  # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
  # region <- regionPrebas(initPrebas)
  ###run PREBAS
  if(!all(is.na(initSoilCin))){
    initPrebas$soilC[,1,,,] <- initSoilC <- initSoilCin
  }
  # }else{
  #   if(harscen !="Base"){
  #     if(outType!="uncRun"){
  #       if(!harscen %in% c("protect","protectNoAdH")){
  #         load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,".rdata"))  
  #       }
  #     }else{
  #       load(paste0("initSoilCunc/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
  #     }
  #     initPrebas$yassoRun <- rep(1,initPrebas$nSites)
  #     initPrebas$soilC[,1,,,] <- initSoilC
  #   }
  # }
  print(harscen)
  HarvLimX <- HarvLim1[1:nYears,]
  
  if(harscen %in% c("adapt","adaptNoAdH","adaptTapio")){
    if(harscen=="adaptNoAdH"){
      compHarvX=0.
    }else{
      compHarvX=3.
    }
    # HarvLimX[,2]=0.
    # initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
    if(harscen=="adaptTapio"){
      region <- regionPrebas(initPrebas,
                             fertThin = fertThin,nYearsFert = nYearsFert)
    }else{
      region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                             cutAreas = cutArX,compHarv=compHarvX,
                             fertThin = fertThin,nYearsFert = nYearsFert)
    }
  }else if(harscen %in% c("Mitigation","MitigationNoAdH")){
    if(harscen=="MitigationNoAdH"){
      compHarvX=0.
    }else{
      compHarvX=3.
    }
    HarvLimX[,2]=0.
    initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
    load(paste0("input/",regSets,"/pClCut_mitigation/ClCutplots_maak",r_no,".rdata"))
    ClcutX <- updatePclcut(initPrebas,pClCut)
    initPrebas$inDclct <- ClcutX$inDclct
    initPrebas$inAclct <- ClcutX$inAclct
    initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
    region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                           cutAreas =cutArX,compHarv=compHarvX,
                           ageHarvPrior = ageHarvPriorX)
  }else if(harscen %in% c("protect","protectNoAdH")){
    if(harscen=="protectNoAdH"){
      compHarvX=0.
    }else{
      compHarvX=3.
    }
    ####no energy cuts
    HarvLimX[,2]=0.
    initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
    region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                           cutAreas =cutArX,compHarv=compHarvX,
                           ageHarvPrior = ageHarvPriorX,
                           oldLayer = 1)
  }else{
    ##Don't pass minDharvX if NA
    if (is.na(minDharvX)) {
      region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                             cutAreas =cutArX,compHarv=compHarvX)
    } else {
      region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                             minDharv = minDharvX,cutAreas =cutArX,
                             compHarv=compHarvX)
    }
  }
  
  print(paste("runModel",sampleID,"completed"))
  ##calculate steady state carbon from prebas litter 
  if(harscen=="Base"){
    initSoilC <- stXX_GV(region, 1)
    print(paste("initSoilC",sampleID))
    # if(outType!="testRun"){
    #   if(!outType %in% c("uncRun","uncSeg")){
    #     save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
    #   } else {
    #     save(initSoilC,file=paste0("initSoilCunc/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
    #   }
    # }
    ###run yasso (starting from steady state) using PREBAS litter
    region <- yassoPREBASin(region,initSoilC)
    # out <- region$multiOut[,,,,1]
  }
  print(paste("all runs done",sampleID))
  
  #####start initialize deadWood volume
  ## identify managed and unmanaged forests
  manFor <-  which(sampleX$cons==0)
  unmanFor <- which(sampleX$cons==1)
  if(outType=="ststDeadW"){
    unmanDeadW <- initDeadW(region,unmanFor,yearsDeadW)
    manDeadW <- initDeadW(region,manFor,yearsDeadW)
    save(unmanDeadW,manDeadW,file=paste0("initDeadWVss/reg",
                                         r_no,"_deadWV.rdata"))
    return("deadWood volume at steady state saved")
  }else{
    load(paste0("initDeadWVss/reg",
                r_no,"_deadWV.rdata"))
    deadWx <- aperm(replicate(length(manFor),(manDeadW$ssDeadW[1:nYears,])),c(3,1:2))
    region$multiOut[manFor,,8,1:3,1] <- region$multiOut[manFor,,8,1:3,1] + deadWx
    deadWx <- aperm(replicate(length(unmanFor),(unmanDeadW$ssDeadW[1:nYears,])),c(3,1:2))
    region$multiOut[unmanFor,,8,1:3,1] <- region$multiOut[unmanFor,,8,1:3,1] + deadWx
  }
  ####end initialize deadWood Volume
  
  # if(outType=="testRun"){
    # return(list(region = region,
    #             initPrebas=initPrebas,
    #             initSoil=initSoilC))
  # } 

