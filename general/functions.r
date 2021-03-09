## ---------------------------------------------------------------------
## FUNCTIONS
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## MAIN SCRIPT
## ---------------------------------------------------------------------
runModel <- function(sampleID){
  # print(date())
  print(paste("start sample ID",sampleID))
  sampleX <- ops[[sampleID]]
  sampleX[,area := N*16^2/10000]
  sampleX[,id:=climID]
  HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
  nSample = nrow(sampleX)#200#nrow(data.all)
  ## Loop management scenarios
  # harvestscenarios = c("Policy", "MaxSust", "Base","Low","Tapio","NoHarv") ## noharv must be the last element otherwise cons area are ignored
  # WRITEREGIONDATA = TRUE
  
  # climatepath = "/scratch/project_2000994/RCP/"
  
  # regionsummaries = data.table()
  
  
  ## ---------------------------------------------------------
  i = 0
  # load("/scratch/project_2000994/PREBASruns/metadata/initSoilCstst.rdata")
  # load("outSoil/InitSoilCstst_Base.rdata")
  for(rcpfile in rcps) { ## ---------------------------------------------
    # print(rcpfile)
    if(rcpfile=="CurrClim"){
      load(paste(climatepath, rcpfile,".rdata", sep=""))  
      #####process data considering only current climate###
      # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
      maxRday <- max(dat$rday)
      xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
      dat = rbind(dat,dat,dat)
      dat[,rday:=xday]
      
    } else{
      load(paste(climatepath, rcpfile,".rdata", sep=""))  
    }
    # load("C:/Users/minunno/Documents/research/lukeDB/example #2/CanESM2.rcp45.rdata")
    
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
      totAreaSample <- sum(data.sample$area)
      
      clim = prep.climate.f(dat, data.sample, startingYear, nYears)
      
      Region = nfiareas[ID==r_no, Region]
      
      ## Second, continue now starting from soil SS
      initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                         startingYear = startingYear,domSPrun=domSPrun)
      ###set parameters
      #    initPrebas$pCROBAS <- pCROBAS
      
      
      opsna <- which(is.na(initPrebas$multiInitVar))
      initPrebas$multiInitVar[opsna] <- 0.
      
      # initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
      # initSoil[,,1,,1] <- initSoilCstst[[r_no]]
      # initSoil <- aperm(initSoil,c(4,5,1:3))
      # initPrebas$soilC <- initSoil
      # if(exists("soilCststXX")) initPrebas$soilC[,1,,,] <- soilCststXX[[sampleID]]$soilC
      
      ##here mix years for weather inputs for Curr Climate
      if(rcpfile=="CurrClim"){
        set.seed(10)
        resampleYear <- sample(1:nYears,nYears)
        initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
        initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
        initPrebas$weather <- initPrebas$weather[,resampleYear,,]
        initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
      }
      
      
      # Loop management scenarios ------------------------------------------------
      for(harscen in harvestscenarios) { ## MaxSust fails, others worked.
        # print(date())
        # print(harscen)
        i = i + 1
        # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
        # harscen ="Base"
        ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
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
        }else{
          roundWood <- HarvLim1 * roundTotWoodRatio
          enWood <- HarvLim1 - roundWood
          HarvLim1 <- cbind(roundWood,enWood)
        }
          
        # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
        # HarvLim1 <- rep(0,2)
        # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
        # region <- regionPrebas(initPrebas)
        region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1),minDharv = 1.)
        initSoilC <- stXX_GV(region, 1)
        region <- yassoPREBASin(region,initSoilC)
        
        # out <- region$multiOut[,,,,1]
        
        margin= 1:2#(length(dim(out$annual[,,varSel,]))-1)
        for (ij in 1:length(varSel)) {
          if(funX[ij]=="baWmean"){
            outX <- data.table(segID=sampleX$segID,baWmean(region,varSel[ij]))
          }
          if(funX[ij]=="sum"){
            outX <- data.table(segID=sampleX$segID,apply(region$multiOut[,,varSel[ij],,1],margin,sum))
          }
          p1 <- outX[, .(per1 = rowMeans(.SD)), .SDcols = colsOut1, by = segID] 
          p2 <- outX[, .(per2 = rowMeans(.SD)), .SDcols = colsOut2, by = segID] 
          p3 <- outX[, .(per3 = rowMeans(.SD)), .SDcols = colsOut3, by = segID] 
          pX <- merge(p1,p2)
          pX <- merge(pX,p3)
          assign(varNames[varSel[ij]],pX)
          
          save(list=varNames[varSel[ij]],file=paste0("outputDT/forCent",r_no,"/",
                                                     varNames[varSel[ij]],"_",
                                                       harscen,"_",rcpfile,"_",
                                                     "sampleID",sampleID,".rdata"))
          rm(list=varNames[varSel[ij]]); gc()
        }  
        rm(list=c("region")); gc()
        # rm(out); gc()
      }
    # } ###region loop
  }
  print(paste("end sample ID",sampleID))
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
create_prebas_input.f = function(r_no, clim, data.sample, nYears, startingYear=0,domSPrun=0) { # dat = climscendataset
  #domSPrun=0 initialize model for mixed forests according to data inputs 
  #domSPrun=1 initialize model only for dominant species 
  nSites <- nrow(data.sample)
  
  ###site Info matrix. nrow = nSites, cols: 1 = siteID; 2 = climID; 3=site type;
  ###4 = nLayers; 5 = nSpecies;
  ###6=SWinit;   7 = CWinit; 8 = SOGinit; 9 = Sinit
  
  siteInfo <- matrix(c(NA,NA,NA,160,0,0,20,3,3,413,0.45,0.118),nSites,12,byrow = T)
  #siteInfo <- matrix(c(NA,NA,NA,3,3,160,0,0,20),nSites,9,byrow = T)
  siteInfo[,1] <- data.sample$segID
  siteInfo[,2] <- as.numeric(data.sample[,id])
  siteInfo[,3] <- data.sample[,fert]
  
  litterSize <- matrix(0,3,3)
  litterSize[1,1:2] <- 30
  litterSize[1,3] <- 10
  litterSize[2,] <- 2
  
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
  
  areas <- data.sample$area
  
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
    ####increase spruce dbh 10% for spruce sitetype 1:2
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,X:=(ba-1.1*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,dbhP:=X*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,dbhSP:=1.1*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5  & baSP > baP & dbhP<0.5,dbhSP:=((ba-(0.5/dbh)*baP-baB)/baSP)*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5  & baSP > baP & dbhP<0.5,dbhP:=0.5]
    
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhSP:=dbh * (ba - 0.9*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhP:=pmax(0.9*dbh,0.3)]
    
    ####increase spruce h 10% for spruce sitetype 1:2
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,X:=(ba-1.1*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,hP:=X*h]   
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,hSP:=1.1*h]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP & hP<1.5,hSP:=((ba-(1.5/h)*baP-baB)/baSP)*h]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP & hP<1.5,hP:=1.5]
    
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hSP:=h * (ba - 0.9*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hP:=pmax(0.9*h,1.3)]
    
    ####increase spruce dbh 5% for spruce sitetype 3
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,X:=(ba-1.05*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,dbhP:=X*dbh]   
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,dbhSP:=1.05*dbh]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP & dbhP<0.5,dbhSP:=((ba-(0.5/dbh)*baP-baB)/baSP)*dbh]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP & dbhP<0.5,dbhP:=0.5]
    
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhSP:=dbh * (ba - 0.95*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhP:=pmax(0.95*dbh,0.3)]
    
    ####increase spruce h 5% for spruce sitetype 3
    data.sample[pine>0. & spruce >0. & fert==3,X:=(ba-1.05*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert==3,hP:=X*h]  
    data.sample[pine>0. & spruce >0. & fert==3,hSP:=1.05*h]  
    data.sample[pine>0. & spruce >0. & fert==3 & hP<1.5,hSP:=((ba-(1.5/h)*baP-baB)/baSP)*h]
    data.sample[pine>0. & spruce >0. & fert==3 & hP<1.5,hP:=1.5]
    
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hSP:=h * (ba - 0.95*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hP:=pmax(0.95*h,1.3)]
    
    ####increase pine dbh 10% for sitetype >= 4
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,X:=(ba-1.1*baP-baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhSP:=X*dbh]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhP:=1.1*dbh]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & dbhSP<0.5,dbhP:=((ba-(0.5/dbh)*baSP-baB)/baP)*dbh]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & dbhSP<0.5,dbhSP:=0.5]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhP:=dbh * (ba - 0.9*baSP - baB)/baP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhSP:=pmax(0.9*dbh,0.3)]
    ####increase pine h 10% for sitetype >= 4
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,X:=(ba-1.1*baP-baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hSP:=X*h]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hP:=1.1*h]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & hSP<1.5,hP:=((ba-(1.5/h)*baSP-baB)/baP)*h]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & hSP<1.5,hSP:=1.5]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hP:=h * (ba - 0.9*baSP - baB)/baP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hSP:=pmax(0.9*h,1.3)]
    initVar[,3,1] <- as.numeric(data.sample[,hP])
    initVar[,3,2] <- as.numeric(data.sample[,hSP])
    initVar[,4,1] <- as.numeric(data.sample[,dbhP])
    initVar[,4,2] <- as.numeric(data.sample[,dbhSP])
    
  }
  
  # initVar[,6,] <- as.numeric(data.sample[,hc])
  
  ###check which BA ==0. and set to 0 the rest of the variables
  NoPine <- which(initVar[,5,1]==0.)
  NoSpruce <- which(initVar[,5,2]==0.)
  NoDecid <- which(initVar[,5,3]==0.)
  
  siteInfo[NoPine,8] <- siteInfo[NoPine,8] - 1
  siteInfo[NoSpruce,8] <- siteInfo[NoSpruce,8] - 1
  siteInfo[NoDecid,8] <- siteInfo[NoDecid,8] - 1
  
  #siteInfo[NoPine,4] <- siteInfo[NoPine,4] - 1
  #siteInfo[NoSpruce,4] <- siteInfo[NoSpruce,4] - 1
  #siteInfo[NoDecid,4] <- siteInfo[NoDecid,4] - 1
  initVar[NoPine,3:6,1] <- 0.
  initVar[NoSpruce,3:6,2] <- 0.
  initVar[NoDecid,3:6,3] <- 0.
  initVar[NoSpruce,,2] <- initVar[NoSpruce,,3]
  initVar[NoPine,,1:2] <- initVar[NoPine,,2:3]
  
  nLay1 <- which(siteInfo[,8]==1)
  nLay2 <- which(siteInfo[,8]==2)
  initVar[nLay1,3:6,2:3] <- 0
  initVar[nLay2,3:6,3] <- 0
  # initVar[which(initVar[,5,1]==0.),,1] <- initVar[which(initVar[,5,1]==0.),,2]
  # initVar[which(initVar[,5,1]==0.),,2] <- initVar[which(initVar[,5,1]==0.),,3]
  # initVar[which(initVar[,5,1]==0.),1,3] <- 1
  # initVar[which(initVar[,5,1]==0.),3:6,3] <- 0
  
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
  # siteInfo[, 2]  = match(siteInfo[,2], unique(dat$id))
  
  defaultThin=as.numeric(1-data.sample[, cons])
  energyCut <- ClCut <- as.numeric(1-data.sample[, cons])
  ## Set to match climate data years
  initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),siteInfo=siteInfo,
                              litterSize = litterSize,#pAWEN = parsAWEN,
                              defaultThin=defaultThin,
                              ClCut = ClCut, areas =areas,
                              energyCut = energyCut, 
                              multiInitVar = as.array(initVar),
                              PAR = clim$PAR[, 1:(nYears*365)],
                              TAir=clim$TAir[, 1:(nYears*365)],
                              VPD=clim$VPD[, 1:(nYears*365)],
                              Precip=clim$Precip[, 1:(nYears*365)],
                              CO2=clim$CO2[, 1:(nYears*365)],
                              yassoRun = 1)
  initPrebas
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


simSummary.f = function(region=region, r_no, nYears, startingYear, rcpfile, harscen) {
  
  out = region[['multiOut']]
  VOL = out[, , 30, , 1]
  VOL = apply(VOL, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  VOL = apply(VOL, 2, mean)
  ## Multiply by area (tha)
  VOL_INAREA = VOL * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  ## at the beginning 207.7 mill m3, vrt 189.9 according to NFI (for region 7 = Keski-Suomi)
  
  Vmort = out[, , 42, , 1]
  Vmort = apply(Vmort, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  Vmort = apply(Vmort, 2, mean)
  Vmort_INAREA = Vmort * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  
  ## WHY THIS IS NOT THE SAME AS har?
  Vharvested = out[, , 37, , 1]
  Vharvested = apply(Vharvested, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  Vharvested = apply(Vharvested, 2, mean)
  Vharvested_INAREA = Vharvested * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  grossgrowth = out[, , 43, , 1]
  grossgrowth = apply(grossgrowth, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  grossgrowth = apply(grossgrowth, 2, mean)
  grossgrowth_INAREA = grossgrowth * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  dbh = out[, , 12, , 1]
  dbh = apply(dbh, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  dbh = apply(dbh, 2, mean)
  
  age = out[, , 7, , 1]
  age = apply(age, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  age = apply(age, 2, mean)
  
  gpp = out[, , 10, , 1]
  gpp = apply(gpp, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  gpp = apply(gpp, 2, mean)
  #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
  
  
  npp = out[, , 18, , 1]
  npp = apply(npp, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  npp = apply(npp, 2, mean)
  #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
  
  
  nep = out[, , 46, , 1]
  nep = apply(nep, c(1,2), sum, na.rm=TRUE)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  nep = apply(nep, 2, mean)
  
  
  B_tree = out[, , 35, , 1]
  B_tree = apply(B_tree, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  B_tree = apply(B_tree, 2, mean)
  
  lproj = out[, , 21, , 1]
  lproj = apply(lproj, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  lproj = apply(lproj, 2, mean)
  data.table(r_no, rcpfile, harscen, year=startingYear + (1:nYears),
             VOL, VOL_INAREA, Vharvested, Vmort, Vmort_INAREA,
             grossgrowth_INAREA, dbh, age, gpp, npp, nep, B_tree, lproj)
}

# this function create maps in tif format from raw data.
createTif <- function(climate, management, yearOut, variable, species, startingYear){
  simYear <- yearOut - startingYear
  
  files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
  
  outX <- data.table()
  ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
  
  for(i in 1:length(files)){
    sampleID <- paste0("sample",i,".")
    
    fileX <- files[grep(sampleID,files,fixed = T)]
    
    load(paste0("output/",fileX))
    
    out <- data.table(out$annual[,simYear,variable,])
    
    set.seed(1)
    sampleX <- ops[[i]]
    sampleX[,area := N*16^2/10000]
    sampleX[,id:=climID]
    
    outX <- rbind(outX,cbind(sampleX$segID,out))
    print(i)
  }
  
  
  
  setnames(outX,c("segID","pine","spruce","birch"))
  
  outX[, tot := rowSums(.SD), .SDcols = c("pine","spruce","birch")]
  
  
  outXY <- merge(kokeIDsTab,outX,all = T)
  
  ###create raster 
  rastX <- rasterFromXYZ(outXY[,c("x","y",species),with=F])
  crs(rastX) <- crs(kokeShp)
  
  rastName <- paste0("outRast/",climate,"_",management,"_var",varNames[variable],
                     "_spec",species,"_year",yearOut,".tif")
  writeRaster(rastX,filename = rastName)
}

# this function create maps in tif format from data.tables selecting one year or the average of a time priod if yearOut is a vector of years
createTifFromDT <- function(climate, management, yearOut, variable, species, startingYear){
  simYear <- yearOut - startingYear
  fileDT=paste0("outputDT/",varNames[variable],"_",management,"_",climate,".rdata")  
  load(fileDT)
  
  outX <- t(get(varNames[variable]))
  if (length(simYear)==1) outX <- outX[simYear,]
  if (length(simYear)>1) outX <- colMeans(outX[simYear,],na.rm = T)
  
  segID <- areas <-numeric(0)
  set.seed(1)
  ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
  for(i in 1:115){
    # set.seed(1)
    sampleX <- ops[[i]]
    sampleX[,area := N*16^2/10000]
    sampleX[,id:=climID]
    segID <- c(segID,sampleX$segID)
    areas <- c(areas,sampleX$area)
    # print(i)
  }
  outX <- data.table(cbind(segID,areas,outX))
  
  setnames(outX,c("segID","areas",varNames[variable]))
  
  # outX[, tot := rowSums(.SD), .SDcols = c("pine","spruce","birch")]
  
  outXY <- merge(kokeIDsTab,outX,all = T)
  
  ###create raster 
  rastX <- rasterFromXYZ(outXY[,c("x","y",varNames[variable]),with=F])
  crs(rastX) <- crs(kokeShp)
  
  rastName <- paste0("outRast/",climate,"_",management,"_var",varNames[variable],
                     "_spec",species,"_year",min(yearOut),"_",max(yearOut),".tif")
  writeRaster(rastX,filename = rastName,overwrite=T)
}



##function to compile all data and create data.table 
createDT <- function(climate, management,variable, species, startingYear){
  
  files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
  
  for (ij in variable) assign(varNames[ij],data.table())
  VenergyWood <- WenergyWood <- data.table()
  
  # segID <- areas <-numeric(0)
  
  for(i in 1:length(files)){
    sampleID <- paste0("sample",i,".")
    
    fileX <- files[grep(sampleID,files,fixed = T)]
    
    load(paste0("output/",fileX))
    
    ###sum harvests
    if(i==1){
      harvest <- out$harvest
    }else{
      harvest <- harvest+out$harvest  
    }
    
    VenergyWood <- rbind(VenergyWood,apply(out$energyWood[,,,1],1:2,sum))
    WenergyWood <- rbind(WenergyWood,apply(out$energyWood[,,,2],1:2,sum))
    
    margin= 1:2#(length(dim(out$annual[,,variable,]))-1)
    for (ij in variable) {
     varIndx <- match(varNames[ij],varNames[varSel])  
     assign(varNames[ij],data.table(rbind(eval(parse(text = varNames[ij])),
                 apply(out$annual[,,varIndx,],margin,sum))))
    }    
    print(i)
  }
  
  ###proc and save total harvests
  totHarvest <- data.table(harvest)
  setnames(totHarvest,c("roundWood","energyWood"))
  save(totHarvest,file=paste0("outputDT/","totHarvest","_",management,"_",climate,".rdata"))
  
  save(VenergyWood,file=paste0("outputDT/","VenergyWood","_",management,"_",climate,".rdata"))
  save(WenergyWood,file=paste0("outputDT/","WenergyWood","_",management,"_",climate,".rdata"))
  
  
  for(ij in variable) save(list=varNames[ij],file=paste0("outputDT/",varNames[ij],"_",management,"_",climate,".rdata"))
}

##function to compile all data and create data.table by species
createDTbySp <- function(climate, management,variable, species, startingYear){
  
  files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
  
  for (ij in variable){
    assign(paste0(varNames[ij],1),data.table())
    assign(paste0(varNames[ij],2),data.table())
    assign(paste0(varNames[ij],3),data.table())
  }
  # segID <- areas <-numeric(0)
  
  for(i in 1:length(files)){
    sampleID <- paste0("sample",i,".")
    
    fileX <- files[grep(sampleID,files,fixed = T)]
    
    load(paste0("output/",fileX))
    
    margin= 1:2#(length(dim(out$annual[,,variable,]))-1)
    for (ij in variable){
      assign(paste0(varNames[ij],1),
             data.table(rbind(eval(parse(text = paste0(varNames[ij],1))),
                              out$annual[,,ij,1])))
      assign(paste0(varNames[ij],2),
             data.table(rbind(eval(parse(text = paste0(varNames[ij],2))),
                              out$annual[,,ij,2])))
      assign(paste0(varNames[ij],3),
             data.table(rbind(eval(parse(text = paste0(varNames[ij],3))),
                              out$annual[,,ij,3])))
    } 
    
    print(i)
  }
  
  for(ij in variable){
    save(list=c(paste0(varNames[ij],1),paste0(varNames[ij],2),paste0(varNames[ij],3)),
         file=paste0("outputDT/",varNames[ij],"_",management,"_",climate,"_bySpecies.rdata"))
  } 
}


# this function compute the annual totals of the region from data.tables 
aTOTfromDT <- function(yearOut, variable, species="tot", startingYear){
  simYear <- yearOut - startingYear
  segID <- areas <-numeric(0)
  set.seed(1)
  ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
  for(i in 1:115){
    sampleX <- ops[[i]]
    sampleX[,area := N*16^2/10000]
    sampleX[,id:=climID]
    segID <- c(segID,sampleX$segID)
    areas <- c(areas,sampleX$area)
    # print(i)
  }
  files <- list.files("outputDT/",pattern=paste0(varNames[variable],"_"))
  if (species=="tot") files <- files[-grep("bySpecies",files)]
  allOut <- data.table()
  for(i in 1:length(files)){
    load(paste0("outputDT/",files[i]))
    
    dats <- strsplit(files[i], "[_.]+")
    if(variable %in% 32:33) dats[[1]] <- dats[[1]][-2]
    climate=dats[[1]][3]
    management=dats[[1]][2]
    outX <- get(varNames[variable])*areas
    outX <- colMeans(outX,na.rm=T)
    outX <- data.table(cbind(outX,climate,management))
    outX[,year:=yearOut]
    allOut <- rbind(allOut,outX)
  }
  
  setnames(allOut,c(varNames[variable],"climate","management","year"))
  allOut$climate <- factor(allOut$climate)
  allOut$management <- factor(allOut$management)
  allOut[,1] <- as.numeric(unlist(allOut[,1]))
  
  fwrite(allOut,file= paste0("plots/",varNames[variable],"_DT.txt"))  
  
  p <- ggplot(data=allOut, 
              aes_string(x="year", y=varNames[variable])) +
    # scale_shape_manual(values=1:nlevels(countryTot$harvScenario)) +
    labs(title = varNames[variable])+
    # geom_smooth() +
    xlab("Year") +
    ylab("") +
    geom_point(aes(colour=management, shape = climate,group=interaction(management, climate))) +
    geom_line(aes(colour=management, group=interaction(management, climate)))
  
  pdf(file=paste0("plots/",varNames[variable], ".pdf"))
  print(p)
  dev.off()
}

###compute total biomass from DTs
Wtot <- function(manClim){
  files <- paste0("outputDT/",varNames[c(24:25,31:33)],manClim)
  for(i in 1:5) load(files[i])
  Wtot <- Wstem + W_croot + wf_STKG + Wbranch + WfineRoots
  save(Wtot,file = paste0("outputDT/","Wtot",manClim))
}


createPlotfromDT <- function(path, variable){
  DT <- fread(paste0(path,varNames[variable],"_DT.txt"))
  p <- ggplot(data=DT, 
              aes_string(x="year", y=varNames[variable])) +
    # scale_shape_manual(values=1:nlevels(countryTot$harvScenario)) +
    labs(title = varNames[variable])+
    # geom_smooth() +
    xlab("Year") +
    ylab("") +
    geom_point(aes(colour=management, shape = climate,group=interaction(management, climate))) +
    geom_line(aes(colour=management, group=interaction(management, climate)))
  
  png(file=paste0("plots/",varNames[variable], ".png"),width = 500,height = 500)
  print(p)
  dev.off()
}

calMean <- function(varX,hscenX,areas){
  load(paste0("outputDT/",varX,"_",hscenX,"_CurrClim.rdata"))
  varAreas <- get(varX)*areas
  # Vareas <- Vareas[-siteX]
  totX <- colSums(varAreas,na.rm = T)
  meanX <- totX/sum(areas)#co
  return(meanX)
}
