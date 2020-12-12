## ---------------------------------------------------------------------
## FUNCTIONS
## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## MAIN SCRIPT
## ---------------------------------------------------------------------
runModel <- function(sampleID){
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
    print(date())
    print(rcpfile)
    if(rcpfile=="CurrClim.rdata"){
      load(paste(climatepath, rcpfile, sep=""))  
      #####process data considering only current climate###
      # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
      maxRday <- max(dat$rday)
      xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
      dat = rbind(dat,dat,dat)
      dat[,rday:=xday]
      
    } else{
      load(paste(climatepath, rcpfile, sep=""))  
    }
    # load("C:/Users/minunno/Documents/research/lukeDB/example #2/CanESM2.rcp45.rdata")
    
    ## Loop regions -------------------------------------------------------
    for (r_no in regions) {
      print(date())
      print(paste("Region", r_no) )
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
      # data.all <- data.evo
      # data.all <- fread("raster/data.procEVO.txt")
      # data.all <- fread("EVOarea/data.procEVO.txt")
      # data.all <- data.all[sampleX]
      ## Prepare the same initial state for all harvest scenarios that are simulated in a loop below
      data.sample = sample_data.f(sampleX, nSample)
      if(rcpfile=="CurrClim.rdata") data.sample$id <- data.sample$CurrClimID
      totAreaSample <- sum(data.sample$area)
      # data.sample = rbind(data.sample,sample_data.f(data.all[sampleX], nSample))
      # if(r_no==1) data.sample[which(id==1350),id:=1351]#####!!!!!!region 1 do not macth climID 1350 in dat
      # if(r_no==3) data.sample[which(id==5174),id:=5175]#####!!region3 5174 ->5175
      # if(r_no==12) data.sample[which(id==6689),id:=6688]#####!!region12 6689 -> 6688
      # if(r_no==15){
      #   data.sample[which(id==1726),id:=1725]#####!!region15 1726 -> 1725
      #   data.sample[which(id==1240),id:=1241]#####!!region15 1240 -> 1241
      # }
      # yclim_allYears = yasso.mean.climate.f(dat, data.sample, 1980, 119)
      # 
      # # fwrite(yclim_allYears, file=paste('results_evo/mean.climate.', r_no, ".nfi", nfiareas[ID == r_no, NFIcode], ".",
      # #                                   rcpfile, '.txt', sep=""))
      # yclim =  yclim_allYears[Year >= 1990 & Year < startingYear] ## For Yasso
      # rm(yclim_allYears)
      
      clim = prep.climate.f(dat, data.sample, startingYear, nYears)
      
      Region = nfiareas[ID==r_no, Region]
      # harscen="Base"
      # HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-startingYear"]
      # HarvLim0  = (nSample) / nfiareas[ID == r_no, AREA] * HarvLim0
      # ## First, estimate the soil SS: climate is mean from 1980-2009, forest is startingYear forest, and harvest is
      # ## Base scenarios harvest
      # initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = 1,
      #                                    startingYear = startingYear)
      # #initPrebas$nYears = rep(1, length(initPrebas$nYears))
      # system.time(region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim0), minDharv = 15.0))
      # # initPrebas$nYears = rep(nYears, length(initPrebas$nYears))
      # 
      # L = region$multiOut[, 1, c(4, 26:29), , 1]
      # L = as.data.table(adply(L, c(1,3)))
      # L = L[, .(sum(Litter_fol + Litter_fr),
      #           sum(Litter_branch), sum(Litter_wood)), by=.(X1, species)]
      # L = L[, .(mean(V1), mean(V2), mean(V3)), by=species][species!=0]
      # L = cbind(L, c(0, 2, 30), c(0, 2, 30), c(0, 2, 10), c(1, 2, 3))[, -1]
      # 
      # S = soilCstst(L, as.numeric(yclim[, mean(Tmean)]), as.numeric(yclim[, mean(Tampl)]),
      #               as.numeric(yclim[, mean(Precip)]),species = 1:3)
      # S = apply(S,c(1,2),sum)
      
      ## Second, continue now starting from soil SS
      # initPrebas$nYears = nYears
      # data.sample = sample_data.f(sampleX, nSample)
      # if(r_no==1) data.sample[which(id==1350),id:=1351]#####!!!!!!region 1 do not macth climID 1350 in dat
      # if(r_no==3) data.sample[which(id==5174),id:=5175]#####!!region3 5174 ->5175
      # if(r_no==12) data.sample[which(id==6689),id:=6688]#####!!region12 6689 -> 6688
      # if(r_no==15){
      #   data.sample[which(id==1726),id:=1725]#####!!region15 1726 -> 1725
      #   data.sample[which(id==1240),id:=1241]#####!!region15 1240 -> 1241
      # }
      initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                         startingYear = startingYear,domSPrun=domSPrun)
      ###set parameters
      #    initPrebas$pCROBAS <- pCROBAS
      
      # #####old parameters for new version
      # pX <- pCROB; pX[12,] <- pCROB[12,]-1
      # pX[31,] <- 0.
      # pX[c(8,9),1] <- c(0.4,0.5)
      # pX[c(8,9),2] <- c(0.4,0.5)
      # pX[c(8,9),3] <- c(0.4,0.5)
      # pX[21,1] <- c(0.4) #alfar1 pine
      # pX[22,1] <- c(0.44) #alfar2 pine
      # alfar3                      0.47000000  3.800000e-01    0.64000000
      # alfar4                      0.64000000  4.800000e-01    0.75000000
      # alfar5                      0.84000000  5.800000e-01    0.94000000
      # initPrebas$pCROBAS <- pX
      # #####old parameters for new version END
      
      
      opsna <- which(is.na(initPrebas$multiInitVar))
      initPrebas$multiInitVar[opsna] <- 0.
      
      # initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
      # initSoil[,,1,,1] <- initSoilCstst[[r_no]]
      # initSoil <- aperm(initSoil,c(4,5,1:3))
      # initPrebas$soilC <- initSoil
      initPrebas$soilC[,1,,,] <- soilCststXX[[sampleID]]$soilC
      
      ##here mix years for weather inputs for Curr Climate
      if(rcpfile=="CurrClim.rdata"){
        set.seed(10)
        resampleYear <- sample(1:nYears,nYears)
        initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
        initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
        initPrebas$weather <- initPrebas$weather[,resampleYear,,]
        initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
      }
      
      
      # Loop management scenarios ------------------------------------------------
      for(harscen in harvestscenarios) { ## MaxSust fails, others worked.
        print(date())
        print(harscen)
        i = i + 1
        print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
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
        } 
        # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
        # HarvLim1 <- rep(0,2)
        # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
        system.time(region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1),minDharv = 1.))
        # out <- region$multiOut[,,,,1]
        out = list(annual=region$multiOut[,,,,1],
                   harvest=region$HarvLim,
                   energyWood = region$multiEnergyWood)
        
        save(out,file=paste0("output/",rcpfile,harscen,"_sample",sampleID,".rdata"))
        rm(region); gc()
        rm(out); gc()
        # out = simSummary.f(region=region, r_no, nYears, startingYear, rcpfile, harscen)
        # fwrite(out, file = paste("results_evo/regionSummaries", sampleID , "_",
        #                          harscen,"_", rcpfile,".txt", sep="" ), append=TRUE)
        # out = list(annual=region$multiOut[, , -c(1, 2, 19, 20), , 1],
        #            dailysample=region$dailyPRELES[floor(seq(1, nSample,
        #                                                     len=min(c(nSample/100), 100))), , ])
        # if (WRITEREGIONDATA) save(out,
        #                           file = paste("results_evo/regionOutput_", sampleID, "_",
        #                                        harscen,"_", rcpfile, sep="" ))
        # 
        # tabOut <- data.table(melt(out$annual))
        # tabOut[,Var4:=as.numeric(Var4)]
        # setnames(tabOut,c("siteID","year","output","layer","value"))
        # 
        # if (WRITEREGIONDATA) fwrite(tabOut,file = paste("results_evo/EvoTab_", 
        #                                                 sampleID, harscen,"_", rcpfile,".txt", sep="" ))
      }
    }
  }
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
  siteInfo[,1] <- 1:nSites
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

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # StartingYear = climate data that detrermines simulation period must have year greater than this.
# create_prebas_input.f = function(r_no, clim, data.sample, nYears, startingYear=0) { # dat = climscendataset
#   
#   nSites <- nrow(data.sample)
#   
#   ###site Info matrix. nrow = nSites, cols: 1 = siteID; 2 = climID; 3=site type;
#   ###4 = nLayers; 5 = nSpecies;
#   ###6=SWinit;   7 = CWinit; 8 = SOGinit; 9 = Sinit
#   
#   siteInfo <- matrix(c(NA,NA,NA,160,0,0,20,3,3),nSites,9,byrow = T)
#   #siteInfo <- matrix(c(NA,NA,NA,3,3,160,0,0,20),nSites,9,byrow = T)
#   siteInfo[,1] <- 1:nSites
#   siteInfo[,2] <- data.sample[,id]
#   siteInfo[,3] <- data.sample[,fert]
#   
#   litterSize <- matrix(0,3,3)
#   litterSize[1,1:2] <- 30
#   litterSize[1,3] <- 10
#   litterSize[2,] <- 2
#   
#   ###Initialise model
#   # initVardension nSites,variables, nLayers
#   # variables: 1 = species; 2 = Age; 3 = H; 4=dbh; 5 = ba; 6 = Hc
#   initVar <- array(NA, dim=c(nSites,6,3))
#   data.sample[,baP:= (ba * pine/(pine+spruce+decid))]
#   data.sample[,h:= h/10]
#   data.sample[,N:=ba/(pi*(dbh/2)^2/10000)]
#   data.sample[,VforHc:= N*(pi*(dbh/2)^2/10000)*h/3]
#   data.sample[,hc:= pmax(0,HcMod(h,dbh,N,ba,VforHc,age))]
#   
#   initVar[,1,] <- as.numeric(rep(1:3,each=nSites))
#   initVar[,2,] <- as.numeric(data.sample[,age])
#   initVar[,3,] <- as.numeric(data.sample[,h])
#   initVar[,3,][which(initVar[,3,]<1.5)] <- 1.5  ####if H < 1.5 set to 1.5
#   
#   initVar[,4,] <- as.numeric(data.sample[,dbh])
#   # initVar[,5,1] <- as.numeric(data.sample[,(ba * pine/(pine+spruce+decid))])
#   # initVar[,5,2] <- as.numeric(data.sample[,(ba * spruce/(pine+spruce+decid))])
#   # initVar[,5,3] <- as.numeric(data.sample[,(ba * decid/(pine+spruce+decid))])
#   initVar[,5,] = 0.
#   ix = unlist(data.sample[, which.max(c(pine, spruce, decid)), by=1:nrow(data.sample)] [, 2])
#   for(jx in 1:nSites) initVar[jx,5,ix[jx]] = as.numeric(data.sample[, ba])[jx]
#   initVar[,6,] <- as.numeric(data.sample[,hc])
#   
#   ###check which BA ==0. and set to 0 the rest of the variables
#   NoPine <- which(initVar[,5,1]==0.)
#   NoSpruce <- which(initVar[,5,2]==0.)
#   NoDecid <- which(initVar[,5,3]==0.)
#   
#   siteInfo[NoPine,8] <- siteInfo[NoPine,8] - 1
#   siteInfo[NoSpruce,8] <- siteInfo[NoSpruce,8] - 1
#   siteInfo[NoDecid,8] <- siteInfo[NoDecid,8] - 1
#   
#   #siteInfo[NoPine,4] <- siteInfo[NoPine,4] - 1
#   #siteInfo[NoSpruce,4] <- siteInfo[NoSpruce,4] - 1
#   #siteInfo[NoDecid,4] <- siteInfo[NoDecid,4] - 1
#   initVar[NoPine,3:6,1] <- 0.
#   initVar[NoSpruce,3:6,2] <- 0.
#   initVar[NoDecid,3:6,3] <- 0.
#   initVar[NoSpruce,,2] <- initVar[NoSpruce,,3]
#   initVar[NoPine,,1:2] <- initVar[NoPine,,2:3]
#   
#   # initVar[which(initVar[,5,1]==0.),,1] <- initVar[which(initVar[,5,1]==0.),,2]
#   # initVar[which(initVar[,5,1]==0.),,2] <- initVar[which(initVar[,5,1]==0.),,3]
#   # initVar[which(initVar[,5,1]==0.),1,3] <- 1
#   # initVar[which(initVar[,5,1]==0.),3:6,3] <- 0
#   
#   if (FALSE) {
#     dat = dat[id %in% data.sample[, unique(id)]]
#     
#     
#     dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
#     dat[, DOY:= as.numeric(format(pvm, "%j"))]
#     dat[, Year:= as.numeric(format(pvm, "%Y"))]
#     dat = dat[Year >= startingYear]
#     dat[DOY==366, DOY:=365]
#     
#     
#     PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
#                        value.var="PAR")[, -1])
#     TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
#                         value.var="TAir")[, -1])
#     VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
#                        value.var="VPD")[, -1])
#     Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
#                           value.var="Precip")[, -1])
#     CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
#                        value.var="CO2")[, -1])
#   }
#   siteInfo[, 2]  = match(siteInfo[, 2], as.numeric(rownames(clim[[1]])))
#   
#   defaultThin=as.numeric(1-data.sample[, cons])
#   ClCut = as.numeric(1-data.sample[, cons])
#   ## Set to match climate data years
#   
#   initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),siteInfo=siteInfo,
#                               litterSize = litterSize,#pAWEN = parsAWEN,
#                               defaultThin=defaultThin,
#                               ClCut = ClCut,
#                               multiInitVar = as.array(initVar),
#                               PAR = clim[[1]][, 1:(nYears*365)],
#                               TAir=clim[[2]][, 1:(nYears*365)],
#                               VPD=clim[[3]][, 1:(nYears*365)],
#                               Precip=clim[[4]][, 1:(nYears*365)],
#                               CO2=clim[[5]][, 1:(nYears*365)],
#                               yassoRun = 1,lukeRuns = 1.)
#   initPrebas
# }
# 
# yasso.mean.climate.f = function(dat, data.sample, startingYear, nYears){
#   dat = dat[id %in% data.sample[, unique(id)]]
#   dat[, DOY:=rep(1:365, len=dim(dat)[1])]
#   dat[, Year:=rep(1980:2099, each=365)]
#   #dat[, Year:= as.numeric(format(pvm, "%Y"))]
#   dat = dat[Year >= startingYear & Year <= startingYear+nYears]
#   dat[, pvm:= as.Date(paste(Year, '-01-01', sep="")) - 1 + DOY ]
#   #dat[, DOY:= as.numeric(format(pvm, "%j"))]
#   dat[, Mon:= as.numeric(format(pvm, "%m"))]
#   #dat[DOY==366, DOY:=365]
#   Tmean = dat[, mean(TAir), by = Year]
#   Tsum = dat[, sum(ifelse(TAir>5, TAir-5, 0)), by=.(id, Year)][, mean(V1), by=Year]
#   PAR = dat[, mean(PAR), by = Year]
#   VPD = dat[, mean(VPD), by = Year]
#   CO2 = dat[, mean(CO2), by = Year]
#   Precip = dat[, sum(Precip), by = .(id, Year)][, mean(V1), by=Year]
#   Tampl = dat[, .(mean(TAir)), by = .(id, Year, Mon)][, (max(V1)-min(V1))/2, by=Year]
#   
#   out = cbind(Tmean, Precip[, -1], Tampl[, -1], CO2[, -1], PAR[, -1], VPD[, -1], Tsum[, -1])
#   colnames(out) = c('Year','Tmean','Precip','Tampl', 'CO2', "PAR", "VPD", "Tsum5")
#   out
# }
# 
# 
# prep.climate.f = function(dat, data.sample, startingYear, nYears){
#   dat = dat[id %in% data.sample[, unique(id)]]
#   dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
#   dat[, DOY:= as.numeric(format(pvm, "%j"))]
#   dat[, Mon:= as.numeric(format(pvm, "%m"))]
#   dat[, Year:= as.numeric(format(pvm, "%Y"))]
#   dat = dat[Year >= startingYear & Year <= startingYear+nYears]
#   dat[DOY==366, DOY:=365]
#   
#   PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
#                      value.var="PAR")[, -1])
#   TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
#                       value.var="TAir")[, -1])
#   VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
#                      value.var="VPD")[, -1])
#   Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
#                         value.var="Precip")[, -1])
#   CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
#                      value.var="CO2")[, -1])
#   list(PARtran, TAirtran, VPDtran, Preciptran, CO2tran)
# }
# 
# 
# simSummary.f = function(region=region, r_no, nYears, startingYear, rcpfile, harscen) {
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
#   data.table(r_no, rcpfile, harscen, year=startingYear + (1:nYears),
#              VOL, VOL_INAREA, Vharvested, Vmort, Vmort_INAREA,
#              grossgrowth_INAREA, dbh, age, gpp, npp, nep, B_tree, lproj)
# }
