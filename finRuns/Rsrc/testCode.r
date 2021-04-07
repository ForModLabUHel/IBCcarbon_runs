r_no <- regions <- 1
sampleID <- 10
manScen <- "Base"

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

# setX=1
nSitesRun = 20000
nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
# sampleIDs <- split(1:nSamples,             # Applying split() function
#                    cut(seq_along(1:nSamples),
#                        nSetRuns,
#                        labels = FALSE))[[setX]]
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))




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
rcpfile = rcps
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
  areas <- data.sample$area
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
  harscen = harvestscenarios
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
    print(paste("runModel",sampleID))
    initSoilC <- stXX_GV(region, 1)
    print(paste("initSoilC",sampleID))
    region <- yassoPREBASin(region,initSoilC)
    print(paste("all runs done",sampleID))
    # out <- region$multiOut[,,,,1]
    

    
        ####create pdf for test plots
    if(sampleID==10){
      pdf(paste0("plots/testPlots_",r_no,".pdf"))
      out <- region$multiOut
      save(out,file = paste0("outputDT/forCent",r_no,"/testData.rdata"))
      rm(out);gc()
    }
    margin= 1:2#(length(dim(out$annual[,,varSel,]))-1)
    for (ij in 1:length(varSel)) {
      print(varSel[ij])
      if(funX[ij]=="baWmean"){
        outX <- data.table(segID=sampleX$segID,baWmean(region,varSel[ij]))
      }
      if(funX[ij]=="sum"){
        outX <- data.table(segID=sampleX$segID,apply(region$multiOut[,,varSel[ij],,1],margin,sum))
      }
      ####test plot
      # print(outX)
      if(sampleID==10){testPlot(outX,varNames[varSel[ij]],areas)}

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

    ####process and save special variales
    print("start special vars")
    specialVarProc(sampleX,region,r_no,harscen,rcpfile,sampleID,
                   colsOut1,colsOut2,colsOut3,areas)


