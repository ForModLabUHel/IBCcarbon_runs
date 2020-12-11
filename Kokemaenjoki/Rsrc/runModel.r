## ---------------------------------------------------------------------
## MAIN SCRIPT
## ---------------------------------------------------------------------

###runs settings###
harvestLims <- c(9775000,1466000)
HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
year1harv=1
domSPrun = 0
nSample = nrow(sampleX)#200#nrow(data.all)
startingYear = 2015
nYears = 2099-startingYear
harvestscenarios = c("Base","Low","MaxSust") #c("Low","MaxSust")#Base"#Low"#c("MaxSust", "Base","NoHarv") ## noharv must be the last element otherwise cons area are ignored
## Loop climate scenarios
rcps = "CurrClim.rdata" #c("CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")#c("CurrClim","CanESM2.rcp26.rdata")#,"CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")

# rcps = dir(climatepath, pattern = "*[0-9].rdata")
regions = 4 # c(1:2,11:15)
r_no=4
## Loop management scenarios
# harvestscenarios = c("Policy", "MaxSust", "Base","Low","Tapio","NoHarv") ## noharv must be the last element otherwise cons area are ignored
WRITEREGIONDATA = TRUE

climatepath = "/scratch/project_2000994/RCP/"
rempast = fread('/scratch/project_2000994/PREBASruns/metadata/Luke_Met_Poistuma_01.csv')
rempast = rempast[Puutavaralaji %in% c('Kaikki puutavaralajit','Energiapuu')]
rempast = rempast[Metsakeskusalue != "KOKO MAA"]
rempast = rempast[Vuosi < 2014]
rempast = rempast[, NFIcode:=tapply(Metsakeskusalue,1:dim(rempast)[1],
                                    function(x) strsplit(x, " ")[[1]][1])][, c(1, 4, 5, 6)]
colnames(rempast)[3] = "VOL"
foo = rempast[Puutavaralaji == "Kaikki puutavaralajit", VOL] - rempast[Puutavaralaji == "Energiapuu", 0.52*VOL]
rempast[Puutavaralaji == "Kaikki puutavaralajit", rem:=foo]
rempast = rempast[!is.na(rem)]
rempast = rempast[, mean(VOL), by=.(NFIcode)]

## GET removals (according to MELA, mill. m3)
# rem = fread('lukeInputs/EIS2016_realised_MELA_removals.csv')
rem = fread('/scratch/project_2000994/PREBASruns/metadata/EIS2016_realised_MELA_removals.csv')

## LOAD REGION NFI-DATA
# nfiareas = fread("lukeInputs/forest_centres.txt")
nfiareas = fread("/scratch/project_2000994/PREBASruns/metadata/forest_centres.txt")
## Not sure if also other forestry land should be here (mets?tiet, varastot ym.)
nfiareas[, AREA:=Metsamaa_1000ha]
nfiareas[, VOL:=Vol_mill_m3*1.1]
nfiareas[NFIcode %in% c('1a', '0', '2','3','4','5','6'), Region:="South"]
nfiareas[NFIcode %in% c('11','12','13'), Region:="North"]
nfiareas[is.na(Region), Region:='Middle']
nfiareas[, VOL_fraction:=VOL/sum(VOL), by=.(Region)]

nfiareas$drain_avg1990_2013 = c(65.45833333, 746.2083333, 5011.958333, 5870.916667, 4703.958333, 18251.83333, 3610.416667, 2369.208333, 1609.791667, 5725.25, 4322.625, 4809.083333, 1909.833333, 3909.833333, 6056.333333)
bigregiondrain = nfiareas[, sum(drain_avg1990_2013), by = Region]
colnames(bigregiondrain) = c('Area','1990-2013')
rem = merge(rem, bigregiondrain)

# regionsummaries = data.table()


## ---------------------------------------------------------
i = 0
# load("/scratch/project_2000994/PREBASruns/metadata/initSoilCstst.rdata")
load("outSoil/InitSoilCstst_Base.rdata")
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
