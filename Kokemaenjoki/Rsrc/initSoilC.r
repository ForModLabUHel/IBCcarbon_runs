# if (!require(Rprebasso)) {
#   devtools::install_github("checcomi/Rprebasso", force=TRUE,ref="master")
#   #install.packages("C:/Users/peltonie/Downloads/prebassoInprog-master (3)/prebassoInprog-master",  type="source", repos=NULL)
#   require(Rprebasso)
# }


  # regiondatapath = "/wrk/mpeltoni/DONOTREMOVE/Ilmastopaneeli_toprocess/"
  # massivedatapath = "/wrk/mpeltoni/DONOTREMOVE/Ilmastopaneeli_toprocess/codeshare/results_v6.2/CurrClim/"

  setwd(pathtoken)
  
  
  
# setwd("C:/Users/minunno/Documents/research/lukeDB/example #2")




## ---------------------------------------------------------------------
## MAIN SCRIPT
## ---------------------------------------------------------------------

rempast = fread('/wrk/mpeltoni/DONOTREMOVE/Ilmastopaneeli_toprocess/Luke_Met_Poistuma_01.csv')
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
rem = fread('EIS2016_realised_MELA_removals.csv')

## LOAD REGION NFI-DATA
nfiareas = fread("forest_centres.txt")

## nfiareas[, AREA:=Metsamaa_1000ha+Kitumaa_1000ha+Joutomaa_1000ha]

## 21.9.2018: AREA where harvests are allocated (used for total harvest rescaling for the sample of sites
## that are simulated) now contains only forest land (which)
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

nSample = 10000
startingYear = 2013
nYears = 2063-2013
regionsummaries = data.table()

## FOR TESTING
if (FALSE) {
  nSample = 100
  r_no = 7
  rcpfile = "CanESM2.rcp26.rdata"
  harscen = "Policy"
  load(paste(climatepath, rcpfile, sep=""))
}

## ---------------------------------------------------------
## Loop climate scenarios
rcps = dir(climatepath, pattern = "*[0-9].rdata")
regions = 1:15
harvestscenarios = c("Policy", "MaxSust", "Base","Low","Tapio","NoHarv") ## noharv must be the last element otherwise cons area are ignored
WRITEREGIONDATA = TRUE
i = 0
ststSoilC <- list()

# for (rcpfile in rcps[1]) { ## ---------------------------------------------
  rcpfile=rcps[1]
  # print(date())
  # print(rcpfile)
  # rcpfile = "CanESM2.rcp26.rdata"
  load(paste(climatepath, rcpfile, sep=""))
  
  #####process data considering only current climate###
  dat <- dat[rday %in% 1:10958]
  xday <- c(dat$rday,(dat$rday+10958),(dat$rday+10958*2),(dat$rday+10958*3))
  dat = rbind(dat,dat,dat,dat)
  dat[,rday:=xday]
  
  ## Loop regions -------------------------------------------------------
  for (r_no in regions) {
    if(r_no==12) nSample = 15000
    if(r_no==13) nSample = 4206
    if(r_no %in% c(1:11,14,15)) nSample= 10000
    print(date())
    print(paste("Region", r_no) )
    # r_no=7
    ## Load samples from regions; region-files include every 1000th pixel
    ## Pixel data are from 16 m x 16 m cells, but all numbers are per unit area.
    ## Model also produces per values  per hectar or m2.
    ## Note also that some of the pixels are non-forest (not metsamaa, kitumaa, joutomaa)
    ## or not inside Finland (32767) or may be cloudcovered (32766).
    
    data.all = fread(paste(regiondatapath, "data.proc.v6.", r_no, ".txt", sep=""))
    # data.all = fread(paste("data.proc.", r_no, ".txt",sep=""))
    #dat = dat[id %in% data.all[, unique(id)]]
    gc()
    
    ## Prepare the same initial state for all harvest scenarios that are simulated in a loop below
    data.sample = sample_data.f(data.all, nSample)
    if(r_no==1) data.sample[which(id==1350),id:=1351]#####!!!!!!region 1 do not macth climID 1350 in dat
    if(r_no==3) data.sample[which(id==5174),id:=5175]#####!!region3 5174 ->5175
    if(r_no==3) data.sample[which(id==5416),id:=5417]#####!!region3 5174 ->5175
    if(r_no==11) data.sample[which(id==3665),id:=3667]#####!!region3 5174 ->5175
    if(r_no==12) data.sample[which(id==6689),id:=6688]#####!!region12 6689 -> 6688
    if(r_no==15){
      data.sample[which(id==1726),id:=1725]#####!!region15 1726 -> 1725
      data.sample[which(id==1240),id:=1241]#####!!region15 1240 -> 1241
    }
    yclim_allYears = yasso.mean.climate.f(dat, data.sample, 1980, 119)
    
    # fwrite(yclim_allYears, file=paste(massivedatapath,'mean.climate.', r_no, ".nfi", nfiareas[ID == r_no, NFIcode], ".",
    # rcpfile, '.txt', sep=""))
    yclim =  yclim_allYears[Year >= 1990 & Year < 2013] ## For Yasso
    rm(yclim_allYears)
    
    clim = prep.climate.f(dat, data.sample, startingYear, nYears)
    
    Region = nfiareas[ID==r_no, Region]
    harscen="Base"
    HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
    HarvLim0  = (nSample) / nfiareas[ID == r_no, AREA] * HarvLim0
    ## First, estimate the soil SS: climate is mean from 1980-2009, forest is 2013 forest, and harvest is
    ## Base scenarios harvest
    initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = 1,
                                       startingYear = startingYear)
    #initPrebas$nYears = rep(1, length(initPrebas$nYears))
    system.time(region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim0), minDharv = 15.0))
    # initPrebas$nYears = rep(nYears, length(initPrebas$nYears))
    
    L = region$multiOut[, 1, c(1,3,4, 26:29), , 1]
    # L = as.data.table(adply(L, c(1,3)))
    L = as.data.table(L)
    # L = L[, .(sum(Litter_fol + Litter_fr),
    # sum(Litter_branch), sum(Litter_wood)), by=.(sitetype, species)]
    L = L[, .(mean(Litter_fol + Litter_fr), mean(Litter_branch),
              mean(Litter_wood)), by=species][species!=0]
    L = L[order(species)]
    L = cbind(L, c(0, 2, 30), c(0, 2,30), c(0,2,10), c(1, 2, 3))[, -1]
    
    S = soilCstst(L, as.numeric(yclim[, mean(Tmean)]), as.numeric(yclim[, mean(Tampl)]),
                  as.numeric(yclim[, mean(Precip)]),species = 1:3)
    specProrp <- table(region$multiOut[,1,4,1,1])/length(region$multiOut[,1,4,1,1])
    for(jx in 1:3)    S[,,jx]  <- specProrp[jx]* S[,,jx]
    S = apply(S,c(1,2),sum)
    
    ## Second, continue now starting from soil SS
    initPrebas$nYears = nYears
    data.sample = sample_data.f(data.all, nSample)
    if(r_no==1) data.sample[which(id==1350),id:=1351]#####!!!!!!region 1 do not macth climID 1350 in dat
    if(r_no==3) data.sample[which(id==5174),id:=5175]#####!!region3 5174 ->5175
    if(r_no==3) data.sample[which(id==5416),id:=5417]#####!!region3 5174 ->5175
    if(r_no==11) data.sample[which(id==3665),id:=3667]#####!!region3 5174 ->5175
    if(r_no==12) data.sample[which(id==6689),id:=6688]#####!!region12 6689 -> 6688
    if(r_no==15){
      data.sample[which(id==1726),id:=1725]#####!!region15 1726 -> 1725
      data.sample[which(id==1240),id:=1241]#####!!region15 1240 -> 1241
    }
    initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                       startingYear = startingYear)
    
    initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
    initSoil[,,1,,1] <- S*0.9                         #####!!!!!decrease soilC of 10%
    initSoil <- aperm(initSoil,c(4,5,1:3))
    initPrebas$soilC <- initSoil
    
    ##here mix years for weather inputs
    set.seed(63)
    resampleYear <-  sample(1:30)
    set.seed(12)
    resampleYear <-  c(resampleYear,sample(1:30)+30)
    set.seed(34265)
    resampleYear <-  c(resampleYear,sample(1:26)+60)
    resampleYear <- resampleYear[resampleYear <= nYears]
    initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
    initPrebas$P0y <- initPrebas$P0y[,resampleYear]
    initPrebas$weather <- initPrebas$weather[,resampleYear,,]
    initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
    
    
    # harscen ="Base"
    ## Assign harvesting quota for the region based on volume (in NFI 2013) and MELA
    Region = nfiareas[ID==r_no, Region]
    if(harscen=="NoHarv"){
      initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      HarvLim1 = 0
    }else if(harscen=="Tapio"){
      HarvLim1 = 0
    }else{
      HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
      HarvLim0  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2015-2024"]
      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- rep(as.numeric(HarvLim),12)
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2025-2034"]
      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2035-2044"]
      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2045-2054"]
      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2055-2064"]
      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),44))
    }
    ## In the model, harvests are always per hectar units. If 1000 pixels (nSample)
    ## are simulated it corresponds to 1000 hectars, although pixels are only 16x16 m2.
    ## Therefore, we need to apply the areal fraction of removals scenarios
    ## nfiareas are in 1000 ha, model takes Harvlim in m3, while removals from Mela are 1000 m3
    #      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
    
    system.time(region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1), minDharv = 15.0))
    
    ststSoilC[[r_no]] <- region$soilC
  }
# }

save(ststSoilC,file="run_v6/ststSoilC.rdata")


initSoilCstst <- list()
for( i in 1:15){
  initSoilCstst[[i]] <- apply(ststSoilC[[i]][,50,,,],c(2:3),mean,na.rm=T)
}

save(initSoilCstst,file="run_v6/initSoilCstst.rdata")
