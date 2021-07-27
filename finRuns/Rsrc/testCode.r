# for(sampleID in 1:73){
  

r_no <- regions <- 1
sampleID <- 1#498 #136
manScen <- "Base"
regSets = "maakunta"

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


###check for NAS
# load("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent12/NApoints/NApoints2017-2025.rdata")
# rm(NA_points);gc()
# sampleX <- data.all[segID %in% idx[1200:1210]]
# for(i in 1:204){
#   print(i)
#   sampleX <- ops[[i]]
#   print(which(sampleX$segID %in% idx))
# }
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
# for(rcpfile in rcps) { ## ---------------------------------------------
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
  }else{
    roundWood <- HarvLim1 * roundTotWoodRatio
    enWood <- HarvLim1 - roundWood
    HarvLim1 <- cbind(roundWood,enWood)
  }
}else{
  HarvLim1 <- HarvLimMaak*1000*sum(areas)/sum(data.all$area)
  if(harscen == "Low"){ HarvLim1 <- HarvLimMaak * 0.6}
  if(harscen == "MaxSust"){HarvLim1 <- HarvLimMaak * 1.2}
}          

###calculate clearcutting area for the sample
clcutArX <- clcutAr * sum(areas)/sum(data.all$area)
clcutArX <- clcutArX[1:nYears]

# initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
# HarvLim1 <- rep(0,2)
# save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
# region <- regionPrebas(initPrebas)
###run PREBAS
if(harscen!="Base"){
  load(paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
  initPrebas$yassoRun <- rep(1,initPrebas$nSites)
  initPrebas$soilC[,1,,,] <- initSoilC
}

region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1),
                       minDharv = 1.,clearcutAreas =clcutArX)

####roundWood is totHarv
###HarvLim1 defines the harvesting limits (matrix with 2 columns). 
###HarvLim1[,1] is the target for roundWood, HarvLim[,2] is the target for EnergyWood

####compare roundWood
#compare harvest limts
plot(region$totHarv)
points(HarvLim1[,1],col=2,pch=20)
##compare areas clearcutted
plot(region$clearcutAreas[,2])
points(region$clearcutAreas[,1],col=2,pch=20)

####compare energyWood
enWood <- apply(region$multiEnergyWood[,,,1],2,sum)
plot(enWood)
points(HarvLim1[,2],col=2,pch=20)
