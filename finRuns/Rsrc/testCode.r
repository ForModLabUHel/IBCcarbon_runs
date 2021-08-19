# devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/testCode.r")

r_no <- regions <- 1
sampleID <- 1#498 #136
harvestscenarios <- "Base"
regSets = "maakunta"
minDharvX <- 15
compHarvX=0.
thinFactX=0.25
NotTapio <- TRUE##flag to switch off precommercial thinnings (TRUE) FALSE otherwise
NoftTapio <- FALSE ##flag to switch off first thinning (TRUE) FALSE otherwise

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

if(NoftTapio) ftTapioParX  <- ftTapio * 1e5  ##switch off first thinning
if(NotTapio) tTapioParX  <- tTapio * 1e5  ##switch off precommercial thinning 

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

siteV <- rowSums(initPrebas$multiOut[,1,30,,1])
maakV <- sum(siteV*sampleX$area)/sum(sampleX$area)*sum(data.all$area)
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


HarvLimX <- HarvLim1[1:nYears,]

####save for testing
# save(initPrebas,HarvLimX,minDharvX,clcutArX,
#      file="test.rdata")
print("initialized")
region0 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                        minDharv = minDharvX,clearcutAreas =clcutArX,
                        compHarv=0, thinFact=thinFactX)
print("region0 done")
region1 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                        minDharv = minDharvX,clearcutAreas =clcutArX,
                        compHarv=1, thinFact=thinFactX)
print("region1 done")
region2 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                       minDharv = minDharvX,clearcutAreas =clcutArX,
                       compHarv=3, thinFact=thinFactX)
print("region2 done")

####roundWood is totHarv
###HarvLim1 defines the harvesting limits (matrix with 2 columns). 
###HarvLim1[,1] is the target for roundWood, HarvLim[,2] is the target for EnergyWood
regX <- paste0("region",0:2)
ggMeanAll <- regThinarea <- clcutAreaAll <- regThinVolAll <- regThinareaAll <- 
  regClcutVolAll<- enWoodAll <- regRoundWoodAll <- matrix(NA, nYears,3)
rescalFactor <- sum(data.all$area)/sum(sampleX$area)

for(ix in 1:3){
  region <- get(regX[ix])

####calculate thinned areas
areaThin <- areaClcut <- volThin <- volClcut <- rep(NA,nYears)
harvested <- apply(region$multiOut[,,37,,1],1:2,sum)
vols <- apply(region$multiOut[,,30,,1],1:2,sum)
clcutsOld <- data.table(which(harvested>0 & vols==0,arr.ind=T))
clcuts <- data.table(which(region$multiOut[,,2,1,2]>0,arr.ind=T))
thinOld <- data.table(which(harvested>0 & vols>0,arr.ind=T))
thin <- data.table(which(region$multiOut[,,1,1,2]>0,arr.ind=T))
setnames(clcuts,c("siteID","year"))
setnames(thin,c("siteID","year"))

for(i in 1:nYears) areaThin[i] <- sum(region$areas[thin[year==i]$siteID])
for(i in 1:nYears) areaClcut[i] <- sum(region$areas[clcuts[year==i]$siteID])
for(i in 1:nYears) volThin[i] <- sum(region$areas[thin[year==i]$siteID] * harvested[thin[year==i]$siteID,i])
for(i in 1:nYears) volClcut[i] <- sum(region$areas[clcuts[year==i]$siteID] * harvested[clcuts[year==i]$siteID,i])

regThinarea <- areaThin*rescalFactor
regClcutArea <- areaClcut*rescalFactor
regRoundWood <- region$totHarv*rescalFactor
regThinVolAll[,ix] <- volThin*rescalFactor
regClcutVolAll[,ix] <- volClcut*rescalFactor
regThinareaAll[,ix] <- regThinarea
regRoundWoodAll[,ix] <- regRoundWood
enWoodAll[,ix] <- apply(region$multiEnergyWood[,,,1],2,sum)
clcutAreaAll[,ix] <- region$clearcutAreas[,2]

gg <- apply(region$multiOut[,,43,,1],1:2,sum)
for(i in 1:nYears){
  gg[,i] <- areas * gg[,i]/sum(areas)
}
ggMeanAll[,ix] <- colSums(gg)

}

save(maakV,ggMeanAll,regThinarea,clcutAreaAll,regThinVolAll,regThinareaAll,
     regClcutVolAll,enWoodAll,regRoundWoodAll,file= "...")

###plot #1
####compare roundWood
#compare harvest limts
plot1 <- function(){
  par(mfrow=c(3,2))
  ylim=range(regRoundWoodAll,roundWood*1000)
  plot(regRoundWoodAll[,1],pch=20,col=2,ylim=ylim, main="roundWood")
  points(regRoundWoodAll[,2],pch=20,col=3)
  points(regRoundWoodAll[,3],pch=20,col=4)
  points(roundWood[1:nYears]*1000)
  legend("bottomright",legend = c("noCom","clcut","thin","ref"),
         pch=c(20,20,20,1),col=c(2:4,1))
  ####compare energyWood
  ylim=range(enWoodAll* rescalFactor,HarvLim1[,2] * rescalFactor)
  plot(enWoodAll[,1] * rescalFactor,main="energyWood",col=2,pch=20,ylim=ylim)
  points(enWoodAll[,2] * rescalFactor,col=3,pch=20)
  points(enWoodAll[,3] * rescalFactor,col=4,pch=20)
  points(HarvLim1[,2] * rescalFactor)
  ##compare areas clearcutted
  ylim=range(clcutAreaAll* rescalFactor,
             region$clearcutAreas[,1]*rescalFactor)
  plot(clcutAreaAll[,1] * rescalFactor,main="area clearcuts",col=2,pch=20,ylim=ylim)
  points(clcutAreaAll[,2] * rescalFactor,col=3,pch=20)
  points(clcutAreaAll[,3] * rescalFactor,col=4,pch=20)
  points(region$clearcutAreas[,1]*rescalFactor)
  
  yrange <- range(regThinareaAll,thinAr)
  plot(regThinareaAll[,1],ylim=yrange,main="area thinning",col=2,pch=20)
  points(regThinareaAll[,2],col=3,pch=20)
  points(regThinareaAll[,3],col=4,pch=20)
  points(thinAr)
  points(noClcutAr,col=5)
  
  yrange <- range(ggMeanAll)
  plot(ggMeanAll[,1], main="gross growth",ylim=yrange,col=2,pch=20)
  points(ggMeanAll[,2],col=3,pch=20)
  points(ggMeanAll[,3],col=4,pch=20)
}
###plot #2
plot2 <- function(){
  par(mfrow=c(3,1))
  volumes <- rbind(regThinVolAll[,1],regClcutVolAll[,1])
  barplot(volumes,main="volumes thin/clcut NOcomp",legend=c("thin","clcut"))
  volumes <- rbind(regThinVolAll[,2],regClcutVolAll[,2])
  barplot(volumes,main="volumes thin/clcut ClCut",legend=c("thin","clcut"))
  volumes <- rbind(regThinVolAll[,3],regClcutVolAll[,3])
  barplot(volumes,main="volumes thin/clcut thinning",legend=c("thin","clcut"))
}

plot1()
plot2()