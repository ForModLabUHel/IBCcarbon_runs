# devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/testCode.r")
if(!exists("maaks")) maaks <- 1:19 
if(!exists("sampleID")) sampleID <- 1#498 #136
if(!exists("harvestscenarios")) harvestscenarios <- "Base"
if(!exists("regSets")) regSets = "maakunta"
if(!exists("minDharvX")) minDharvX <- 15
if(!exists("compHarvX")) compHarvX=0.
if(!exists("thinFactX")) thinFactX=0.25
if(!exists("HcFactor")) HcFactor = 1    ##1, 0.8, 1.2, 0.8, 1.2
if(!exists("baFact")) baFact = 1.0    ##1, 1.1, 1.1,  1,   1
if(!exists("dbhFact")) dbhFact = 1.0   ##1,  1 ,   1, 1.1, 1.1
if(!exists("NotTapio")) NotTapio <- FALSE##flag to switch off precommercial thinnings (TRUE) FALSE otherwise
if(!exists("NoftTapio")) NoftTapio <- FALSE ##flag to switch off first thinning (TRUE) FALSE otherwise

ggCountry <- array(NA,dim=c(36,3,19))

for(klk in maaks){
  
# klk <- 3
r_no <- regions <- klk
  
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
# load("test.rdata")
# region2 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
#                         minDharv = minDharvX,cutAreas =clcutArX,
#                         compHarv=3, thinFact=thinFactX)
# plot(apply(region2$multiOut[930,,13,,1],1,sum))

data.all$ba = data.all$ba * baFact
data.all$dbh = data.all$dbh * dbhFact
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
clcutArX <- cbind(clcutArX[1:nYears],0.)
tendX <- tendingAr * sum(areas)/sum(data.all$area)
tendX <- cbind(tendX[1:nYears],0.)
fThinX <- firstThinAr * sum(areas)/sum(data.all$area)
fThinX <- cbind(fThinX[1:nYears],0.)
cutArX <- cbind(clcutArX,tendX)
cutArX <- cbind(cutArX,fThinX)

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
# save(initPrebas,HarvLimX,minDharvX,cutArX,
#      file="test.rdata")
print("initialized")
region0 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                        minDharv = minDharvX,cutAreas =cutArX,
                        compHarv=0, thinFact=thinFactX)
print("region0 done")
region1 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                        minDharv = minDharvX,cutAreas =cutArX,
                        compHarv=1, thinFact=thinFactX)
print("region1 done")
region2 <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                       minDharv = minDharvX,cutAreas =cutArX,
                       compHarv=3, thinFact=thinFactX)
print("region2 done")

####roundWood is totHarv
###HarvLim1 defines the harvesting limits (matrix with 2 columns). 
###HarvLim1[,1] is the target for roundWood, HarvLim[,2] is the target for EnergyWood
regX <- paste0("region",0:2)
ggMeanAll <- regThinarea <- clcutAreaAll <- regThinVolAll <- regThinareaAll <- 
  regClcutVolAll<- enWoodAll <- regRoundWoodAll <- matrix(NA, nYears,3)
rescalFactor <- sum(data.all$area)/sum(sampleX$area)

regThinAreaX <- regThinVolX <- array(NA,dim=c(nYears,4,3))
regClcutAreaX <- regClcutVolX <- array(NA,dim=c(nYears,2,3))

for(ix in 1:3){
  region <- get(regX[ix])

####calculate thinned areas
areaThin <- areaClcut <- volThin <- volClcut <- rep(NA,nYears)
areaThinX <- volThinX <- matrix(NA,nYears,4)
areaClcutX <- volClcutX <- matrix(NA,nYears,2)

harvested <- apply(region$multiOut[,,37,,1],1:2,sum)
vols <- apply(region$multiOut[,,30,,1],1:2,sum)
clcutsOld <- data.table(which(harvested>0 & vols==0,arr.ind=T))
clcuts <- data.table(which(region$multiOut[,,2,1,2]>0,arr.ind=T))
clcuts1 <- data.table(which(region$multiOut[,,2,1,2]==1,arr.ind=T))
clcuts2 <- data.table(which(region$multiOut[,,2,1,2]==20,arr.ind=T))
thinOld <- data.table(which(harvested>0 & vols>0,arr.ind=T))
thin <- data.table(which(region$multiOut[,,1,1,2]>0,arr.ind=T))
thin1 <- data.table(which(region$multiOut[,,1,1,2]==1,arr.ind=T))
thin2 <- data.table(which(region$multiOut[,,1,1,2]==2,arr.ind=T))
thin3 <- data.table(which(region$multiOut[,,1,1,2]==3,arr.ind=T))
thin4 <- data.table(which(region$multiOut[,,1,1,2]==4,arr.ind=T))
setnames(clcuts,c("siteID","year"))
setnames(clcuts1,c("siteID","year"))
setnames(clcuts2,c("siteID","year"))
setnames(thin,c("siteID","year"))
setnames(thin1,c("siteID","year"))
setnames(thin2,c("siteID","year"))
setnames(thin3,c("siteID","year"))
setnames(thin4,c("siteID","year"))

for(i in 1:nYears) areaThin[i] <- sum(region$areas[thin[year==i]$siteID])
for(i in 1:nYears) areaClcut[i] <- sum(region$areas[clcuts[year==i]$siteID])
for(i in 1:nYears) volThin[i] <- sum(region$areas[thin[year==i]$siteID] * harvested[thin[year==i]$siteID,i])
for(i in 1:nYears) volClcut[i] <- sum(region$areas[clcuts[year==i]$siteID] * harvested[clcuts[year==i]$siteID,i])

for(i in 1:nYears){
  areaThinX[i,1] <- sum(region$areas[thin1[year==i]$siteID])
  areaThinX[i,2] <- sum(region$areas[thin2[year==i]$siteID])
  areaThinX[i,3] <- sum(region$areas[thin3[year==i]$siteID])
  areaThinX[i,4] <- sum(region$areas[thin4[year==i]$siteID])
  
  areaClcutX[i,1] <- sum(region$areas[clcuts1[year==i]$siteID])
  areaClcutX[i,2] <- sum(region$areas[clcuts2[year==i]$siteID])
  
  volThinX[i,1] <- sum(region$areas[thin1[year==i]$siteID] * harvested[thin1[year==i]$siteID,i])
  volThinX[i,2] <- sum(region$areas[thin2[year==i]$siteID] * harvested[thin2[year==i]$siteID,i])
  volThinX[i,3] <- sum(region$areas[thin3[year==i]$siteID] * harvested[thin3[year==i]$siteID,i])
  volThinX[i,4] <- sum(region$areas[thin4[year==i]$siteID] * harvested[thin4[year==i]$siteID,i])
  
  volClcutX[i,1] <- sum(region$areas[clcuts1[year==i]$siteID] * harvested[clcuts1[year==i]$siteID,i])
  volClcutX[i,2] <- sum(region$areas[clcuts2[year==i]$siteID] * harvested[clcuts2[year==i]$siteID,i])
} 

regThinarea <- areaThin*rescalFactor
regClcutArea <- areaClcut*rescalFactor
regRoundWood <- region$totHarv*rescalFactor
regThinVolAll[,ix] <- volThin*rescalFactor
regClcutVolAll[,ix] <- volClcut*rescalFactor
regThinareaAll[,ix] <- regThinarea
regRoundWoodAll[,ix] <- regRoundWood
enWoodAll[,ix] <- apply(region$multiEnergyWood[,,,1],2,sum)
clcutAreaAll[,ix] <- region$cutAreas[,2]


regThinAreaX[,,ix] <- areaThinX*rescalFactor
regClcutAreaX[,,ix] <- areaClcutX*rescalFactor
regThinVolX[,,ix] <- volThinX*rescalFactor
regClcutVolX[,,ix] <- volClcutX*rescalFactor
# clcutAreaAll[,ix] <- region$cutAreas[,2]


gg <- apply(region$multiOut[,,43,,1],1:2,sum)
for(i in 1:nYears){
  gg[,i] <- areas * gg[,i]/sum(areas)
}
ggMeanAll[,ix] <- colSums(gg)

}

# save(maakV,ggMeanAll,regThinarea,clcutAreaAll,regThinVolAll,regThinareaAll,
#      regClcutVolAll,enWoodAll,regRoundWoodAll,file= "...")

SDIinit <- reinekeMSinit(initPrebas$multiInitVar)

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
  ylim=range(enWoodAll* rescalFactor,HarvLimX[,2] * rescalFactor)
  plot(enWoodAll[,1] * rescalFactor,main="energyWood",col=2,pch=20,ylim=ylim)
  points(enWoodAll[,2] * rescalFactor,col=3,pch=20)
  points(enWoodAll[,3] * rescalFactor,col=4,pch=20)
  points(HarvLim1[,2] * rescalFactor)
  ##compare areas clearcutted
  ylim=range(clcutAreaAll* rescalFactor,
             region$cutAreas[,1]*rescalFactor)
  plot(clcutAreaAll[,1] * rescalFactor,main="area clearcuts",col=2,pch=20,ylim=ylim)
  points(clcutAreaAll[,2] * rescalFactor,col=3,pch=20)
  points(clcutAreaAll[,3] * rescalFactor,col=4,pch=20)
  points(region$cutAreas[,1]*rescalFactor)
  
  yrange <- range(regThinareaAll,thinAr)
  plot(regThinareaAll[,1],ylim=yrange,main="area thinning",col=2,pch=20)
  points(regThinareaAll[,2],col=3,pch=20)
  points(regThinareaAll[,3],col=4,pch=20)
  points(thinAr)
  points(noClcutAr,col=5)
  
  hist(SDIinit)
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

plot2 <- function(){
  par(mfrow=c(3,1))
  volumes <- rbind(regThinVolAll[,1],regClcutVolAll[,1])
  barplot(volumes,main="volumes thin/clcut NOcomp",legend=c("thin","clcut"))
  volumes <- rbind(regThinVolAll[,2],regClcutVolAll[,2])
  barplot(volumes,main="volumes thin/clcut ClCut",legend=c("thin","clcut"))
  volumes <- rbind(regThinVolAll[,3],regClcutVolAll[,3])
  barplot(volumes,main="volumes thin/clcut thinning",legend=c("thin","clcut"))
}

plot3 <- function(){
  par(mfrow=c(3,1))
  volumes <- t(regThinAreaX[,,1])
  barplot(volumes,main="thin areas NOcomp",legend=c("pre-com","firstT","thin","compThin"))
  volumes <- t(regThinAreaX[,,2])
  barplot(volumes,main="thin areas ClCutComp",legend=c("pre-com","firstT","thin","compThin"))
  volumes <- t(regThinAreaX[,,3])
  barplot(volumes,main="thin areas thinComp",legend=c("pre-com","firstT","thin","compThin"))
}

regStat <- function(modOut,varX, funX){
  v0 <- apply(modOut$multiOut[,,varX,,1],1:2,funX,na.rm=T)
  v1 <- colSums(sweep(v0,1,areas,"*"))/sum(data.sample$area,na.rm=T)
  return(v1)
}
v0 <- regStat(region0,30,"sum")
v1 <- regStat(region1,30,"sum")
v2 <- regStat(region2,30,"sum")
npp0 <- regStat(region0,18,"sum")
npp1 <- regStat(region1,18,"sum")
npp2 <- regStat(region2,18,"sum")
ba0 <- regStat(region0,13,"sum")
ba1 <- regStat(region1,13,"sum")
ba2 <- regStat(region2,13,"sum")
gpp0 <- regStat(region0,44,"sum")
gpp1 <- regStat(region1,44,"sum")
gpp2 <- regStat(region2,44,"sum")

plot4 <- function(){
  par(mfrow=c(3,2))
  ylim=range(0.,region0$cutAreas[,3:4]*rescalFactor,
             region1$cutAreas[,4]*rescalFactor,
             region2$cutAreas[,4]*rescalFactor)
  plot(region0$cutAreas[,3]*rescalFactor,ylim=ylim, 
       main="pre-com areas",ylab="ha")
  points(region0$cutAreas[,4]*rescalFactor,pch=20,col=2)
  points(region1$cutAreas[,4]*rescalFactor,pch=20,col=3)
  points(region2$cutAreas[,4]*rescalFactor,pch=20,col=4)
  legend("bottomright",legend = c("noCom","clcut","thin","ref"),
         pch=c(20,20,20,1),col=c(2:4,1))
  
  ylim=range(0.,region0$cutAreas[,5:6]*rescalFactor,
             region1$cutAreas[,6]*rescalFactor,
             region2$cutAreas[,6]*rescalFactor)
  plot(region0$cutAreas[,5]*rescalFactor,ylim=ylim, 
       main="1st thin areas",ylab="ha")
  points(region0$cutAreas[,6]*rescalFactor,pch=20,col=2)
  points(region1$cutAreas[,6]*rescalFactor,pch=20,col=3)
  points(region2$cutAreas[,6]*rescalFactor,pch=20,col=4)
  
  ylim=range(0.,v0,v1,v2)
  plot(v0,ylim=ylim, main="volume",ylab="m3/ha",pch=20,col=2)
  points(v1,pch=20,col=3)
  points(v2,pch=20,col=4)
  stats[regID==0,points(Vtot/ForLandTot,col=2)]
  stats[regID==r_no,points(Vtot/ForLandTot,col=1)]
  
  ylim=range(0.,gpp0,gpp1,gpp2)
  plot(gpp0,ylim=ylim, main="GPP",ylab="gC/m2",pch=20,col=2)
  points(gpp1,pch=20,col=3)
  points(gpp2,pch=20,col=4)
  
  ylim=range(0.,npp0,npp1,npp2)
  plot(npp0,ylim=ylim, main="npp",ylab="gC/m2",pch=20,col=2)
  points(npp1,pch=20,col=3)
  points(npp2,pch=20,col=4)
  
  ylim=range(0.,ba0,ba1,ba2)
  plot(ba0,ylim=ylim, main="ba",ylab="gC/m2",pch=20,col=2)
  points(ba1,pch=20,col=3)
  points(ba2,pch=20,col=4)
  
}


Ws0 <- regStat(region0,31,"sum")
Ws1 <- regStat(region1,31,"sum")
Ws2 <- regStat(region2,31,"sum")
Wf0 <- regStat(region0,33,"sum")
Wf1 <- regStat(region1,33,"sum")
Wf2 <- regStat(region2,33,"sum")
Wb0 <- regStat(region0,24,"sum")
Wb1 <- regStat(region1,24,"sum")
Wb2 <- regStat(region2,24,"sum")
Wdb0 <- regStat(region0,51,"sum")
Wdb1 <- regStat(region1,51,"sum")
Wdb2 <- regStat(region2,51,"sum")
Wfb0 <- Wf0+Wb0+Wdb0
Wfb1 <- Wf1+Wb1+Wdb1
Wfb2 <- Wf2+Wb2+Wdb2
Wcr0 <- regStat(region0,32,"sum")
Wcr1 <- regStat(region1,32,"sum")
Wcr2 <- regStat(region2,32,"sum")
Wfr0 <- regStat(region0,25,"sum")
Wfr1 <- regStat(region1,25,"sum")
Wfr2 <- regStat(region2,25,"sum")
Wtot0 <- Wfr0 + Wcr0 + Wfb0 + Ws0+Wdb0
Wtot1 <- Wfr1 + Wcr1 + Wfb1 + Ws1+Wdb1
Wtot2 <- Wfr2 + Wcr2 + Wfb2 + Ws2+Wdb2

plot5 <- function(){
  par(mfrow=c(3,2))

  mreg <- as.numeric(stats[regID==0,Vtot/ForLandTot])
  mcount <- as.numeric(stats[regID==r_no,Vtot/ForLandTot])
  ylim=range(0.,v0,v1,v2,mreg,mcount,na.rm=T)
  plot(v0,ylim=ylim, main="volume",ylab="m3/ha",pch=20,col=2)
  points(v1,pch=20,col=3)
  points(v2,pch=20,col=4)
  points(mreg,col=2)
  points(mcount,col=1)
  
  mreg <- as.numeric(stats[regID==0,IncrTot])
  mcount <- as.numeric(stats[regID==r_no,IncrTot])
  yrange=range(ggMeanAll,mreg,mcount,na.rm=T)
  plot(ggMeanAll[,1], main="gross growth",ylim=yrange,col=2,pch=20)
  points(ggMeanAll[,2],col=3,pch=20)
  points(ggMeanAll[,3],col=4,pch=20)
  stats[regID==0,points(IncrTot,col=2)]
  stats[regID==r_no,points(IncrTot,col=1)]
  
  mreg <- as.numeric(stats[regID==0,WfbTot/ForLandTot])
  mcount <- as.numeric(stats[regID==r_no,WfbTot/ForLandTot])
  ylim=range(Wfb0,Wfb1,Wfb2,mreg,mcount,na.rm=T)
  plot(Wfb0,ylim=ylim, main="Foliage + Branches",ylab="kgC/ha",pch=20,col=2)
  points(Wfb1,pch=20,col=3)
  points(Wfb2,pch=20,col=4)
  stats[regID==0,points(WfbTot/ForLandTot,col=2)]
  stats[regID==r_no,points(WfbTot/ForLandTot,col=1)]  

  mreg <- as.numeric(stats[regID==0,WsTot/ForLandTot])
  mcount <- as.numeric(stats[regID==r_no,WsTot/ForLandTot])
  ylim=range(Ws0,Ws1,Ws2,mreg,mcount,na.rm=T)
  plot(Ws0,ylim=ylim, main="Ws",ylab="kgC/ha",pch=20,col=2)
  points(Ws1,pch=20,col=3)
  points(Ws2,pch=20,col=4)
  stats[regID==0,points(WsTot/ForLandTot,col=2)]
  stats[regID==r_no,points(WsTot/ForLandTot,col=1)]  
  
  mreg <- as.numeric(stats[regID==0,WcrTot/ForLandTot])
  mcount <- as.numeric(stats[regID==r_no,WcrTot/ForLandTot])
  ylim=range(Wcr0,Wcr1,Wcr2,mreg,mcount,na.rm=T)
  plot(Wcr0,ylim=ylim, main="Wcr",ylab="kgC/ha",pch=20,col=2)
  points(Wcr1,pch=20,col=3)
  points(Wcr2,pch=20,col=4)
  stats[regID==0,points(WcrTot/ForLandTot,col=2)]
  stats[regID==r_no,points(WcrTot/ForLandTot,col=1)]  
  
  mreg <- as.numeric(stats[regID==0,WtotTot/ForLandTot])
  mcount <- as.numeric(stats[regID==r_no,WtotTot/ForLandTot])
  ylim=range(Wtot0,Wtot1,Wtot2,mreg,mcount,na.rm=T)
  plot(Wtot0,ylim=ylim, main="Wtot",ylab="kgC/ha",pch=20,col=2)
  points(Wtot1,pch=20,col=3)
  points(Wtot2,pch=20,col=4)
  stats[regID==0,points(WtotTot/ForLandTot,col=2)]
  stats[regID==r_no,points(WtotTot/ForLandTot,col=1)]  

}

save(Ws0, Ws1, Ws2, Wf0, Wf1, Wf2, Wb0, Wb1, Wb2,SDIinit,
     Wdb0, Wdb1, Wdb2, Wfb0, Wfb1, Wfb2, Wcr0, Wcr1, Wcr2,
     Wfr0, Wfr1, Wfr2, Wtot0, Wtot1, Wtot2,v0,v1,v2,ggMeanAll,
    file=paste0("testPlotsData/reg_",klk,"_HcF",HcFactor,"_baF",baFact,"_dbhF",dbhFact,".rdata"))
pdf(paste0("testPlotsData/plots_",klk,"_HcF",HcFactor,"_baF",baFact,"_dbhF",dbhFact,".pdf"))
plot1()
plot2()
plot3()
plot4()
plot5()
dev.off()
print(klk)

ggCountry[,,klk] <- ggMeanAll
ls()
# rm(list=ls())
rm(list=ls()[-which(ls()=="ggCountry")])
gc()


}
save(ggCountry,file=paste0("testPlotsData/ggCountry","_HcF",HcFactor,"_baF",baFact,"_dbhF",dbhFact,".rdata"))
# maakNam <- read.table("/scratch/project_2000994/PREBASruns/metadata/maakunta/maakunta_numbers.txt")