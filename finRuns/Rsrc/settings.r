# CSCrun=T

library(raster)
library(rgdal)
library(parallel)
library(ggplot2)
library(readxl)

###load packages in CSC project folder
# if(CSCrun){
if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
}
require(devtools)
require(data.table)
require(plyr)
require(dplyr)
require(abind)
require(sm)

# ###choose PREBAS version
# vPREBAS <- "master"   #### choose PREBAS version to run the model  "master" "v0.2.x"
# install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

require(Rprebasso)

# library(Rprebasso)
library(DescTools)

# r_no = regions = 2  ### forest center ID
nCores <- 6  ####  number of cores
sampleForPlots <- 1
###management and intesity of harvest
if(!exists("harvScen")) harvScen = "Base"#c("Base", Low","MaxSust")
if(!exists("harvInten")) harvInten = "Base"#c("NoHarv","Base")

# Missing from varOuts: 
# MinPeat-index, NEPdrPeat
if(!exists("mortMod")) mortMod=3
if(!exists("BioIndCalc")) BioIndCalc=FALSE ##if true include the bioindeces in the data.tables outputDT
if(!exists("siteTypes")) siteTypes=1:20
if(!exists("landClassX")) landClassX=1:3
###flag for settings the regions to consider
if(!exists("regSets")) regSets <- "maakunta" ### "forCent", "maakunta"
if(!exists("minDharvX")) minDharvX <- 15 ### minimum DBH for clearcutting
if(!exists("compHarvX")) compHarvX=0.
if(!exists("ageHarvPriorX")) ageHarvPriorX=120.
if(!exists("thinIntX")) thinIntX=0.9
if(!exists("thinFactX")) thinFactX=0.2
if(!exists("clcutArX") & regSets=="forCent") clcutArX <- NA
if(!exists("clcutArX") & regSets=="maakunta") clcutArX <- 1
if(!exists("clcutArFact")) clcutArFact <- 1 ###multiplication factor to increase or decrease the clearcutting area
if(!exists("HSIruns")) HSIruns=FALSE #flag for HSI Ismael runs if T average deadwood initialization over the three regions that Ismael is using in the analysis
####Hcmodel bias for all maakunta regions
HcFactorAll <- rep(1,19)#c(1.2,0.8,0.8,1.2,1.2,0.8,1.2,1.2,0.8,
# 0.8,1.2,0.8,0.8,1.2,0.8,1.2,0.8,1.2,1.2)
if(!exists("HcFactor") & regSets=="maakunta") HcFactor = HcFactorAll[r_no]
if(!exists("HcFactor") & regSets=="forCent") HcFactor = 1.
if(!exists("HcModVx")) HcModVx = 1

###parameters for adaptation scenario
if(!exists("fertThin")) fertThin=3 ###flag that indicates to which TapioType of thinning apply the fertilization:1->precommercial; 2->firstThin; 3->normal thinning
if(!exists("nYearsFert")) nYearsFert=20 ###number of years for which the fertilization has effect

###reduce krein parameter in order to increase mortality
if(!exists("pCrobasX")) pCrobasX <- pCROB
pCrobasX[17,1:3] <- pCROB[17,1:3]

varOuts <- c("NEP","GPPtrees", "npp", "grossGrowth", 
             "soilC", "V", "age", "WroundWood","VroundWood",
             "Litter_fol", "Litter_fr", 
             "Litter_fWoody", "Litter_cWoody",
             "DeadWoodVolume", "D", "BA", "H", "Vmort","Wdb",
             "Hc_base","wf_STKG","Rh")
varSel <- match(varOuts,varNames)
specialVars <- c("domSpecies","domAge","Vdec","Vpine","Vspruce","VenergyWood",
                 "WenergyWood","Wtot","GVgpp","GVw")

#varSel <- c(7,8,9,11:13,17:18,22,24:33,37:39,41:46)   #### variables IDs to be stored

###set if you want to use Layers sum of BA average of stored variables
funX <- rep("sum",length(varSel))
funX[match(varNames[c(7,11:12,14)],varNames[varSel])] <- "baWmean"
# name raster file with segs
# rastSegFN <- "/scratch/project_2000994/MVMIsegments/segment-IDs/ls_seg2.img"

####read shapefiles for masking
# maskX <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/", layer = "Koke_Paavesistoalue_VALUE")
# forCent <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/",layer = "mkeskus13tm35")


####paths
pathtoken = "/scratch/project_2000994/PREBASruns/finRuns/"
climatepath = "/scratch/project_2000994/RCP/"

crsX <- ("+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m
  +no_defs")

setwd(pathtoken)

####ratio used to calculate round Wood percentage of total harvest, the remaining is Energywood
roundTotWoodRatio <- 0.87

harvestLims <- c(9775000,1466000)
year1harv=0 ###if 1 set harvLim for Low and MaxSust as 0.6 and 1.2 of HarvLim (Base)
domSPrun = 0   ### 1 -> run only dominant layer
startingYear = 2015
endingYear = 2051
if(!exists("nYears")) nYears = endingYear-startingYear

if(!exists("rcps")) rcps = "CurrClim" #c("CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")#c("CurrClim","CanESM2.rcp26.rdata")#,"CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")

if(!exists("nSitesRun")) nSitesRun = 20000  ###aproximative number of samples for set runs
# nSetRuns = 10 #number of set runs

####period for model output calculations
per1=2017:2025
per2=2026:2033
per3=2034:2050
simYear1 = per1 - startingYear
simYear1 <- simYear1[simYear1>0]
simYear2 = per2 - startingYear
simYear3 = per3 - startingYear
colsOut1 = c(paste("V", simYear1, sep=""))
colsOut2 = c(paste("V", simYear2, sep=""))
colsOut3 = c(paste("V", simYear3, sep=""))


if(regSets=="forCent"){
  load(paste0("input/forCent/data.all_forCent_",r_no,".rdata"))
}else{
  load(paste0("input/maakunta/data.all_maakunta_",r_no,".rdata"))
  data.all$segID <- data.all$maakuntaID
}
####procData
data.all <- data.all[fert %in% siteTypes]
data.all <- data.all[landclass %in% landClassX]
cloudpixels = data.all[, sum(ba==32766)]
nonforest = data.all[, sum(ba==32767)]
forest = data.all[, sum(ba< 32766)]
AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
AREA_1000ha = AREA / 10000 / 1000
data.all[,area:=nPix*16^2/10000]
pixTot <- sum(data.all$nPix)
setnames(data.all,"nPix","N")
## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
data.all = data.all[ba < 32766]
data.all = data.all[!is.na(cons)]


####load data
# load("outSoil/InitSoilCstst_Base.rdata")
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



if(regSets=="maakunta"){
  roundWoodTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="roundWood"))
  roundWood <- as.numeric(c(unlist(roundWoodTab[id==r_no,3:9]),
                            rep(unlist(roundWoodTab[id==r_no,10]),3),
                            rep(unlist(roundWoodTab[id==r_no,11]),10),
                            rep(unlist(roundWoodTab[id==r_no,12]),10),
                            rep(unlist(roundWoodTab[id==r_no,13]),10),
                            rep(unlist(roundWoodTab[id==r_no,14]),10)
  ))
  energyWoodFromRoundWoodTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="energyWoodRoundWood"))
  energyWoodFromRoundWood <- as.numeric(c(unlist(energyWoodFromRoundWoodTab[id==r_no,3:9]),
                                          rep(unlist(energyWoodFromRoundWoodTab[id==r_no,10]),3),
                                          rep(unlist(energyWoodFromRoundWoodTab[id==r_no,11]),10),
                                          rep(unlist(energyWoodFromRoundWoodTab[id==r_no,12]),10),
                                          rep(unlist(energyWoodFromRoundWoodTab[id==r_no,13]),10),
                                          rep(unlist(energyWoodFromRoundWoodTab[id==r_no,14]),10)
  ))
  energyWoodTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="energyWood"))
  energyWood <- as.numeric(c(unlist(energyWoodTab[id==r_no,3:9]),
                             rep(unlist(energyWoodTab[id==r_no,10]),3),
                             rep(unlist(energyWoodTab[id==r_no,11]),10),
                             rep(unlist(energyWoodTab[id==r_no,12]),10),
                             rep(unlist(energyWoodTab[id==r_no,13]),10),
                             rep(unlist(energyWoodTab[id==r_no,14]),10)
  ))
  
  ####!!!!!!!!!!!to be checked!!!!!!!
  ####energy wood from statistics includes pulp, sawn and energyWood from roundWood
  roundWood <- energyWoodFromRoundWood + roundWood
  ######!!!!#####
  
  clcutArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="clearcutAreas"))
  clcutAr <- as.numeric(c(unlist(clcutArTab[id==r_no,3:9]),
                          rep(unlist(clcutArTab[id==r_no,10]),3),
                          rep(unlist(clcutArTab[id==r_no,11]),10),
                          rep(unlist(clcutArTab[id==r_no,12]),10),
                          rep(unlist(clcutArTab[id==r_no,13]),10),
                          rep(unlist(clcutArTab[id==r_no,14]),10)
  ))
  clcutAr <- clcutAr * clcutArFact
  HarvLimMaak <- cbind(roundWood,energyWood)
  
  thinArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="thinningAreas"))
  thinAr <- as.numeric(c(unlist(thinArTab[id==r_no,3:9]),
                         rep(unlist(thinArTab[id==r_no,10]),3),
                         rep(unlist(thinArTab[id==r_no,11]),10),
                         rep(unlist(thinArTab[id==r_no,12]),10),
                         rep(unlist(thinArTab[id==r_no,13]),10),
                         rep(unlist(thinArTab[id==r_no,14]),10)
  ))
  noClcutArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="NoClearCutArea"))
  noClcutAr <- as.numeric(c(unlist(noClcutArTab[id==r_no,3:9]),
                            rep(unlist(noClcutArTab[id==r_no,10]),3),
                            rep(unlist(noClcutArTab[id==r_no,11]),10),
                            rep(unlist(noClcutArTab[id==r_no,12]),10),
                            rep(unlist(noClcutArTab[id==r_no,13]),10),
                            rep(unlist(noClcutArTab[id==r_no,14]),10)
  ))
  
  firstThinAreaTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="firstThinArea"))
  firstThinAr <- as.numeric(c(unlist(firstThinAreaTab[id==r_no,3:9]),
                              rep(unlist(firstThinAreaTab[id==r_no,10]),3),
                              rep(unlist(firstThinAreaTab[id==r_no,11]),10),
                              rep(unlist(firstThinAreaTab[id==r_no,12]),10),
                              rep(unlist(firstThinAreaTab[id==r_no,13]),10),
                              rep(unlist(firstThinAreaTab[id==r_no,14]),10)
  ))
  
  tendingAreaTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="tendingArea"))
  tendingAr <- as.numeric(c(unlist(tendingAreaTab[id==r_no,3:9]),
                            rep(unlist(tendingAreaTab[id==r_no,10]),3),
                            rep(unlist(tendingAreaTab[id==r_no,11]),10),
                            rep(unlist(tendingAreaTab[id==r_no,12]),10),
                            rep(unlist(tendingAreaTab[id==r_no,13]),10),
                            rep(unlist(tendingAreaTab[id==r_no,14]),10)
  ))
  stats <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="stats",col_types=c("text","numeric","numeric","text",rep("numeric",40)),na="NA"))
  ####converts data to model output units
  cFact <- 1e6 ####M m3 -> m3
  stats[,names(stats)[5:9]:=.SD*cFact,.SDcols=5:9]  ####converts volume from Mm3 to m3
  cFact <- 1e9/2 #### orginal units M t dryMatter -> kgC
  stats[,names(stats)[11:30]:= .SD*cFact,.SDcols=11:30] ####converts biomasses
  cFact <- 1e3 #### orginal units kha -> ha
  stats[,names(stats)[38:44]:= .SD*cFact,.SDcols=38:44] ####converts areas
  
}

regIDs <- stats[4:22,3:4]
setkey(regIDs,regID)

