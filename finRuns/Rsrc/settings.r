CSCrun=T

library(raster)
library(rgdal)
library(data.table)
library(devtools)
library(plyr)
# library(reshape2)
library(parallel)
library(ggplot2)
library(abind)
require(sm)

###load packages in CSC project folder
if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
}
###choose PREBAS version
vPREBAS <- "master"   #### choose PREBAS version to run the model  "master" "v0.2.x"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(Rprebasso)
library(DescTools)

# r_no = regions = 2  ### forest center ID
nCores <- 6  ####  number of cores
sampleForPlots <- 9999999
# Missing from varOuts: 
# MinPeat-index, NEPdrPeat

###flag for settings the regions to consider
if(!exists("regSets")) regSets <- "forCent" ### "forCent", "maakunta"
if(!exists("minDharvX")) minDharvX <- 15 ### minimum DBH for clearcutting
if(!exists("compHarvX")) compHarvX=3.
if(!exists("thinFactX")) thinFactX=0.2
if(!exists("clcutArX") & regSets=="forCent") clcutArX <- NA
if(!exists("clcutArX") & regSets=="maakunta") clcutArX <- 1

varOuts <- c("NEP sp","GPPspecies", "npp", "gross growth", 
             "soilC", "V", "age", "Wharvested", 
             "Litter_fol", "Litter_fr", 
             "Litter_fWoody", "Litter_cWoody",
             "DeadWoodVolume", "D", "BA", "H")
varSel <- match(varOuts,varNames)
specialVars <- c("domSpecies","domAge","Vdec","WenergyWood","Wtot")

#varSel <- c(7,8,9,11:13,17:18,22,24:33,37:39,41:46)   #### variables IDs to be stored

###set if you want to use Layers sum of BA average of stored variables
funX <- rep("sum",length(varSel))
funX[match(varNames[c(7,11:12)],varNames[varSel])] <- "baWmean"
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
nYears = 2051-startingYear
if(!exists("harvestscenarios")) harvestscenarios = "Base"#c("Low","MaxSust","NoHarv","Base")
rcps = "CurrClim" #c("CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")#c("CurrClim","CanESM2.rcp26.rdata")#,"CanESM2.rcp45.rdata","CanESM2.rcp85.rdata")

nSitesRun = 20000  ###aproximative number of samples for set runs
# nSetRuns = 10 #number of set runs

####period for model output calculations
per1=2017:2025
per2=2026:2033
per3=2034:2050
simYear1 = per1 - startingYear
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
  roundWood <- as.numeric(c(unlist(roundWoodTab[id==r_no,3:8]),
                 rep(unlist(roundWoodTab[id==r_no,9]),4),
                 rep(unlist(roundWoodTab[id==r_no,10]),10),
                 rep(unlist(roundWoodTab[id==r_no,11]),10),
                 rep(unlist(roundWoodTab[id==r_no,12]),10),
                 rep(unlist(roundWoodTab[id==r_no,13]),10)
  ))
  energyWoodTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="energyWood"))
  energyWood <- as.numeric(c(unlist(energyWoodTab[id==r_no,3:8]),
                  rep(unlist(energyWoodTab[id==r_no,9]),4),
                  rep(unlist(energyWoodTab[id==r_no,10]),10),
                  rep(unlist(energyWoodTab[id==r_no,11]),10),
                  rep(unlist(energyWoodTab[id==r_no,12]),10),
                  rep(unlist(energyWoodTab[id==r_no,13]),10)
  ))
  clcutArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="clearcutAreas"))
  clcutAr <- as.numeric(c(unlist(clcutArTab[id==r_no,3:8]),
               rep(unlist(clcutArTab[id==r_no,9]),4),
               rep(unlist(clcutArTab[id==r_no,10]),10),
               rep(unlist(clcutArTab[id==r_no,11]),10),
               rep(unlist(clcutArTab[id==r_no,12]),10),
               rep(unlist(clcutArTab[id==r_no,13]),10)
  ))
  HarvLimMaak <- cbind(roundWood,energyWood)

  thinArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="thinningAreas"))
  thinAr <- as.numeric(c(unlist(thinArTab[id==r_no,3:8]),
               rep(unlist(thinArTab[id==r_no,9]),4),
               rep(unlist(thinArTab[id==r_no,10]),10),
               rep(unlist(thinArTab[id==r_no,11]),10),
               rep(unlist(thinArTab[id==r_no,12]),10),
               rep(unlist(thinArTab[id==r_no,13]),10)
  ))
  noClcutArTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="NoClearCutArea"))
  noClcutAr <- as.numeric(c(unlist(noClcutArTab[id==r_no,3:8]),
              rep(unlist(noClcutArTab[id==r_no,9]),4),
              rep(unlist(noClcutArTab[id==r_no,10]),10),
              rep(unlist(noClcutArTab[id==r_no,11]),10),
              rep(unlist(noClcutArTab[id==r_no,12]),10),
              rep(unlist(noClcutArTab[id==r_no,13]),10)
  ))

  firstThinAreaTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="firstThinArea"))
  firstThinAr <- as.numeric(c(unlist(firstThinAreaTab[id==r_no,3:8]),
                 rep(unlist(firstThinAreaTab[id==r_no,9]),4),
                 rep(unlist(firstThinAreaTab[id==r_no,10]),10),
                 rep(unlist(firstThinAreaTab[id==r_no,11]),10),
                 rep(unlist(firstThinAreaTab[id==r_no,12]),10),
                 rep(unlist(firstThinAreaTab[id==r_no,13]),10)
  ))

  tendingAreaTab <- data.table(read_excel(
    path = "/scratch/project_2000994/PREBASruns/metadata/maakunta/harvest.xlsx",
    sheet="tendingArea"))
  tendingAr <- as.numeric(c(unlist(tendingAreaTab[id==r_no,3:8]),
                              rep(unlist(tendingAreaTab[id==r_no,9]),4),
                              rep(unlist(tendingAreaTab[id==r_no,10]),10),
                              rep(unlist(tendingAreaTab[id==r_no,11]),10),
                              rep(unlist(tendingAreaTab[id==r_no,12]),10),
                              rep(unlist(tendingAreaTab[id==r_no,13]),10)
  ))
  
}
