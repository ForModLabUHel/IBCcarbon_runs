# if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
# }

###choose PREBAS version
vPREBAS <- "v0.2.x"   #### choose PREBAS verson to run the model  "master"
devtools::install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(raster)
library(rgdal)
library(data.table)
library(devtools)
library(plyr)
require(sm)
library(reshape2)
library(Rprebasso)


# name raster file with segs
rastSegFN <- "/scratch/project_2000994/MVMIsegments/segment-IDs/ls_seg2.img"

####read shapefiles for masking
maskX <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/", layer = "Koke_Paavesistoalue_VALUE")
forCent <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/",layer = "mkeskus13tm35")


####paths
pathtoken = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/"
climatepath = "/scratch/project_2000994/RCP/"

setwd(pathtoken)

data.all <- fread("input/kokeInputs")
setnames(data.all,"consArea","cons")
cloudpixels = data.all[, sum(ba==32766)]
nonforest = data.all[, sum(ba==32767)]
forest = data.all[, sum(ba< 32766)]
AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
AREA_1000ha = AREA / 10000 / 1000
data.all[,area:=N*16^2/10000]
areaTot <- sum(data.all$N)
## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
data.all = data.all[ba < 32766]
data.all = data.all[!is.na(cons)]
