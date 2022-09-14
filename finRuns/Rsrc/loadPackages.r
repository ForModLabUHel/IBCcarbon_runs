CSCrun=T

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

###choose PREBAS version
vPREBAS <- "master"   #### choose PREBAS version to run the model  "master" "v0.2.x"
install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(Rprebasso)
library(DescTools)
