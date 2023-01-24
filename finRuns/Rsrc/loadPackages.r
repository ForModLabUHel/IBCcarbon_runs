CSCrun=T

library(raster)
library(rgdal)
library(parallel)
library(ggplot2)
library(readxl)
library(ggpubr)
library(httr)
require(devtools)
require(data.table)
require(plyr)
require(dplyr)
require(abind)
require(sm)
library(Matrix)

###load packages in CSC project folder
# if(CSCrun){
if(CSCrun){
  .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
  libpath <- .libPaths()[1]
}
###choose PREBAS version
vPREBAS <- "v1.0.0"   #### choose PREBAS version to run the model  "master" "v1.0.0"
install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)

library(Rprebasso)
library(DescTools)
