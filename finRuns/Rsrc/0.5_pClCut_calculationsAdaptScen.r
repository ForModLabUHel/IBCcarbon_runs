library(Rprebasso)
library(minpack.lm)

factRotLength = -7 ###numbers of years to reduce rotation length
factFert = -20 + factRotLength
sampleID = 3 ###12 45    ####for region 13 take 3 and 6; region 12 sampleID c(23,440); region 11 sampleID c(23,150)
pathX <- "input/maakunta/pClCut_adapt/"

if(!exists("regions")) regions <- 1:19
# toMem <- ls()

for(r_no in regions){
  
  # r_no = regions = 4 ### forest center ID (metakeskus) 1:15
  regSets <- "maakunta"
  nSetRuns = 10 #number of set runs
  harvScen = "Base"		##management scenarios it can be  ### c("Low","MaxSust","NoHarv","Base")
  harvInten = "Base"
  rcpfile= "CurrClim"
  # compHarvX= 3
  # minDharvX = 15
  NoftTapio= FALSE
  NotTapio= FALSE
  
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/sampleRun.r")
  out <- sampleX$region
  
  # setwd("C:/Users/checcomi/Documents/research/IBC-carbon/testRun/")
  # load("testOut.rdata")
  
  spSite <- c("sitesP3","sitesP4","sitesP3",
              "sitesSP2","sitesSP3",
              "sitesB2","sitesB3")
  spXs <- c(1,1,1,2,2,3,3)
  tabX <- c("ClCut_pine","ClCut_pine","ClCut_pine",
            "ClCut_spruce","ClCut_spruce",
            "ClCut_birch","ClCut_birch")
  indX <- c(1,2,3,1,2,1,2)
  ####reduce rotation of 25%
  pClCut <- calNewDclcut(out,ClCut_pine,
                         ClCut_spruce,
                         ClCut_birch,
                         fact=factRotLength)
  
  pFert <- calNewDclcut(out,pClCut$ClCut_pine,
                          pClCut$ClCut_spruce,
                          pClCut$ClCut_birch,
                         fact=factFert)
  
  save(pClCut,pFert,
       file=paste0(pathX,"ClCutplots_maak",r_no,".rdata"))
  print(paste("region",r_no,"completed")) 
  # rm(list=setdiff(ls(), toMem));gc()
}


Sys.chmod(list.dirs(pathX), "0777",use_umask=FALSE)
f <- list.files(pathX, all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

