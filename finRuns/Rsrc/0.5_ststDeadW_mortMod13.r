# devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/testCode.r")
mortMod=13
if(!exists("maaks")) maaks <- 1:19 
if(!exists("sampleID")) sampleID <- 1#498 #136
if(!exists("harvScen")) harvScen <- "Base"
if(!exists("harvInten")) harvInten <- "Base"
if(!exists("regSets")) regSets = "maakunta"
if(!exists("minDharvX")) minDharvX <- 100
if(!exists("compHarvX")) compHarvX=2.
if(!exists("thinFactX")) thinFactX=0.25
#if(!exists("HcFactor")) HcFactor = 1    ##1, 0.8, 1.2, 0.8, 1.2
if(!exists("NotTapio")) NotTapio <- FALSE##flag to switch off precommercial thinnings (TRUE) FALSE otherwise
if(!exists("NoftTapio")) NoftTapio <- FALSE ##flag to switch off first thinning (TRUE) FALSE otherwise
if(!exists("ggCountry")) ggCountry <- array(NA,dim=c(36,3,19))

noRmList <- c(ls(),"noRmList")
nYears <- 36#50
sampleID=3
yearsDeadW <- 1:36#21:50

for(klk in maaks){
  
# klk <- 4
  r_no <- regions <- klk
  
  source("/scratch/project_2000994/PREBASruns/finRuns/Rsrc/fra/maakuntaRuns/codes/outSampleSettings.r")
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
    
  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
  set.seed(1)
  ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  toMem <- ls()
  
  runModel(sampleID,outType="ststDeadW",
           harvScen="Base",harvInten="Base",
           compHarvX=compHarvX)
  
  print(paste("region",r_no,"completed"))
  rm(list=setdiff(ls(), c(toMem,"toMem")))
  gc()
  
}

Sys.chmod(list.dirs("initDeadWVss"), "0777",use_umask=FALSE)
f <- list.files("initDeadWVss", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

