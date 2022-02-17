# devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/testCode.r")
if(!exists("maaks")) maaks <- 1:19 
if(!exists("sampleID")) sampleID <- 1#498 #136
if(!exists("harvestscenarios")) harvestscenarios <- "Base"
if(!exists("regSets")) regSets = "maakunta"
if(!exists("minDharvX")) minDharvX <- 15
if(!exists("compHarvX")) compHarvX=0.
if(!exists("thinFactX")) thinFactX=0.25
#if(!exists("HcFactor")) HcFactor = 1    ##1, 0.8, 1.2, 0.8, 1.2
if(!exists("NotTapio")) NotTapio <- FALSE##flag to switch off precommercial thinnings (TRUE) FALSE otherwise
if(!exists("NoftTapio")) NoftTapio <- FALSE ##flag to switch off first thinning (TRUE) FALSE otherwise
if(!exists("ggCountry")) ggCountry <- array(NA,dim=c(36,3,19))

noRmList <- c(ls(),"noRmList")
nYears <- 50
sampleID=5
yearsDeadW <- 21:50

for(klk in maaks){
  
# klk <- 4
  r_no <- regions <- klk
    
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
  set.seed(1)
  ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  toMem <- ls()
  
  runModel(sampleID,outType="ststDeadW")
  print(paste("region",r_no,"completed"))
}

Sys.chmod(list.dirs("initDeadWVss"), "0744",use_umask=FALSE)
f <- list.files("initDeadWVss", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0744"),use_umask=FALSE)

