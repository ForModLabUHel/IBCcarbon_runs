if(!exists("r_no")) r_no <- 4
if(!exists("sampleID")) sampleID=3
if(!exists("harvScen")) harvScen <- "Base"
if(!exists("harvScen")) harvInten <- "Base"
# if(!exists("mortMod")) mortMod <- 3
# HcFactor=1.
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

# if(NoftTapio) ftTapioParX  <- ftTapio * 1e5  ##switch off first thinning
# if(NotTapio) tTapioParX  <- tTapio * 1e5  ##switch off precommercial thinning 

# setX=1
# nSitesRun=1000
nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
# sampleIDs <- split(1:nSamples,             # Applying split() function
#                    cut(seq_along(1:nSamples),
#                    nSetRuns,
#                    labels = FALSE))[[setX]]
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))

toMem <- ls()

sampleX <- runModel(sampleID,outType="testRun",
                  harvScen=harvScen,harvInten=harvInten)
# sampleX <- runModel(sampleID,outType="uncSeg")
