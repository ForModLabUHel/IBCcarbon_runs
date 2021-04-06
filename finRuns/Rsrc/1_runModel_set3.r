devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

setX=3
nSitesRun = 20000
nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
sampleIDs <- split(1:nSamples,             # Applying split() function
                   cut(seq_along(1:nSamples),
                   nSetRuns,
                   labels = FALSE))[[setX]]
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
# for(sampleID in sampleIDs){
set.seed(1234, kind = "L'Ecuyer-CMRG" )
mclapply(sampleIDs, function(jx) {
       runModel(jx)  ## Do nothing for 10 seconds
    }, mc.cores = nCores)      ## Split this job across 10 cores
  
