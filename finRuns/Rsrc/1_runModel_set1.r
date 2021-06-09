devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

setX=1
nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
sampleIDs <- split(1:nSamples,             # Applying split() function
                   cut(seq_along(1:nSamples),
                   nSetRuns,
                   labels = FALSE))[[setX]]
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))

###check and run missing sampleIDs 
# library('stringi')
# fileX <- list.files(path= "/scratch/project_2000994/PREBASruns/finRuns/outputDT/forCent12/", pattern = "age")
# sampleIDs <- which(!1:nSamples %in%  as.numeric(stri_extract_last(fileX, regex = "(\\d+)")))
# print(sampleIDs)
# sampleIDs <- c(136,498)
mclapply(sampleIDs, function(jx) {
  runModel(jx)  ## Do nothing for 10 seconds
}, mc.cores = nCores,mc.silent=FALSE)      ## Split this job across 10 cores
