sampleID=16
r_no = regions = 6 ### forest center ID
sampleForPlotsX <- TRUE 
  
nSetRuns = 10 #number of set runs

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
if(sampleForPlotsX) sampleForPlots <- sampleID
setX=1
nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
sampleIDs <- split(1:nSamples,             # Applying split() function
                   cut(seq_along(1:nSamples),
                       nSetRuns,
                       labels = FALSE))[[setX]]
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
# for(sampleID in sampleIDs){
runModel(sampleID)