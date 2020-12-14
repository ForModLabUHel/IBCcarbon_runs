print("start")
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")
sampleIDs <- 25:36
set.seed(1)
ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
# for(sampleID in sampleIDs){
  mclapply(sampleIDs, function(jx) {
       runModel(jx)  ## Do nothing for 10 seconds
    }, mc.cores = nCores)      ## Split this job across 10 cores
  
#   mclapply(sampleIDs)
#   print(sampleID)
# }

# load("output/CurrClim.rdataBase_sample1.rdata")
# ciao <- apply(out$annual[,,37,],1:2,sum)
# plot(colSums(ciao))
# ops <- ciao*sampleX$area
# plot(colSums(ops))
