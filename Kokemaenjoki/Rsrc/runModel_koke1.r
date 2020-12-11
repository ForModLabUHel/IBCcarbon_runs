devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")
sampleIDs <- 1:12
for(sampleID in sampleIDs){
  set.seed(1)
  ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))
  sampleX <- ops[[sampleID]]
  sampleX[,area := N*16^2/10000]
  sampleX[,id:=climID]
  
  source("runModel.r")
  print(sampleID)
}

# load("output/CurrClim.rdataBase_sample1.rdata")
# ciao <- apply(out$annual[,,37,],1:2,sum)
# plot(colSums(ciao))
# ops <- ciao*sampleX$area
# plot(colSums(ops))
