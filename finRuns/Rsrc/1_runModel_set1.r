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
# test
toMem <- ls()
###check and run missing sampleIDs 
# library('stringi')
# fileX <- list.files(path= "/scratch/project_2000994/PREBASruns/finRuns/outputDT/forCent12/", pattern = "age")
# sampleIDs <- which(!1:nSamples %in%  as.numeric(stri_extract_last(fileX, regex = "(\\d+)")))
# print(sampleIDs)
# sampleIDs <- c(66,342,395)
mclapply(sampleIDs, function(jx) {
  runModel(jx,harvScen=harvScen,harvInten=harvInten)
}, mc.cores = nCores,mc.silent=FALSE)      

# models outputs to NAs, outputDT, initSoilC and plots
Sys.chmod(list.dirs("NAs"), "0777",use_umask=FALSE)
f <- list.files("NAs", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("outputDT"), "0777",use_umask=FALSE)
f <- list.files("outputDT", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("plots"), "0777",use_umask=FALSE)
f <- list.files("plots", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)