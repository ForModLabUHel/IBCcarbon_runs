###load local settings  !!!! this needs to be commented out later
source("/scratch/project_2000994/PREBASruns/finRuns/Rsrc/localSettings.r")

####load general settings and data
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
rcpfile <- rcps
pathFiles <- paste0("raster/forCent",r_no,"/")
####end load general settings and data


####load soil information raster
# pseudoptyp.img: Whole Finland, 100 = mineral soil, 400 = drained peatland, 700=other peatland, 0=non-forest
finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudopty.img")

###load npp first outside loop to get peatX
npp = raster(paste0("rasters/forCent",r_no,"/",
                    "npp_",min(per1),"-",max(per1),"_",
                    harvestscenarios,"_",rcpfile,".tif"))
peatX <- crop(finPeats,npp)

###load site type raster
fert=raster(paste0("rasters/forCent",r_no,"/",
                   "sitetype_",min(per1),"-",max(per1),".tif"))/3


for (i in 1:3) {
  curr = paste0("per",i)
  npp = raster(paste0("rasters/forCent",r_no,"/",
                      "npp_",min(get(curr)),"-",max(get(curr)),"_",
                      harvestscenarios,"_",rcpfile,".tif"))
  #writeRaster(peatX,filename = paste0("rasters/forCent",r_no,"/","/peatXtif"))
  nep = raster(paste0("rasters/forCent",r_no,"/",
                      "nep_",min(get(curr)),"-",max(get(curr)),"_",
                      harvestscenarios,"_",rcpfile,".tif"))
  nep = processPeat(peatX,fert,npp,nep,400,1)
  nep = processPeat(peatX,fert,npp,nep,400,2)
  writeRaster(nep,filename = paste0(pathFiles,
                                    "nepProcPeat_",min(get(curr)),"-",max(get(curr)),"_",
                                    harvestscenarios,"_",rcpfile,".tif"))
}


processPeat <- function(peatXf, fertf, nppf, nepf, peatval, fertval) {
  # mask out pixels where peatXf == peatval and fertx == fertval
  drPeatNeg <- peatXf == peatval & fertf == fertval
  drPeatNeg[drPeatNeg==0] <- NA
  drPeatP1F1 <- mask(nppf, drPeatNeg)
  if (fertval == 1) {
    drPeat <- drPeat - 270
  } else if (fertval == 2) {
    drPeat <- drPeat + 70
  }
  return(merge(drPeat,nepf))
}


###example code: loading rasters on a local machine for testing
###npp and nep have been renamed accordingly
load("D:/Forestry Work/R/8.7.21ex/test.rdata")
fert <- raster("D:/Forestry Work/R/8.7.21ex/sitetype_2017-2025.tif")
peatX <- raster("D:/Forestry Work/R/8.7.21ex/peatX.tif")
for (i in 1:3) {
  curr = paste0("per",i)
  npp <- raster(paste0("D:/Forestry Work/R/8.7.21ex/npp-",curr,".tif"))
  nep <- raster(paste0("D:/Forestry Work/R/8.7.21ex/NEP-",curr,".tif"))
  nep1 = processPeat(peatX,fert,npp,nep,400,1)
  nep2 = processPeat(peatX,fert,npp,nep1,400,2)
  plot(density(nep, plot=FALSE),main=paste0("Local run of nepProcPeat-per",i))
  lines(density(nep2, plot=FALSE), col = "red")
  legend("topleft", legend=c("Preproc","proc"), col=c("black","red"), lty=c(1,1))
}
