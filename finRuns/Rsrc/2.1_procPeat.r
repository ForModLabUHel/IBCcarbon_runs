###load local settings  !!!! this needs to be commented out later
#source("/scratch/project_2000994/PREBASruns/finRuns/Rsrc/localSettings.r")

####load general settings and data
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
rcpfile <- rcps
pathFiles <- paste0("rasters/forCent",r_no,"/")
####end load general settings and data


####load soil information raster
soilSyke <- TRUE  ####If TRUE uses Syke peatland database if FALSE uses luke database
# luke database pseudoptyp.img: Whole Finland, 100 = mineral soil, 400 = drained peatland, 700=other peatland, 0=non-forest
# syke database peatSyke16res.tif: 1 = Undrained peatland; 2 = Drained peatland; 3 = Peat extraction area 
if(soilSyke){
  finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/peatSyke16res.tif")
  drPeatID <- 2  ### ID = 2 for syke database
}else{
  finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudoptyp.img")
  drPeatID <- 400  ### ID = 400 for luke database; 
}

###load npp first outside loop to get peatX
npp = raster(paste0("rasters/forCent",r_no,"/",
                    "npp_",min(per1),"-",max(per1),"_",
                    harvestscenarios,"_",rcpfile,".tif"))
peatX <- crop(finPeats,npp)

###load site type raster
fert=raster(paste0("rasters/forCent",r_no,"/",
                   "siteType.tif"))


for (i in 1:3) {
  curr = paste0("per",i)
  npp = raster(paste0("rasters/forCent",r_no,"/",
                      "npp_",min(get(curr)),"-",max(get(curr)),"_",
                      harvestscenarios,"_",rcpfile,".tif"))
  #writeRaster(peatX,filename = paste0("rasters/forCent",r_no,"/","/peatXtif"))
  nep = raster(paste0("rasters/forCent",r_no,"/",
                      "NEP sp_",min(get(curr)),"-",max(get(curr)),"_",
                      harvestscenarios,"_",rcpfile,".tif"))
  nep = processPeat(peatX,fert,npp,nep,drPeatID,1)
  nep = processPeat(peatX,fert,npp,nep,drPeatID,2)
  writeRaster(nep,filename = paste0(pathFiles,
                                    "nepProcPeat_",min(get(curr)),"-",max(get(curr)),"_",
                                    harvestscenarios,"_",rcpfile,".tif"))
}



###example code: loading rasters on a local machine for testing
###npp and nep have been renamed accordingly
# load("D:/Forestry Work/R/8.7.21ex/test.rdata")
# fert <- raster("D:/Forestry Work/R/8.7.21ex/sitetype_2017-2025.tif")
# peatX <- raster("D:/Forestry Work/R/8.7.21ex/peatX.tif")
# for (i in 1:3) {
#   curr = paste0("per",i)
#   npp <- raster(paste0("D:/Forestry Work/R/8.7.21ex/npp-",curr,".tif"))
#   nep <- raster(paste0("D:/Forestry Work/R/8.7.21ex/NEP-",curr,".tif"))
#   nep1 = processPeat(peatX,fert,npp,nep,400,1)
#   nep2 = processPeat(peatX,fert,npp,nep1,400,2)
#   plot(density(nep, plot=FALSE),main=paste0("Local run of nepProcPeat-per",i))
#   lines(density(nep2, plot=FALSE), col = "red")
#   legend("topleft", legend=c("Preproc","proc"), col=c("black","red"), lty=c(1,1))
# }
