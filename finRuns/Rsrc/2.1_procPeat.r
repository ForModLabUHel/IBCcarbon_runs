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

##!!###step to reduce the size of the peat raster 
###load npp first outside loop to get peatX
npp = raster(paste0("rasters/forCent",r_no,"/",
                    "npp_",min(per1),"-",max(per1),
                    "_harscen",harvScen,
                    "_harInten",harvInten,rcpfile,".tif"))
peatX <- crop(finPeats,npp)
##!!## end

###load site type raster
fert=raster(paste0("rasters/forCent",r_no,"/",
                   "siteType.tif"))

#####Loop along periods
for (i in 1:3) {
  curr = paste0("per",i)
  npp = raster(paste0("rasters/forCent",r_no,"/",
                      "npp_",min(get(curr)),"-",max(get(curr)),
                      "_harscen",harvScen,
                      "_harInten",harvInten,rcpfile,".tif"))
  lit_cWoody <- raster(paste0("rasters/forCent",r_no,"/",
                              "Litter_cWoody_",min(get(curr)),"-",max(get(curr)),
                              "_harscen",harvScen,
                              "_harInten",harvInten,rcpfile,".tif"))
  lit_fol <- raster(paste0("rasters/forCent",r_no,"/",
                           "Litter_fol_",min(get(curr)),"-",max(get(curr)),
                           "_harscen",harvScen,
                           "_harInten",harvInten,rcpfile,".tif"))
  lit_fr <- raster(paste0("rasters/forCent",r_no,"/",
                          "Litter_fr_",min(get(curr)),"-",max(get(curr)),
                          "_harscen",harvScen,
                          "_harInten",harvInten,rcpfile,".tif"))
  lit_fWoody <- raster(paste0("rasters/forCent",r_no,"/",
                              "Litter_fWoody_",min(get(curr)),"-",max(get(curr)),
                              "_harscen",harvScen,
                              "_harInten",harvInten,rcpfile,".tif"))
  #writeRaster(peatX,filename = paste0("rasters/forCent",r_no,"/","/peatXtif"))
  nep = raster(paste0("rasters/forCent",r_no,"/",
                      "NEP sp_",min(get(curr)),"-",max(get(curr)),
                      "_harscen",harvScen,
                      "_harInten",harvInten,rcpfile,".tif"))
  npp_lit <- npp - lit_cWoody/10 - lit_fol/10 - lit_fr/10 - lit_fWoody/10
  nep = processPeat(peatX,fert,npp_lit,nep,drPeatID,1)
  nep = processPeat(peatX,fert,npp_lit,nep,drPeatID,2)
  writeRaster(nep,filename = paste0(pathFiles,
                                    "nepProcPeat_",min(get(curr)),"-",max(get(curr)),
                                    "_harscen",harvScen,
                                    "_harInten",harvInten,rcpfile,".tif"))
}

# procPeat outputs to rasters and plots
Sys.chmod(list.dirs("rasters"), "0777",use_umask=FALSE)
f <- list.files("rasters", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("plots"), "0777",use_umask=FALSE)
f <- list.files("plots", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)


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
