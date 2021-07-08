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

###load npp
npp=raster(paste0("rasters/forCent",r_no,"/",
                  "npp_",min(per1),"-",max(per1),"_",
                  harvestscenarios,"_",rcpfile,".tif"))

####crop soil info raster to the region of interest 
peatX <- crop(finPeats,npp)
#writeRaster(peatX,filename = paste0("rasters/forCent",r_no,"/","/peatXtif"))

###load site type raster
fert=raster(paste0("rasters/forCent",r_no,"/",
                  "sitetype_",min(per1),"-",max(per1),".tif"))/3



example code: loading rasters on a local machine
#load("D:/Forestry Work/R/8.7.21ex/test.rdata")
#peatX <- raster("D:/Forestry Work/R/8.7.21ex/peatX.tif")
#fert <- raster("D:/Forestry Work/R/8.7.21ex/sitetype_2017-2025.tif")
#npp <- raster("D:/Forestry Work/R/8.7.21ex/npp_2017-2025_MaxSust_CurrClim.tif")
#nep <- raster("D:/Forestry Work/R/8.7.21ex/NEP sp_2017-2025_MaxSust_CurrClim.tif")



####what needs to be developed for the 3 periods:
#recalculate NEP for drained peatlands (400)
1a. mask out the pixel where peatX == 400 and fert == 1
example code:
drPeatNeg <- peatX == 400 & fert == 1
drPeatNeg[drPeatNeg==0] <- NA
drPeatP1F1 <- mask(npp, drPeatNeg)
drPeatP1F1 <- drPeatP1F1 - 270

2a. replace the NEP of those pixels with npp - 270
nep1 <- merge(drPeatP1F1,nep)
#overlap, default first layer (drPeatP1F1) overrides

###fert = site type
1b. mask out the pixel where peatX == 400 and fert == 2
example code:
drPeatNeg <- peatX == 400 & fert == 2
drPeatNeg[drPeatNeg==0] <- NA
drPeatP1F2 <- mask(npp, drPeatNeg)
drPeatP1F2 <- drPeatP1F2 + 70

2b. replace the NEP of those pixels with npp + 70
#nepProc will merge original NEP and newNEP for drained peatlands
nep1 <- merge(drPeatP1F2,nep1) # same as before but now drPeatP1F2 and merging with the new nep1 from before
