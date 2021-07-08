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

###load site type raster
fert=raster(paste0("rasters/forCent",r_no,"/",
                  "sitetype_",min(per1),"-",max(per1),".tif"))/3


####what needs to be developed for the 3 periods:
#recalculate NEP for drained peatlands (400)
1a. mask out the pixel where peatX == 400 and fert == 1
example code:
  ll <- peatX==400 & fert==1
  ll[ll==0] <- NA
  drPeat <- mask(npp, ll)
  drPeat <- drPeat - 270

2a. replace the NEP of those pixels with npp - 270


1b. mask out the pixel where peatX == 400 and fert == 2
example code:
  ll <- peatX==400 & fert==2
  ll[ll==0] <- NA
  drPeat <- mask(npp, ll)
  drPeat <- drPeat + 70

2b. replace the NEP of those pixels with npp + 70

