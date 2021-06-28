# r_no <- regions <- 12
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
rcpfile <- rcps
pathFiles <- paste0("outputDT/forCent",r_no,"/")

load(paste0("input/forCent_",r_no,"_IDsTab.rdata"))
forCentIDsTab <- forCentIDsTab[segID!=0]
setkey(forCentIDsTab,segID)
pdf(paste0("plots/histRast_",r_no,"_",
           harvestscenarios,"_",rcpfile,".pdf"))

varXs <- c(varNames[varSel], specialVars)
for(varX in varXs){
  # varX <- varXs[1]
  fileXs <- list.files(path = paste0(pathtoken,pathFiles), pattern = paste0(varX,"_",harvestscenarios,"_",rcps))
  outX <- data.table()
  for(i in 1:length(fileXs)){
    load(paste0(pathFiles,fileXs[i]))
    outX <- rbind(outX,get(varX))
  }
  
  setkey(outX,segID)

  tabX <- merge(outX,forCentIDsTab)

  
  # can make a loop 
  rastX <- rasterFromXYZ(tabX[,.(x,y,per1)])
  crs(rastX) <- crsX
  writeRaster(rastX,filename = paste0("rasters/forCent",r_no,"/",
                                      varX,"_",min(per1),"-",max(per1),"_",
                                      harvestscenarios,"_",rcpfile,".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per1"))
  
  rastX <- rasterFromXYZ(tabX[,.(x,y,per2)])
  crs(rastX) <- crsX
  writeRaster(rastX,filename = paste0("rasters/forCent",r_no,"/",
                                      varX,"_",min(per2),"-",max(per2),"_",
                                      harvestscenarios,"_",rcpfile,".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per2"))
  
  rastX <- rasterFromXYZ(tabX[,.(x,y,per3)])
  crs(rastX) <- crsX
  writeRaster(rastX,filename = paste0("rasters/forCent",r_no,"/",
                          varX,"_",min(per3),"-",max(per3),"_",
                          harvestscenarios,"_",rcpfile,".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per3"))
 
  if(varX!="DeadWoodVolume")  file.remove(paste0(pathFiles,fileXs))
  print(varX)
}
dev.off()

rm(list=ls());gc()


print("checking data")
years <- c("2017-2025", "2026-2033", "2034-2050")


for(i in 1:3) {
  
  GPPpath <- paste0("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent12/GPPspecies_", years[i], ".tif")
  NEPpath <- paste0("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent12/NEP sp_", years[i], ".tif")
  
  # read in the rasters for GPP and NEP
  GPP_raster <- raster(GPPpath)
  NEP_raster <- raster(NEPpath)
  
  # change the non-NA values to 1
  GPP_raster[!is.na(GPP_raster)] <- 1
  
  # change the NA values to 0
  GPP_raster[is.na(GPP_raster)] <- 0
  
  # take from the NEP raster only the points that are not NA in the GPP raster
  NEP_raster[!is.na(NEP_raster)] <- 1
  # change the points to 0 that are 0 in GPP
  NEP_raster[GPP_raster==0] <- 0
  
  # transform raster to points
  NEP_points <- rasterToPoints(is.na(NEP_raster))
  
  # pick the points with strange NA values
  NA_points <- NEP_points[which(NEP_points[,3]==1),]
  
  ids <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/la_seg2.img")
  idx <- unique(extract(ids,NA_points[,1:2]))
  savepath <- paste0("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent12/NApoints/NApoints",
                     years[i],"_",harvestscenarios,"_",rcpfile, ".rdata")
  save(idx,NA_points, file=savepath)
}