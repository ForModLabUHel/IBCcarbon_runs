# r_no <- regions <- 12
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

pathFiles <- paste0("outputDT/forCent",r_no,"/")

load(paste0("input/forCent_",r_no,"_IDsTab.rdata"))
forCentIDsTab <- forCentIDsTab[segID!=0]
setkey(forCentIDsTab,segID)
pdf(paste0("plots/histRast_",r_no,".pdf"))

varXs <- c(varNames[varSel], specialVars)
for(varX in varXs){
  # varX <- varXs[1]
  fileXs <- list.files(path = paste0(pathtoken,pathFiles), pattern = paste0(varX,"_"))
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
                                      varX,"_",min(per1),"-",max(per1),".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per1"))
  
  rastX <- rasterFromXYZ(tabX[,.(x,y,per2)])
  crs(rastX) <- crsX
  writeRaster(rastX,filename = paste0("rasters/forCent",r_no,"/",
                                      varX,"_",min(per2),"-",max(per2),".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per2"))
  
  rastX <- rasterFromXYZ(tabX[,.(x,y,per3)])
  crs(rastX) <- crsX
  writeRaster(rastX,filename = paste0("rasters/forCent",r_no,"/",
                                      varX,"_",min(per3),"-",max(per3),".tiff"),overwrite=T)
  hist(rastX, main = paste(varX,"per3"))
  
  file.remove(paste0(pathFiles,fileXs))
  print(varX)
}
dev.off()

print("checking data")
GPP_raster <- paste0("rasters/forCent",r_no,"/GPPspecies",
       "_",min(per1),"-",max(per1),".tif")
NEP_raster <- paste0("rasters/forCent",r_no,"/NEP sp",
              "_",min(per1),"-",max(per1),".tif")
GPP_tab <- data.table(rasterToPoints(GPP_raster))
NEP_tab <- data.table(rasterToPoints(NEP_raster))
  ifelse(test = nrow(GPP_tab) == nrow(NEP_tab),
         yes = print("NA test OK"),
          no= print(paste("NAs do not match!",nrow(GPP_tab) - nrow(NEP_tab), "NAs in NEP")))