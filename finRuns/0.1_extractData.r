
###raster from which to extract the data
rastSeg <- raster(rastSegFN)

# extract segments ID for Kokemaenjoki river basin
kokenIDs <- mask(rastData,maskX)
##mask peat info
# ops <- mask(ciao,maskX)

# writeRaster(kokenIDs,filename = "kokenSegIds.tif")
kokeIDsTab <- data.table(rasterToPoints(kokenIDs))
setnames(kokeIDsTab,c("long","lat"))
segIDsKoken <- kokeIDsTab[,.N,by=ls_seg2] ###counting the number of pixels for each segID
segIDsKoken <- segIDsKoken[ls_seg2!=0]
setnames(segIDsKoken,c("segID","N"))

lsTab <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/ls.txt")


kokeInputs <-  lsTab[segID %in% segIDsKoken$segID]
kokeInputs <- merge(kokeInputs,segIDsKoken)
setwd("/scratch/project_2000994/PREBASruns/Kokemaenjoki/input")
save(segIDsKoken,kokeIDsTab,file="segIDkoke.rdata")
fwrite(kokeInputs,file = "kokeInputs")
writeRaster(kokenIDs,filename = "kokenSegIds.tif")


