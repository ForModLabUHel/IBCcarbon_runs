library(raster)
library(rgdal)
library(data.table)

###ls is the region of Finland from which to extract the data
ls <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/ls_seg2.img")

####read shapefiles
kokeShp <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/", layer = "Koke_Paavesistoalue_VALUE")
forCent <- readOGR(dsn = "/scratch/project_2000994/PREBASruns/Kokemaenjoki/shapes/",layer = "mkeskus13tm35")

# extract segments ID for Kokemaenjoki river basin
kokenIDs <- mask(ls,kokeShp)
##mask peat info
# ops <- mask(ciao,kokeShp)

# writeRaster(kokenIDs,filename = "kokenSegIds.tif")
kokeIDsTab <- data.table(rasterToPoints(kokenIDs))
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


