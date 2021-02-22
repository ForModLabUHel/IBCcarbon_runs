library(data.table)

####amaa
regX <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/amaa.txt")
data.all <- regX
setnames(data.all,"consArea","cons")
save(data.all,file="input/data.all_forCent_13.rdata")
rm(list=ls());gc()

#### is
regX <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/is.txt")
for(forCent in c(3,5,8,9)){
  load(paste0("input/forCent_",forCent,"_IDsTab.rdata"))
  segIDx <- unique(forCentIDsTab$segID)
  data.all <- regX[segID %in% segIDx]
  setnames(data.all,"consArea","cons")
  save(data.all,file=paste0("input/data.all_forCent_",forCent,".rdata"))
}
rm(list=ls());gc()

#### ls
regX <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/ls.txt")
for(forCent in c(1,2,4,6:7,14,15)){
  load(paste0("input/forCent_",forCent,"_IDsTab.rdata"))
  segIDx <- unique(forCentIDsTab$segID)
  data.all <- regX[segID %in% segIDx]
  setnames(data.all,"consArea","cons")
  save(data.all,file=paste0("input/data.all_forCent_",forCent,".rdata"))
}
rm(list=ls());gc()

#### pk
regX <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/pk.txt")
for(forCent in c(10,11)){
  load(paste0("input/forCent_",forCent,"_IDsTab.rdata"))
  segIDx <- unique(forCentIDsTab$segID)
  data.all <- regX[segID %in% segIDx]
  setnames(data.all,"consArea","cons")
  save(data.all,file=paste0("input/data.all_forCent_",forCent,".rdata"))
}
rm(list=ls());gc()

#### la
data.all <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/la.txt")
setnames(data.all,"consArea","cons")
save(data.all,file=paste0("input/data.all_forCent_",forCent,".rdata"))
rm(list=ls());gc()




# # writeRaster(kokenIDs,filename = "kokenSegIds.tif")
# kokeIDsTab <- data.table(rasterToPoints(kokenIDs))
# setnames(kokeIDsTab,c("long","lat"))
# segIDsKoken <- kokeIDsTab[,.N,by=ls_seg2] ###counting the number of pixels for each segID
# segIDsKoken <- segIDsKoken[ls_seg2!=0]
# setnames(segIDsKoken,c("segID","N"))
# 
# lsTab <- fread("/scratch/project_2000994/MVMIsegments/mvmi15/proc.data/ls.txt")
# 
# 
# kokeInputs <-  lsTab[segID %in% segIDsKoken$segID]
# kokeInputs <- merge(kokeInputs,segIDsKoken)
# setwd("/scratch/project_2000994/PREBASruns/Kokemaenjoki/input")
# save(segIDsKoken,kokeIDsTab,file="segIDkoke.rdata")
# fwrite(kokeInputs,file = "kokeInputs")
# writeRaster(kokenIDs,filename = "kokenSegIds.tif")
# 
# 
