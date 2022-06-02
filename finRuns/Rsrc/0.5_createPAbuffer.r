library(Rprebasso)
library(raster)
library(data.table)
library(sf)
library(stars)
library(dplyr)

# r_no <- 5

pathX <- "/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/"
# pathX <- "C:/Users/checcomi/Downloads/New folder/"

load(paste0(pathX,"maakunta_",r_no,"_IDsTab.rdata"))
load(paste0(pathX,"data.all_maakunta_",r_no,".rdata"))

setkey(data.IDs,maakuntaID)
setkey(data.all,maakuntaID)


consX <- merge(data.IDs[,.(maakuntaID,x,y)],data.all[,.(maakuntaID,cons)])

consRast <- rasterFromXYZ(consX[,2:4])
consRast[consRast==0] <- NA
maakIDrast <- rasterFromXYZ(consX[,c(2:3,1)])

r.to.poly <- st_as_sf(st_as_stars(consRast), 
                                as_points = FALSE, merge = TRUE)
 # requires the sf, sp, raster and stars packages

buf <- st_buffer(r.to.poly, dist = 200)

###write shapes:
# st_write(buf, paste0(pathX,"maakunta_",r_no,"_bufPAs.shp"))
# st_write(r.to.poly, paste0(pathX,"maakunta_",r_no,"_PAs.shp"))

maakIDbuf <- mask(maakIDrast,buf)

kk <- data.table(rasterToPoints(maakIDbuf))
setnames(kk,c("x","y","maakuntaID"))
# plot(r.to.poly)
# plot(buf,col=2,add=T)
setkey(kk,x,y,maakuntaID)
setkey(data.IDs,x,y,maakuntaID)
kk$consBuf <- 1
gg <- merge(kk,data.IDs,all=T)
setkey(gg,maakuntaID,consBuf)

# writeRaster(consRast,filename = paste0("test",r_no,".tif"))
  
# st_write(r.to.poly, paste0(pathX,"poly",r_no, ".shp"))
# st_write(buf, paste0(pathX,"polyBuf",r_no, ".shp"))

bufPix <- gg[, .(.N), by = .(maakuntaID,consBuf)]

buffDat <- data.table()

toSplit <- data.all[cons==0 & maakuntaID %in% bufPix[consBuf==1]$maakuntaID]
data.allOld <- data.all
data.all$Wbuffer=0
nX <- nrow(toSplit)

for(i in 1:nX){
  ID <- toSplit$maakuntaID[i]
  newID <- max(data.all$maakuntaID) + i
  nCons = bufPix[maakuntaID==ID & consBuf==1]$N
  # data.all[maakuntaID==ID,nPix:=nPix-nCons]
  outBuf <- inBuf <- data.all[maakuntaID==ID]
  outBuf[maakuntaID==ID,nPix:=nPix-nCons]
  inBuf$oldMaakID <- outBuf$oldMaakID <- ID
  inBuf$cons = inBuf$Wbuffer=1
  inBuf$nPix = nCons
  inBuf$maakuntaID = newID
  buffDat <- rbind(buffDat,inBuf)
  buffDat <- rbind(buffDat,outBuf)
  # gg[maakuntaID == ID & consBuf==1,maakuntaID := newID]
  if(i %% 1000==0) print(paste0(i," of ",nX))
}

buf.IDs <- gg[maakuntaID %in% toSplit$maakuntaID & consBuf==1]
buf.IDs$newMaakuntaID <- buf.IDs$maakuntaID
newIDs <- max(data.all$maakuntaID) + 1:length(toSplit$maakuntaID)
tabX <- cbind(toSplit$maakuntaID,
              newIDs)
buf.IDs$newMaakuntaID <- newIDs[match(buf.IDs$maakuntaID,tabX)]
setnames(buf.IDs,"consBuf","newCons")
setnames(buffDat,"Wbuffer","newCons")

save(buf.IDs,buffDat, 
     file=paste0(pathX,"maakunta_",r_no,"_IDsBuffer.rdata"))

# hj <- merge(data.IDs, jj, by = c("x", "y"), all = FALSE)



# models outputs to NAs, outputDT, initSoilC and plots
Sys.chmod(list.dirs("NAs"), "0777",use_umask=FALSE)
f <- list.files("NAs", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("outputDT"), "0777",use_umask=FALSE)
f <- list.files("outputDT", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("initSoilC"), "0777",use_umask=FALSE)
f <- list.files("initSoilC", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("plots"), "0777",use_umask=FALSE)
f <- list.files("plots", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("input"), "0777",use_umask=FALSE)
f <- list.files("input", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)

Sys.chmod(list.dirs("rasters"), "0777",use_umask=FALSE)
f <- list.files("rasters", all.files = TRUE, full.names = TRUE, recursive = TRUE)
Sys.chmod(f, (file.info(f)$mode | "0777"),use_umask=FALSE)