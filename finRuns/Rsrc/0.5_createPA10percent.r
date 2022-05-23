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
cons10 <- raster(paste0("/scratch/project_2000994/PREBASruns/metadata/Local_protection/maak_",r_no,"_local_protection.tif"))

setkey(data.IDs,maakuntaID)
setkey(data.all,maakuntaID)


# consX <- merge(data.IDs[,.(maakuntaID,x,y)],data.all[,.(maakuntaID,cons)])

# consRast <- rasterFromXYZ(consX[,2:4])
# consRast[consRast==0] <- NA
maakIDrast <- rasterFromXYZ(data.IDs[,.(x,y,maakuntaID)])
cons10[cons10==0] <- NA
# plot(cons10)
cons10x <- resample(cons10,maakIDrast,method="ngb")

maakIDcons10 <- mask(maakIDrast, cons10x)

# r.to.poly <- st_as_sf(st_as_stars(consRast), 
# as_points = FALSE, merge = TRUE)
# requires the sf, sp, raster and stars packages

# buf <- st_buffer(r.to.poly, dist = 200)

###write shapes:
# st_write(buf, paste0(pathX,"maakunta_",r_no,"_bufPAs.shp"))
# st_write(r.to.poly, paste0(pathX,"maakunta_",r_no,"_PAs.shp"))

# maakIDbuf <- mask(maakIDrast,buf)

kk <- data.table(rasterToPoints(maakIDcons10))
setnames(kk,c("x","y","maakuntaID"))
# plot(r.to.poly)
# plot(buf,col=2,add=T)
setkey(kk,x,y,maakuntaID)
setkey(data.IDs,x,y,maakuntaID)
kk$cons10 <- 1
gg <- merge(kk,data.IDs,all=T)
setkey(gg,maakuntaID,cons10)

# writeRaster(consRast,filename = paste0("test",r_no,".tif"))

# st_write(r.to.poly, paste0(pathX,"poly",r_no, ".shp"))
# st_write(buf, paste0(pathX,"polyBuf",r_no, ".shp"))

cons10Pix <- gg[, .(.N), by = .(maakuntaID,cons10)]

cons10Dat <- data.table()

toSplit <- data.all[cons==0 & maakuntaID %in% cons10Pix[cons10==1]$maakuntaID]
data.allOld <- data.all
data.all$cons10=0
nX <- nrow(toSplit)

for(i in 1:nX){
  ID <- toSplit$maakuntaID[i]
  newID <- max(data.all$maakuntaID) + i
  nCons = cons10Pix[maakuntaID==ID & cons10==1]$N
  # data.all[maakuntaID==ID,nPix:=nPix-nCons]
  outCons10 <- inCons10 <- data.all[maakuntaID==ID]
  outCons10[maakuntaID==ID,nPix:=nPix-nCons]
  inCons10$oldMaakID <- outCons10$oldMaakID <- ID
  inCons10$cons = inCons10$Wbuffer=1
  inCons10$nPix = nCons
  inCons10$maakuntaID = newID
  cons10Dat <- rbind(cons10Dat,inCons10)
  cons10Dat <- rbind(cons10Dat,outCons10)
  # gg[maakuntaID == ID & consBuf==1,maakuntaID := newID]
  if(i %% 1000==0) print(paste0(i," of ",nX))
}

cons10.IDs <- gg[maakuntaID %in% toSplit$maakuntaID & cons10==1]
cons10.IDs$newMaakuntaID <- cons10.IDs$maakuntaID
newIDs <- max(data.all$maakuntaID) + 1:length(toSplit$maakuntaID)
tabX <- cbind(toSplit$maakuntaID,
              newIDs)
cons10.IDs$newMaakuntaID <- newIDs[match(cons10.IDs$maakuntaID,tabX)]

save(cons10.IDs,cons10Dat, 
     file=paste0(pathX,"maakunta_",r_no,"_IDsCons10.rdata"))

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