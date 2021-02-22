.libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")

###Settings
#Harvest scenarios
ststScen <- "Base"
runScens <- c("MaxSust","Low","Base") # "MaxSust" "Low" "Base"

###Process Yasso weather
 load("weatherYassoStstCurClim.rdata")
 load("weatherYassoAnnual.rdata")
 weatherYasso <- array(0.,dim=c(3829, 84, 3))
 for(i in 1:3829) weatherYasso[i,,] <- as.matrix(weatherYassoAnnual[id==i & year %in% 1:84,3:5])


set.seed(1)
ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))

sampleID=8
runScen ="MaxSust"

####Load steady state soilC
  load(file=paste0("outSoil/InitSoilCstst_",ststScen,".rdata"))
for(runScen in runScens){
####load model Outputs
soilTotC <- rh <- data.table()
 for(sampleID in 1:115){
   sampleX <- ops[[sampleID]]
   load(paste0("output/CurrClim.rdata",runScen,"_sample",sampleID,".rdata"))
   
   nSites <- dim(out$annual)[1]
   nLayers <- dim(out$annual)[4]
   nYears <- dim(out$annual)[2]
   nSp <- 3
   litterSize <- matrix(0,3,nSp)
   litterSize[2,] <- 2
   litterSize[1,] <- c(30,30,10)
   species <- out$annual[,1,4,]
   nClimID <- 3829
   climIDs <- sampleX$CurrClimID
   
   Lnw <- out$annual[,,26,] + out$annual[,,27,]
   Lfw <- out$annual[,,28,]
   Lw <- out$annual[,,29,]
   litter <- array(0.,dim=c(nSites, nYears, nLayers, 3))
   litter[,,,1] <- Lnw
   litter[,,,2] <- Lfw
   litter[,,,3] <- Lw
   litter[which(is.na(litter))] <- 0.
   soilC <- array(0.,dim=c(nSites,(nYears+1),5,3,nLayers))
   soilC[,1,,,] <- soilCststXX[[sampleID]]$soilC
   
   soilCsites <- .Fortran("runYasso",
                          litter=as.array(litter),
                          litterSize=as.array(litterSize),
                          nYears=as.integer(nYears),
                          nLayers=as.integer(nLayers), 
                          nSites=as.integer(nSites),
                          nSp=as.integer(nSp),
                          species=as.matrix(species),
                          nClimID=as.integer(nClimID),
                          climIDs=as.integer(climIDs),
                          pAWEN=as.matrix(parsAWEN),
                          pYasso=as.double(pYAS),
                          weatherYasso=as.matrix(weatherYasso),
                          soilC=as.array(soilC)) 
   
   soilC <- soilCsites[c(1,13)]
   
 
   lit <- data.table(apply(soilC$litter,1:2,sum,na.rm=T))
   soilX <- data.table(apply(soilC$soilC,1:2,sum,na.rm=T))
   soilTotC <- rbind(soilTotC,soilX)
   rh <- rbind(rh,(soilX[,1:84] - soilX[,2:85] + lit[,1:84])/10)
   print(sampleID)
 }
print(runScen)
save(soilTotC,rh,file=paste0("outSoil/DTsoilC_rh_",runScen,"_rh.rdata"))
print(paste(runScen, "saved"))


load(paste0("outSoil/DTsoilC_rh_",runScen,"_rh.rdata"))
soilC <- soilTotC
save(soilC,file=paste0("outputDT/soilC_",runScen,"_CurrClim.rdata"))
Rh <- rh
save(Rh,file=paste0("outputDT/Rh_",runScen,"_CurrClim.rdata"))
load(paste0("outputDT/npp_",runScen,"_CurrClim.rdata"))
print("dim npp")
print(dim(npp))
print("dim Rh")
print(dim(Rh))
NEP <- npp-Rh
save(NEP,file=paste0("outputDT/NEP_",runScen,"_CurrClim.rdata"))
print(paste(runScen, "all done"))
rm(Rh,rh,NEP,soilC,npp)
gc()
}