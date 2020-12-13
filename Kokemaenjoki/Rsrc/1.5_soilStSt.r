devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")

###Settings
#Harvest scenarios
ststScen <- "Base"
runScen <- "Base" # "MaxSust" "Low"

###Process Yasso weather
# load("C:/Users/minunno/Documents/research/ibc-carbon/Kokemaenjoki/input/weatherYassoStstCurClim.rdata")
# load("C:/Users/minunno/Documents/research/ibc-carbon/Kokemaenjoki/input/weatherYassoAnnual.rdata")
load("input/weatherYassoStstCurClim.rdata")
load("input/weatherYassoAnnual.rdata")
weatherYasso <- array(0.,dim=c(3829, 84, 3))
for(i in 1:3829) weatherYasso[i,,] <- as.matrix(weatherYassoAnnual[id==i & year %in% 1:84,3:5])


set.seed(1)
ops <- split(data.all, sample(1:115, nrow(data.all), replace=T))

sampleID=8
soilTotC <- rh <- data.table()
soilCststXX <- list()
####Calculate steady state
# test <- list()
for(sampleID in 1:115){
  sampleX <- ops[[sampleID]]
  # load("Kokemaenjoki/Rsrc/CurrClim.rdataBase_sample2.rdata")
  load(paste0("output/CurrClim.rdata",ststScen,"_sample",sampleID,".rdata"))
  
  indx <- match(varNames[26],varNames[varSel])
  Lf <- apply(out$annual[,,indx,],c(1,3),mean,na.rm=T)
  indx <- match(varNames[27],varNames[varSel])
  Lfr <- apply(out$annual[,,indx,],c(1,3),mean,na.rm=T)
  Lnw <- Lf + Lfr
  indx <- match(varNames[28],varNames[varSel])
  Lfw <- apply(out$annual[,,indx,],c(1,3),mean,na.rm=T)
  indx <- match(varNames[29],varNames[varSel])
  Lw <- apply(out$annual[,,indx,],c(1,3),mean,na.rm=T)
  
  nSites <- dim(out$annual)[1]
  nLayers <- dim(out$annual)[4]
  litter <- array(0.,dim=c(nSites,nLayers,3))
  litter[,,1] <- Lnw
  litter[,,2] <- Lfw
  litter[,,3] <- Lw
  nSp <- 3
  litterSize <- matrix(0,3,nSp)
  litterSize[2,] <- 2
  litterSize[1,] <- c(30,30,10)
  
  species <- matrix(NA,nSites,nLayers)
  species[,1] <- 1; species[,2] <- 2; species[,3] <- 3
  soilC <- array(0.,dim=c(nSites,5,3,nLayers))
  nClimID <- 3829
  climIDs <- sampleX$CurrClimID
  
  soilCststXX[[sampleID]] <- .Fortran("StstYasso",litter=as.array(litter),
                 litterSize=as.array(litterSize),
                 nLayers=as.integer(nLayers), 
                 nSites=as.integer(nSites),
                 nSp=as.integer(nSp),
                 species=as.matrix(species),
                 nClimID=as.integer(nClimID),
                 climIDs=as.integer(climIDs),
                 pAWEN=as.matrix(parsAWEN),
                 pYasso=as.double(pYAS),
                 climate=as.matrix(climate),
                 soilC=as.array(soilC)) 

  ###calculate steady state C for gv
  fAPAR <- out$fAPAR
  fAPAR[which(is.na(out$fAPAR),arr.ind = T)] <- 0.
  AWENgv <- array(NA,dim=c(dim(out$fAPAR),4))
  for(ij in 1:nYears){
    AWENgv[,ij,] <- t(sapply(1:nrow(fAPAR), function(i) .Fortran("fAPARgv",fAPAR[i,ij],
                                                                 out$ets[i,ij],out$siteType[ij],
                                                                 0,0,out$p0[ij,1],rep(0,4))[[7]]))
  }
  AWENgv2 <- apply(AWENgv,c(1,3),mean,na.rm=T)
  
  
  ###calculate steady state soil C per GV
  # ststGV <- matrix(NA,nSites,5)
  ststGV <- t(sapply(1:nSites, function(ij) .Fortran("mod5c",
                                                     pYAS,1.,colMeans(weatherYasso[climIDs[ij],,]),rep(0,5),
                                                     c(AWENgv2[ij,],0),litterSize=0,leac=0.,rep(0,5),stSt=1.)[[8]]))
  ####add gvsoilc to first layer foliage soilC
  # check in normal runs where ground vegetation soilC is calculated
  
  print(sampleID)
  # test[[sampleID]] <- xx[c(1,12)]
  print(range(apply(soilCststXX[[sampleID]]$soilC/1e4,1,sum)))
}
  save(soilCststXX,file=paste0("outSoil/InitSoilCstst_",ststScen,".rdata"))
  
# # for(i in 1:10){
# #   print(i)
# #   print(range(apply(test[[i]]$soilC/1e4,1,sum)))
# # }
# 
#   
#   # ### Use new litter
#   # load(paste0("output1/CurrClim.rdataBase_sample",sampleID,".rdata"))
#   # Lf <- apply(out$annual[,,26,],c(1,3),mean,na.rm=T)
#   # Lfr <- apply(out$annual[,,27,],c(1,3),mean,na.rm=T)
#   # 
#   # Lnw <- Lf + Lfr
#   # Lfw <- apply(out$annual[,,28,],c(1,3),mean,na.rm=T)
#   # Lw <- apply(out$annual[,,29,],c(1,3),mean,na.rm=T)
#   # 
#   # nSites <- dim(out$annual)[1]
#   # nLayers <- dim(out$annual)[4]
#   # litter <- array(0.,dim=c(nSites,nLayers,3))
#   # litter[,,1] <- Lnw
#   # litter[,,2] <- Lfw
#   # litter[,,3] <- Lw
#   # nSp <- 3
#   # litterSize <- matrix(0,3,nSp)
#   # litterSize[2,] <- 2
#   # litterSize[1,] <- c(30,30,10)
#   # 
#   # species <- out$annual[,1,4,]
#   # soilC <- array(0.,dim=c(nSites,5,3,nLayers))
#   # nClimID <- 3829
#   # climIDs <- sampleX$CurrClimID
#   # ### end use new litter
#   
#   load(paste0("output/CurrClim.rdata",runScen,"_sample",sampleID,".rdata"))
#   
#   nYears <- dim(out$annual)[2]
#   Lnw <- out$annual[,,26,] + out$annual[,,27,]
#   Lfw <- out$annual[,,28,]
#   Lw <- out$annual[,,29,]
#   litter <- array(0.,dim=c(nSites, nYears, nLayers, 3))
#   litter[,,,1] <- Lnw
#   litter[,,,2] <- Lfw
#   litter[,,,3] <- Lw
#   litter[which(is.na(litter))] <- 0.
#   soilC <- array(0.,dim=c(nSites,(nYears+1),5,3,nLayers))
#   soilC[,1,,,] <- xx$soilC
#   
#   soilCsites <- .Fortran("runYasso",
#                          litter=as.array(litter),
#                          litterSize=as.array(litterSize),
#                          nYears=as.integer(nYears),
#                          nLayers=as.integer(nLayers), 
#                          nSites=as.integer(nSites),
#                          nSp=as.integer(nSp),
#                          species=as.matrix(species),
#                          nClimID=as.integer(nClimID),
#                          climIDs=as.integer(climIDs),
#                          pAWEN=as.matrix(parsAWEN),
#                          pYasso=as.double(pYAS),
#                          weatherYasso=as.matrix(weatherYasso),
#                          soilC=as.array(soilC)) 
#   
#   
#   soilC <- soilCsites[c(1,13)]
#   
#   # save(soilC,file=paste0("outSoil/soilC_sample",sampleID,".rdata"))
#   # print(sampleID)
#   # }
# # soilTotC <- rh <- data.table()
# # for(sampleID in 1:115){
# # load(paste0("outSoil/soilC_sample",sampleID,".rdata"))
# 
#   lit <- data.table(apply(soilC$litter,1:2,sum))
#   soilX <- data.table(apply(soilC$soilC,1:2,sum))
#   soilTotC <- rbind(soilTotC,soilX)
#   rh <- rbind(rh,(soilX[,1:84] - soilX[,2:85] + lit[,1:84])/10)
#   print(sampleID)
# }
# save(soilTotC,rh,file=pate0("outSoil/DTsoilC_",runScen,"_rh.rdata"))
# 
# load(pate0("outSoil/DTsoilC_",runScen,"_rh.rdata"))
# soilC <- soilTotC
# save(soilC,file=paste0("outputDT/soilC_",runScen,"_CurrClim.rdata"))
# Rh <- rh
# save(Rh,file=paste0("outputDT/Rh_",runScen,"_CurrClim.rdata"))
# load(paste0("outputDT/npp_",runScen,"_CurrClim.rdata"))
# NEP <- npp-Rh
# save(NEP,file=paste0("outputDT/NEP_",runScen,"_CurrClim.rdata"))
