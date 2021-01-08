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

# sampleID=1
soilTotC <- rh <- data.table()

####Calculate steady state
soilCststXX <- list()
for(sampleID in sampleIDs){
# soilCststXX <- mclapply(1:115, function(sampleID) {
  sampleX <- ops[[sampleID]]
  # load("Kokemaenjoki/Rsrc/CurrClim.rdataBase_sample2.rdata")
  load(paste0("output/",rcps,ststScen,"_sample",sampleID,".rdata"))
  
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
  
  soilCxx <- .Fortran("StstYasso",litter=as.array(litter),
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
  soilCxx$soilC[,,1,1] <- soilCxx$soilC[,,1,1] + ststGV
  soilCxx$AWENgv <- AWENgv
  print(sampleID)
  # test[[sampleID]] <- xx[c(1,12)]
  print(range(apply(soilCxx$soilC/1e4,1,sum)))
  soilCststXX[[sampleID]] <- soilCxx
}
# return(soilCxx)
# }, mc.cores = nCores)      ## Split this job across 10 cores

  save(soilCststXX,file=paste0("outSoil/InitSoilCstst_",ststScen,".rdata"))
  

