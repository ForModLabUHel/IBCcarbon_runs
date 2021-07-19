# analyse mortality 
library(data.table)
library(Rprebasso)

load("C:/Users/checcomi/Documents/research/IBC-carbon/sampleRuns/sample_12_reg1_Base_CurrClim.rdata")


pMort <- function(modOut,ageClass, rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  pMortX <- rep(0.,length(endX))
  
  for(i in 1:length(startX)){
    ageX <-rowMeans(modOut[,startX[i]:endX[i],7,1,1])
    cX <- which(ageX %in% ageClass)
    # outX <- modOut[cX,,,,]
    mortX <- data.table(which(modOut[cX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
    nMort <- length(unique(mortX$site))
    pMortX[i] <- nMort/length(cX)
  }
  return(pMortX)
}


pMort40 <- pMort(modOut,31:50)
pMort60 <- pMort(modOut,51:70)
pMort80 <- pMort(modOut,71:90)
pMort100 <- pMort(modOut,91:110)
pMort120 <- pMort(modOut,111:130)
pMort140 <- pMort(modOut,131:200)
plot(pMort40,ylim=c(0,1),type="l")
lines(pMort60,col=2)
lines(pMort80,col=3)
lines(pMort100,col=4)
lines(pMort120,col=5)
lines(pMort140,col=6)


sp=1
pureFor <- 0.8
mixFor <- 0.8

###Function to calculate the probability of a mortality (pM) event occuring
# Arguments: 
# modOut = output array from a PREBAS multisite runs
# rangeYear = number of years  for which to calculate pM
# sp = species/layer for which to calculate pM it can be a vector for combinations of species
# pureFor = proportion of Basal area to consider as pure stands
# mixFor = it works only for mixed forests, it is the minimum proportion of basal area for the species of interest

pMort2 <- function(modOut,ageClass, rangeYear=5,sp,pureFor,mixFor){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  pMortX <- rep(0.,length(endX))
  
  for(i in 1:length(startX)){
    ageX <-rowMeans(modOut[,startX[i]:endX[i],7,1,1])
    pBA <- apply(modOut[,startX[i]:endX[i],13,,1],c(1,3),mean)
    pBA <- pBA/rowSums(pBA)
    if(length(sp)==1){
      selX <- which(ageX %in% ageClass & pBA[,sp]>pureFor)
    }else{
      selX <- which(ageX %in% ageClass & rowSums(pBA[,sp])>mixFor &
                      pBA[,1]<pureFor & pBA[,2]<pureFor)  
    }
    
    # outX <- modOut[cX,,,,]
    mortX <- data.table(which(modOut[selX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
    nMort <- length(unique(mortX$site))
    pMortX[i] <- nMort/length(selX)
  }
  return(pMortX)
}

pMortX <- list()
ageClass <- 31:40
##Pure Pine
pMortX[[1]] <- pMort2(modOut,ageClass,sp=1, pureFor = 0.8, mixFor = 0.7)
##Pine Spruce
pMortX[[2]] <- pMort2(modOut,ageClass,sp=1:2, pureFor = 0.8, mixFor = 0.7)
##Spruce
pMortX[[3]] <- pMort2(modOut,ageClass,sp=2, pureFor = 0.8, mixFor = 0.7)
##Spruce deciduous
pMortX[[4]] <- pMort2(modOut,ageClass,sp=2:3, pureFor = 0.8, mixFor = 0.7)
##deciduous
pMortX[[5]] <- pMort2(modOut,ageClass,sp=3, pureFor = 0.8, mixFor = 0.7)
nameFor <- c("pine","piSp","spruce","spDec","decid")
names(pMortX) <- nameFor
yearsX <- 1:length(pMortX[[1]])
barplot(c(mean(pMortX[[1]][yearsX]),mean(pMortX[[2]][yearsX]),
          mean(pMortX[[3]][yearsX]),mean(pMortX[[4]][yearsX]),
          mean(pMortX[[5]][yearsX])),ylim=c(0,1), main="ageClass40",
        names = nameFor)

pMortX <- list()
ageClass <- 71:90
##Pure Pine
pMortX[[1]] <- pMort2(modOut,ageClass,sp=1, pureFor = 0.8, mixFor = 0.8)
##Pine Spruce
pMortX[[2]] <- pMort2(modOut,ageClass,sp=1:2, pureFor = 0.8, mixFor = 0.7)
##Spruce
pMortX[[3]] <- pMort2(modOut,ageClass,sp=2, pureFor = 0.8, mixFor = 0.7)
##Spruce deciduous
pMortX[[4]] <- pMort2(modOut,ageClass,sp=2:3, pureFor = 0.8, mixFor = 0.7)
##deciduous
pMortX[[5]] <- pMort2(modOut,ageClass,sp=3, pureFor = 0.8, mixFor = 0.7)
nameFor <- c("pine","piSp","spruce","spDec","decid")
names(pMortX) <- nameFor
yearsX <- 15:length(pMortX[[1]])
barplot(c(mean(pMortX[[1]][yearsX]),mean(pMortX[[2]][yearsX]),
          mean(pMortX[[3]][yearsX]),mean(pMortX[[4]][yearsX]),
          mean(pMortX[[5]][yearsX])),ylim=c(0,1), main="ageClass40",
        names = nameFor)
