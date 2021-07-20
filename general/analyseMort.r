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
  pMortX <- nSites <- rep(0.,length(endX))
  
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
    nSites[i] <- length(selX)
  }
  return(list(pMort=pMortX,nSites=nSites))
}

pMortX <- list()
ageClass <- 31:50
##Pure Pine
pMortX$pine <- pMort2(modOut,ageClass,sp=1, pureFor = 0.8, mixFor = 0.7)
##Pine Spruce
pMortX$piSp <- pMort2(modOut,ageClass,sp=1:2, pureFor = 0.8, mixFor = 0.7)
##Spruce
pMortX$spruce <- pMort2(modOut,ageClass,sp=2, pureFor = 0.8, mixFor = 0.7)
##Spruce deciduous
pMortX$spDec <- pMort2(modOut,ageClass,sp=2:3, pureFor = 0.8, mixFor = 0.7)
##deciduous
pMortX$dec <- pMort2(modOut,ageClass,sp=3, pureFor = 0.8, mixFor = 0.7)
nameFor <- c("pine","piSp","spruce","spDec","decid")
names(pMortX) <- nameFor

# yearsX <- 1:length(pMortX$pine$pMort)
period1  <- 1:9
period2 <- 10:17
period3 <- 18:32

barplot(c(mean(pMortX$pine$pMort[period1]),mean(pMortX$piSp$pMort[period1]),
          mean(pMortX$spruce$pMort[period1]),mean(pMortX$spDec$pMort[period1]),
          mean(pMortX$dec$pMort[period1])),ylim=c(0,1), main="ageClass40",
        names = nameFor)

barplot(c(mean(pMortX$pine$pMort[period2]),mean(pMortX$piSp$pMort[period2]),
          mean(pMortX$spruce$pMort[period2]),mean(pMortX$spDec$pMort[period2]),
          mean(pMortX$dec$pMort[period2])),ylim=c(0,1), main="ageClass40",
        names = nameFor)

barplot(c(mean(pMortX$pine$pMort[period3]),mean(pMortX$piSp$pMort[period3]),
          mean(pMortX$spruce$pMort[period3]),mean(pMortX$spDec$pMort[period3]),
          mean(pMortX$dec$pMort[period3])),ylim=c(0,1), main="ageClass40",
        names = nameFor)

#set up age class and period for plotting
period1  <- 1:9
period2 <- 10:17
period3 <- 18:32
ageClass1 <- 31:50
ageClass2 <- 51:70
ageClass3 <- 71:90
ageClass4 <- 111:130
ageClass5 <- 131:150
ageClass6 <- 151:200

#run each ageclass for each pure/mixed stand 
pMortX$pine1 <- pMort2(modOut,ageClass1,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$pine2 <- pMort2(modOut,ageClass2,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$pine3 <- pMort2(modOut,ageClass3,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$pine4 <- pMort2(modOut,ageClass4,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$pine5 <- pMort2(modOut,ageClass5,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$pine6 <- pMort2(modOut,ageClass6,sp=1, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp1 <- pMort2(modOut,ageClass1,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp2 <- pMort2(modOut,ageClass2,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp3 <- pMort2(modOut,ageClass3,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp4 <- pMort2(modOut,ageClass4,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp5 <- pMort2(modOut,ageClass5,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$piSp6 <- pMort2(modOut,ageClass6,sp=1:2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce1 <- pMort2(modOut,ageClass1,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce2 <- pMort2(modOut,ageClass2,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce3 <- pMort2(modOut,ageClass3,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce4 <- pMort2(modOut,ageClass4,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce5 <- pMort2(modOut,ageClass5,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spruce6 <- pMort2(modOut,ageClass6,sp=2, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec1 <- pMort2(modOut,ageClass1,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec2 <- pMort2(modOut,ageClass2,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec3 <- pMort2(modOut,ageClass3,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec4 <- pMort2(modOut,ageClass4,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec5 <- pMort2(modOut,ageClass5,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$spDec6 <- pMort2(modOut,ageClass6,sp=2:3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec1 <- pMort2(modOut,ageClass1,sp=3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec2 <- pMort2(modOut,ageClass2,sp=3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec3 <- pMort2(modOut,ageClass3,sp=3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec4 <- pMort2(modOut,ageClass4,sp=3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec5 <- pMort2(modOut,ageClass5,sp=3, pureFor = 0.8, mixFor = 0.7)
pMortX$dec6 <- pMort2(modOut,ageClass6,sp=3, pureFor = 0.8, mixFor = 0.7)

#dataframes for each period with the means of mortality for each age class
df1 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                 AgePeriod1=rep(c("40","60","80","120","140","160"),each=5),
                 pMortality=c(mean(pMortX$pine1$pMort[period1]),mean(pMortX$piSp1$pMort[period1]),
                              mean(pMortX$spruce1$pMort[period1]),mean(pMortX$spDec1$pMort[period1]),
                              mean(pMortX$dec1$pMort[period1]),mean(pMortX$pine2$pMort[period1]),mean(pMortX$piSp2$pMort[period1]),
                              mean(pMortX$spruce2$pMort[period1]),mean(pMortX$spDec2$pMort[period1]),
                              mean(pMortX$dec2$pMort[period1]),mean(pMortX$pine3$pMort[period1]),mean(pMortX$piSp3$pMort[period1]),
                              mean(pMortX$spruce3$pMort[period1]),mean(pMortX$spDec3$pMort[period1]),
                              mean(pMortX$dec3$pMort[period1]),mean(pMortX$pine4$pMort[period1]),mean(pMortX$piSp4$pMort[period1]),
                              mean(pMortX$spruce4$pMort[period1]),mean(pMortX$spDec4$pMort[period1]),
                              mean(pMortX$dec4$pMort[period1]),mean(pMortX$pine5$pMort[period1]),mean(pMortX$piSp5$pMort[period1]),
                              mean(pMortX$spruce5$pMort[period1]),mean(pMortX$spDec5$pMort[period1]),
                              mean(pMortX$dec5$pMort[period1]),mean(pMortX$pine6$pMort[period1]),mean(pMortX$piSp6$pMort[period1]),
                              mean(pMortX$spruce6$pMort[period1]),mean(pMortX$spDec6$pMort[period1]),
                              mean(pMortX$dec6$pMort[period1])))

df2 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                  AgePeriod2=rep(c("40","60","80","120","140","160"),each=5),
                  pMortality=c(mean(pMortX$pine1$pMort[period2]),mean(pMortX$piSp1$pMort[period2]),
                               mean(pMortX$spruce1$pMort[period2]),mean(pMortX$spDec1$pMort[period2]),
                               mean(pMortX$dec1$pMort[period2]),mean(pMortX$pine2$pMort[period2]),mean(pMortX$piSp2$pMort[period2]),
                               mean(pMortX$spruce2$pMort[period2]),mean(pMortX$spDec2$pMort[period2]),
                               mean(pMortX$dec2$pMort[period2]),mean(pMortX$pine3$pMort[period2]),mean(pMortX$piSp3$pMort[period2]),
                               mean(pMortX$spruce3$pMort[period2]),mean(pMortX$spDec3$pMort[period2]),
                               mean(pMortX$dec3$pMort[period2]),mean(pMortX$pine4$pMort[period2]),mean(pMortX$piSp4$pMort[period2]),
                               mean(pMortX$spruce4$pMort[period2]),mean(pMortX$spDec4$pMort[period2]),
                               mean(pMortX$dec4$pMort[period2]),mean(pMortX$pine5$pMort[period2]),mean(pMortX$piSp5$pMort[period2]),
                               mean(pMortX$spruce5$pMort[period2]),mean(pMortX$spDec5$pMort[period2]),
                               mean(pMortX$dec5$pMort[period2]),mean(pMortX$pine6$pMort[period2]),mean(pMortX$piSp6$pMort[period2]),
                               mean(pMortX$spruce6$pMort[period2]),mean(pMortX$spDec6$pMort[period2]),
                               mean(pMortX$dec6$pMort[period2])))

df3 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                  AgePeriod3=rep(c("40","60","80","120","140","160"),each=5),
                  pMortality=c(mean(pMortX$pine1$pMort[period3]),mean(pMortX$piSp1$pMort[period3]),
                               mean(pMortX$spruce1$pMort[period3]),mean(pMortX$spDec1$pMort[period3]),
                               mean(pMortX$dec1$pMort[period3]),mean(pMortX$pine2$pMort[period3]),mean(pMortX$piSp2$pMort[period3]),
                               mean(pMortX$spruce2$pMort[period3]),mean(pMortX$spDec2$pMort[period3]),
                               mean(pMortX$dec2$pMort[period3]),mean(pMortX$pine3$pMort[period3]),mean(pMortX$piSp3$pMort[period3]),
                               mean(pMortX$spruce3$pMort[period3]),mean(pMortX$spDec3$pMort[period3]),
                               mean(pMortX$dec3$pMort[period3]),mean(pMortX$pine4$pMort[period3]),mean(pMortX$piSp4$pMort[period3]),
                               mean(pMortX$spruce4$pMort[period3]),mean(pMortX$spDec4$pMort[period3]),
                               mean(pMortX$dec4$pMort[period3]),mean(pMortX$pine5$pMort[period3]),mean(pMortX$piSp5$pMort[period3]),
                               mean(pMortX$spruce5$pMort[period3]),mean(pMortX$spDec5$pMort[period3]),
                               mean(pMortX$dec5$pMort[period3]),mean(pMortX$pine6$pMort[period3]),mean(pMortX$piSp6$pMort[period3]),
                               mean(pMortX$spruce6$pMort[period3]),mean(pMortX$spDec6$pMort[period3]),
                               mean(pMortX$dec6$pMort[period3])))

#organize axis labels
df1$AgePeriod1 <- factor(df1$AgePeriod1,levels=c("40","60","80","120","140","160"))
df1$Species <- factor(df1$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))
df2$AgePeriod2 <- factor(df2$AgePeriod2,levels=c("40","60","80","120","140","160"))
df2$Species <- factor(df2$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))
df3$AgePeriod3 <- factor(df3$AgePeriod3,levels=c("40","60","80","120","140","160"))
df3$Species <- factor(df3$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))

#plot the bar charts
ggplot(data=df1, aes(x=AgePeriod1, y=pMortality, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(data=df2, aes(x=AgePeriod2, y=pMortality, fill=Species)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=df3, aes(x=AgePeriod3, y=pMortality, fill=Species)) +
  geom_bar(stat="identity", position=position_dodge())

###function to calculate the mortality probability along some variable classes
# Arguments: 
# modOut = output array from a PREBAS multisite runs
# rangeYear = number of years  for which to calculate pM
# minX = minimum value for the variable class
# maxX = maximum value for the variable class
# stepX = class step
# varX = variable ID of PREBAS output (see varNames)
# funX = function to use to aggregate the data (mean or sum) mean for age and DBH, sum for BA, stemNumber
pMortVarX <- function(modOut,minX,maxX,stepX,varX,funX,rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  seqX <- seq(minX,maxX,by=stepX)
  nClass <- length(seqX)+1
  pMortX <- nData <- matrix(0.,length(endX),nClass)
  for(i in 1:length(startX)){
    varXs<-apply(modOut[,startX[i]:endX[i],varX,,1],1:2,funX)
    varXs <- rowMeans(varXs)
    for(ij in 1:nClass){
      if(ij==1) cX <- which(varXs <= seqX[ij])
      if(ij>1 & ij<nClass) cX <- which(varXs <= seqX[ij] & varXs > seqX[ij-1])
      if(ij==nClass) cX <- which(varXs > seqX[ij])
    # outX <- modOut[cX,,,,]
      if(length(cX)>0.){
        mortX <- data.table(which(modOut[cX,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nData[i,ij] <- length(cX)
        pMortX[i,ij] <- nMort/length(cX)
      }
    }
  }
  return(list(pMort=pMortX,nData=nData,classes=seqX))
}


Dmort <- pMortVarX(modOut,minX=10,maxX=45,stepX=5,varX=13,funX = "mean")
plot(rowMeans(Dmort$pMort[period1,]))
plot(rowMeans(Dmort$pMort[period2,]))
plot(rowMeans(Dmort$pMort[period3,]))

BAmort <- pMortVarX(modOut,minX=10,maxX=45,stepX=5,varX=13,funX = "sum")
plot(rowMeans(BAmort$pMort[period1,]))
plot(rowMeans(BAmort$pMort[period2,]))
plot(rowMeans(BAmort$pMort[period3,]))

Nmort <- pMortVarX(modOut,minX=200,maxX=1400,stepX=200,varX=17,funX = "sum")
plot(rowMeans(Nmort$pMort[period1,]))
plot(rowMeans(Nmort$pMort[period2,]))
plot(rowMeans(Nmort$pMort[period3,]))





###function to calculate the mortality probability for species proportion
# Arguments: 
# modOut = output array from a PREBAS multisite runs
# rangeYear = number of years  for which to calculate pM
# minX = minimum species cover
# maxX = maximum species cover
# stepX = class step
pMortSpecies <- function(modOut,minX=0.1,maxX=0.9,stepX=0.1,rangeYear=5){
  endX <- rangeYear:dim(modOut)[2]
  startX <- endX-(rangeYear-1)
  seqX <- seq(minX,maxX,by=stepX)
  nClass <- length(seqX)+1
  pMortXpine <- nDataPine <- 
    pMortXspruce <- nDataSpruce <- 
    pMortXbirch <- nDataBirch <- matrix(0.,length(endX),nClass)
  totBA <- apply(modOut[,,13,,1],1:2,sum)
  pBApine <- modOut[,,13,1,1]/totBA
  pBAspruce <- modOut[,,13,2,1]/totBA
  pBAbirch <- modOut[,,13,3,1]/totBA
  for(i in 1:length(startX)){
    subPine <-rowMeans(pBApine[,startX[i]:endX[i]],na.rm=T)
    subSpruce <-rowMeans(pBAspruce[,startX[i]:endX[i]],na.rm=T)
    subBirch <-rowMeans(pBAbirch[,startX[i]:endX[i]],na.rm=T)
    for(ij in 1:nClass){
      if(ij==1){
        cXpine <- which(subPine <= seqX[ij])
        cXspruce <- which(subSpruce <= seqX[ij])
        cXbirch <- which(subBirch <= seqX[ij])
      } 
      if(ij>1 & ij<nClass){
        cXpine <- which(subPine <= seqX[ij] & subPine > seqX[ij-1])
        cXspruce <- which(subSpruce <= seqX[ij] & subSpruce > seqX[ij-1])
        cXbirch <- which(subBirch <= seqX[ij] & subBirch > seqX[ij-1])
      } 
      if(ij==nClass){
        cXpine <- which(subPine > seqX[ij])
        cXspruce <- which(subSpruce > seqX[ij])
        cXbirch <- which(subBirch > seqX[ij])
      } 
      # outX <- modOut[cX,,,,]
      if(length(cXpine)>0.){
        mortX <- data.table(which(modOut[cXpine,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataPine[i,ij] <- length(cXpine)
        pMortXpine[i,ij] <- nMort/length(cXpine)
      }
      if(length(cXspruce)>0.){
        mortX <- data.table(which(modOut[cXspruce,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataSpruce[i,ij] <- length(cXspruce)
        pMortXspruce[i,ij] <- nMort/length(cXspruce)
      }
      if(length(cXbirch)>0.){
        mortX <- data.table(which(modOut[cXbirch,startX[i]:endX[i],42,,1]>0,arr.ind=T))
        nMort <- length(unique(mortX$site))
        nDataBirch[i,ij] <- length(cXbirch)
        pMortXbirch[i,ij] <- nMort/length(cXbirch)
      }
    }
  }
  return(list(pMortPine=pMortXpine,nDataPine=nDataPine,
              pMortSpruce=pMortXspruce,nDataSpruce=nDataSpruce,
              pMortBirch=pMortXbirch,nDataBirch=nDataBirch))
}

pMortBYspec <- pMortSpecies(modOut,minX=0.1,maxX=0.9,stepX=0.1,rangeYear=5)
plot(rowMeans(pMortBYspec$pMortPine[period1,]))
plot(rowMeans(pMortBYspec$pMortPine[period2,]))
plot(rowMeans(pMortBYspec$pMortPine[period3,]))
plot(rowMeans(pMortBYspec$pMortSpruce[period1,]))
plot(rowMeans(pMortBYspec$pMortSpruce[period2,]))
plot(rowMeans(pMortBYspec$pMortSpruce[period3,]))
plot(rowMeans(pMortBYspec$pMortBirch[period1,]))
plot(rowMeans(pMortBYspec$pMortBirch[period2,]))
plot(rowMeans(pMortBYspec$pMortBirch[period3,]))
