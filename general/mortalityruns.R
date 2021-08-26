library(data.table)
library(Rprebasso)

r_no <- 7
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

load(paste0("~/Documents/2021 Summer/sample_5_reg",r_no,"_Base_0_CurrClim.rdata"))

df11 <- data.frame(Species=rep(c("Pine(Mod)","Pine-spruce(Mod)","Spruce(Mod)","Spruce-deciduous(Mod)","Deciduous(Mod)"),6), 
                   AgePeriod=rep(c("40","60","80","120","140","160"),each=5),
                   pMortality=c(0.275,0.292,0.311,0.360,0.411,0.275,0.301,0.300,
                                0.406,0.487,0.276,0.311,0.349,0.455,0.563,0.219,
                                0.268,0.321,0.477,0.638,0.221,0.276,0.340,0.524,
                                0.706,0.221,0.286,0.360,0.575,0.765),
                   NSites=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

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

sp=1
pureFor <- 0.8
mixFor <- 0.8

pMortX <- list()
period1  <- 1:9
period2 <- 10:17
period3 <- 18:32
ageClass1 <- 31:50
ageClass2 <- 51:70
ageClass3 <- 71:90
ageClass4 <- 111:130
ageClass5 <- 131:150
ageClass6 <- 151:200
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

df1 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                  AgePeriod=rep(c("40","60","80","120","140","160"),each=5),
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
                               mean(pMortX$dec6$pMort[period1])),
                  NSites=c(mean((pMortX$pine1$nSites[period1])),mean((pMortX$piSp1$nSites[period1])),
             mean((pMortX$spruce1$nSites[period1])),mean((pMortX$spDec1$nSites[period1])),
             mean((pMortX$dec1$nSites[period1])),mean((pMortX$pine2$nSites[period1])),mean((pMortX$piSp2$nSites[period1])),
             mean((pMortX$spruce2$nSites[period1])),mean((pMortX$spDec2$nSites[period1])),
             mean((pMortX$dec2$nSites[period1])),mean((pMortX$pine3$nSites[period1])),mean((pMortX$piSp3$nSites[period1])),
             mean((pMortX$spruce3$nSites[period1])),mean((pMortX$spDec3$nSites[period1])),
             mean((pMortX$dec3$nSites[period1])),mean((pMortX$pine4$nSites[period1])),mean((pMortX$piSp4$nSites[period1])),
             mean((pMortX$spruce4$nSites[period1])),mean((pMortX$spDec4$nSites[period1])),
             mean((pMortX$dec4$nSites[period1])),mean((pMortX$pine5$nSites[period1])),mean((pMortX$piSp5$nSites[period1])),
             mean((pMortX$spruce5$nSites[period1])),mean((pMortX$spDec5$nSites[period1])),
             mean((pMortX$dec5$nSites[period1])),mean((pMortX$pine6$nSites[period1])),mean((pMortX$piSp6$nSites[period1])),
             mean((pMortX$spruce6$nSites[period1])),mean((pMortX$spDec6$nSites[period1])),
             mean((pMortX$dec6$nSites[period1]))))

df2 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                  AgePeriod=rep(c("40","60","80","120","140","160"),each=5),
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
                               mean(pMortX$dec6$pMort[period2])),
                  NSites=c(mean((pMortX$pine1$nSites[period2])),mean((pMortX$piSp1$nSites[period2])),
             mean((pMortX$spruce1$nSites[period2])),mean((pMortX$spDec1$nSites[period2])),
             mean((pMortX$dec1$nSites[period2])),mean((pMortX$pine2$nSites[period2])),mean((pMortX$piSp2$nSites[period2])),
             mean((pMortX$spruce2$nSites[period2])),mean((pMortX$spDec2$nSites[period2])),
             mean((pMortX$dec2$nSites[period2])),mean((pMortX$pine3$nSites[period2])),mean((pMortX$piSp3$nSites[period2])),
             mean((pMortX$spruce3$nSites[period2])),mean((pMortX$spDec3$nSites[period2])),
             mean((pMortX$dec3$nSites[period2])),mean((pMortX$pine4$nSites[period2])),mean((pMortX$piSp4$nSites[period2])),
             mean((pMortX$spruce4$nSites[period2])),mean((pMortX$spDec4$nSites[period2])),
             mean((pMortX$dec4$nSites[period2])),mean((pMortX$pine5$nSites[period2])),mean((pMortX$piSp5$nSites[period2])),
             mean((pMortX$spruce5$nSites[period2])),mean((pMortX$spDec5$nSites[period2])),
             mean((pMortX$dec5$nSites[period2])),mean((pMortX$pine6$nSites[period2])),mean((pMortX$piSp6$nSites[period2])),
             mean((pMortX$spruce6$nSites[period2])),mean((pMortX$spDec6$nSites[period2])),
             mean((pMortX$dec6$nSites[period2]))))

df3 <- data.frame(Species=rep(c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"),6), 
                  AgePeriod=rep(c("40","60","80","120","140","160"),each=5),
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
                               mean(pMortX$dec6$pMort[period3])),
                  NSites=c(mean((pMortX$pine1$nSites[period3])),mean((pMortX$piSp1$nSites[period3])),
             mean((pMortX$spruce1$nSites[period3])),mean((pMortX$spDec1$nSites[period3])),
             mean((pMortX$dec1$nSites[period3])),mean((pMortX$pine2$nSites[period3])),mean((pMortX$piSp2$nSites[period3])),
             mean((pMortX$spruce2$nSites[period3])),mean((pMortX$spDec2$nSites[period3])),
             mean((pMortX$dec2$nSites[period3])),mean((pMortX$pine3$nSites[period3])),mean((pMortX$piSp3$nSites[period3])),
             mean((pMortX$spruce3$nSites[period3])),mean((pMortX$spDec3$nSites[period3])),
             mean((pMortX$dec3$nSites[period3])),mean((pMortX$pine4$nSites[period3])),mean((pMortX$piSp4$nSites[period3])),
             mean((pMortX$spruce4$nSites[period3])),mean((pMortX$spDec4$nSites[period3])),
             mean((pMortX$dec4$nSites[period3])),mean((pMortX$pine5$nSites[period3])),mean((pMortX$piSp5$nSites[period3])),
             mean((pMortX$spruce5$nSites[period3])),mean((pMortX$spDec5$nSites[period3])),
             mean((pMortX$dec5$nSites[period3])),mean((pMortX$pine6$nSites[period3])),mean((pMortX$piSp6$nSites[period3])),
             mean((pMortX$spruce6$nSites[period3])),mean((pMortX$spDec6$nSites[period3])),
             mean((pMortX$dec6$nSites[period3]))))

df12 <- rbind(df1, df11)
df13 <- rbind(df2, df11)
df14 <- rbind(df3, df11)
df12$AgePeriod <- factor(df12$AgePeriod,levels=c("40","60","80","120","140","160"))
df12$Species <- factor(df12$Species,levels=c("Pine","Pine(Mod)","Pine-spruce","Pine-spruce(Mod)","Spruce","Spruce(Mod)","Spruce-deciduous","Spruce-deciduous(Mod)","Deciduous","Deciduous(Mod)"))
df13$AgePeriod <- factor(df13$AgePeriod,levels=c("40","60","80","120","140","160"))
df13$Species <- factor(df13$Species,levels=c("Pine","Pine(Mod)","Pine-spruce","Pine-spruce(Mod)","Spruce","Spruce(Mod)","Spruce-deciduous","Spruce-deciduous(Mod)","Deciduous","Deciduous(Mod)"))
df14$AgePeriod <- factor(df14$AgePeriod,levels=c("40","60","80","120","140","160"))
df14$Species <- factor(df14$Species,levels=c("Pine","Pine(Mod)","Pine-spruce","Pine-spruce(Mod)","Spruce","Spruce(Mod)","Spruce-deciduous","Spruce-deciduous(Mod)","Deciduous","Deciduous(Mod)"))

coeff1 <- max(df12$NSites, na.rm=TRUE)
p1 <- ggplot(df12, aes(x=AgePeriod)) + 
  geom_bar(aes(y=pMortality, fill=Species), stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                               "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  geom_line(aes(y=NSites / coeff1, group=Species, colour=Species), stat="identity") +
  scale_colour_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                               "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff1, name="NSites")) +
  theme(legend.position = "none") +
  xlab("Age Period 1") 

coeff2 <- max(df13$NSites, na.rm=TRUE)
p2 <- ggplot(df13, aes(x=AgePeriod)) + 
  geom_bar(aes(y=pMortality, fill=Species), stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                               "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  geom_line(aes(y=NSites / coeff2, group=Species, colour=Species), stat="identity") +
  scale_colour_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                                 "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff2, name="NSites")) +
  theme(legend.position = "none") +
  xlab("Age Period 2")

coeff3 <- max(df14$NSites, na.rm=TRUE)
p3 <- ggplot(df14, aes(x=AgePeriod)) + 
  geom_bar(aes(y=pMortality, fill=Species), stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                               "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  geom_line(aes(y=NSites / coeff3, group=Species, colour=Species), stat="identity") +
  scale_colour_manual(values = c("Pine"="skyblue2","Pine-spruce"="palegreen2","Spruce"="orchid2","Spruce-deciduous"="tomato2","Deciduous"="gold2",
                                 "Pine(Mod)"="grey1","Pine-spruce(Mod)"="grey15","Spruce(Mod)"="grey30","Spruce-deciduous(Mod)"="grey45","Deciduous(Mod)"="grey60")) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff3, name="NSites")) +
  xlab("Age Period 3")

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

Amort <- pMortVarX(modOut,minX=40,maxX=200,stepX=20,varX=7,funX = "mean")

Nmort <- pMortVarX(modOut,minX=200,maxX=1400,stepX=200,varX=17,funX = "sum")

BAmort <- pMortVarX(modOut,minX=10,maxX=45,stepX=5,varX=13,funX = "sum")

df15 <- data.frame(Period=rep(c("Mod"),10),
                   Age=c("<40","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199",">200"),
                   pMortality=c(0.218,0.259,0.271,0.274,0.274,0.248,0.254,0.250,0.265,0.284),
                   NSites=c(2597,3703,3101,2874,2696,2528,1925,938,345,158))

df4 <- data.frame(Period=rep(c("1","2","3"),each=10),
                  Age=rep(c("<40","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199",">200"),3),
                  pMortality=c(colMeans(Amort$pMort[period1,]),colMeans(Amort$pMort[period2,]),colMeans(Amort$pMort[period3,])),
                  NSites=c(colMeans(Amort$nData[period1,]),colMeans(Amort$nData[period2,]),colMeans(Amort$nData[period3,])))

df16 <- data.frame(Period=rep(c("Mod"),8),
                   LayerDensity=c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),
                   pMortality=c(0.068,0.130,0.216,0.290,0.349,0.401,0.446,0.531),
                   NSites=c(1533,4795,5427,4559,2303,1173,665,491))

df5 <- data.frame(Period=rep(c("1","2","3"),each=8),
                  LayerDensity=rep(c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),3),
                  pMortality=c(colMeans(Nmort$pMort[period1,]),colMeans(Nmort$pMort[period2,]),colMeans(Nmort$pMort[period3,])),
                  NSites=c(colMeans(Nmort$nData[period1,]),colMeans(Nmort$nData[period2,]),colMeans(Nmort$nData[period3,])))

df17 <- data.frame(Period=rep(c("Mod"),9),
                   BasalArea=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-44",">45"),
                   pMortality=c(0.124,0.189,0.234,0.278,0.324,0.368,0.406,0.449,NA),
                   NSites=c(4057,3819,4019,3372,2337,1396,779,380,NA))

df6 <- data.frame(Period=rep(c("1","2","3"),each=9),
                  BasalArea=rep(c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-44",">45"),3),
                  pMortality=c(colMeans(BAmort$pMort[period1,]),colMeans(BAmort$pMort[period2,]),colMeans(BAmort$pMort[period3,])),
                  NSites=c(colMeans(BAmort$nData[period1,]),colMeans(BAmort$nData[period2,]),colMeans(BAmort$nData[period3,])))

df18 <- rbind(df15, df4)
df19 <- rbind(df16, df5)
df20 <- rbind(df17, df6)
df18$Age <- factor(df18$Age,levels=c("<40","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199",">200"))
df19$LayerDensity <- factor(df19$LayerDensity,levels=c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"))
df20$BasalArea <- factor(df20$BasalArea,levels=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-44",">45"))

coeff4 <- ((max(df18$NSites, na.rm=TRUE))/0.6)
p4 <- ggplot(df18, aes(x=Age)) +
  geom_bar(aes(y=pMortality, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff4, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff4, name="NSites")) +
  theme(legend.position = "none") 

coeff5 <- ((max(df19$NSites, na.rm=TRUE))/0.6)
p5 <- ggplot(df19, aes(x=LayerDensity)) +
  geom_bar(aes(y=pMortality, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff5, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff5, name="NSites")) +
  theme(legend.position = "none") 

coeff6 <- ((max(df20$NSites, na.rm=TRUE))/0.6)
p6 <- ggplot(df20, aes(x=BasalArea)) +
  geom_bar(aes(y=pMortality, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff6, group=Period, colour=Period), stat="identity", position=position_dodge()) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff6, name="NSites")) 

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

df21 <- data.frame(Period=rep(c("Mod"),10),
                   PineProportion=c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),
                   pMortality=c(0.329,0.323,0.297,0.284,0.263,0.246,0.231,0.209,0.187,0.154),
                   NSites=c(7131,891,754,737,771,839,891,1200,1439,5228))

df7 <- data.frame(Period=rep(c("1","2","3"),each=10),
                  PineProportion=rep(c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),3),
                  pMortality=c(colMeans(pMortBYspec$pMortPine[period1,]),colMeans(pMortBYspec$pMortPine[period2,]),colMeans(pMortBYspec$pMortPine[period3,])),
                  NSites=c(colMeans(pMortBYspec$nDataPine[period1,]),colMeans(pMortBYspec$nDataPine[period2,]),colMeans(pMortBYspec$nDataPine[period3,])))

df22 <- data.frame(Period=rep(c("Mod"),10),
                   SpruceProportion=c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),
                   pMortality=c(0.215,0.243,0.253,0.274,0.288,0.294,0.315,0.309,0.308,0.294),
                   NSites=c(8575,1439,1213,987,928,928,904,987,1110,2817))

df8 <- data.frame(Period=rep(c("1","2","3"),each=10),
                  SpruceProportion=rep(c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),3),
                  pMortality=c(colMeans(pMortBYspec$pMortSpruce[period1,]),colMeans(pMortBYspec$pMortSpruce[period2,]),colMeans(pMortBYspec$pMortSpruce[period3,])),
                  NSites=c(colMeans(pMortBYspec$nDataSpruce[period1,]),colMeans(pMortBYspec$nDataSpruce[period2,]),colMeans(pMortBYspec$nDataSpruce[period3,])))

df23 <- data.frame(Period=rep(c("Mod"),10),
                   BirchProportion=c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),
                   pMortality=c(0.217,0.264,0.293,0.311,0.315,0.336,0.341,0.362,0.361,0.371),
                   NSites=c(12528,2264,1261,860,573,430,258,229,172,1376))

df9 <- data.frame(Period=rep(c("1","2","3"),each=10),
                  BirchProportion=rep(c("<0.1","0.1-0.19","0.2-0.29","0.3-0.39","0.4-0.49","0.5-0.59","0.6-0.69","0.7-0.79","0.8-0.89","0.9-1.0"),3),
                  pMortality=c(colMeans(pMortBYspec$pMortBirch[period1,]),colMeans(pMortBYspec$pMortBirch[period2,]),colMeans(pMortBYspec$pMortBirch[period3,])),
                  NSites=c(colMeans(pMortBYspec$nDataBirch[period1,]),colMeans(pMortBYspec$nDataBirch[period2,]),colMeans(pMortBYspec$nDataBirch[period3,])))

df24 <- rbind(df7, df21)
df25 <- rbind(df8, df22)
df26 <- rbind(df9, df23)

coeff7 <- max(df24$NSites, na.rm=TRUE)
p7 <- ggplot(df24, aes(x=PineProportion)) + 
  geom_bar(aes(y=pMortality, fill=Period),stat="identity", position=position_dodge()) +
  geom_line(aes(y=NSites / coeff7, group=Period, colour=Period), stat="identity", position=position_dodge()) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff7, name="NSites")) +
  theme(legend.position = "none")

coeff8 <- max(df25$NSites, na.rm=TRUE)
p8 <- ggplot(df25, aes(x=SpruceProportion)) + 
  geom_bar(aes(y=pMortality, fill=Period),stat="identity", position=position_dodge()) +
  geom_line(aes(y=NSites / coeff8, group=Period, colour=Period), stat="identity", position=position_dodge()) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff8, name="NSites")) +
  theme(legend.position = "none")

coeff9 <- max(df26$NSites, na.rm=TRUE)
p9 <- ggplot(df26, aes(x=BirchProportion)) + 
  geom_bar(aes(y=pMortality, fill=Period),stat="identity", position=position_dodge()) +
  geom_line(aes(y=NSites / coeff9, group=Period, colour=Period), stat="identity", position=position_dodge()) +
  scale_y_continuous(name="pMortality",sec.axis =sec_axis(~.*coeff9, name="NSites")) +
  theme(legend.position = "none")

figure1 <- ggarrange(p1,p2,p3, labels = c("A","B","C"),
                     hjust = -0.2, vjust = -0.25,
                     common.legend = TRUE, legend = "right",
                     nrow = 1, ncol=3)

figure2 <- ggarrange(p4,p5,p6, labels = c("D","E","F"), 
                    hjust = -0.2, vjust = -0.25,
                    common.legend = TRUE, legend = "right",
                    nrow = 1, ncol=3)

figure3 <- ggarrange(p7,p8,p9, labels = c("G","H","I"),
                     hjust = -0.2, vjust = -0.25,
                     common.legend = TRUE, legend = "right",
                     nrow = 1, ncol=3)

figuref <- ggarrange(figure1,figure2,figure3,
                     common.legend = FALSE, legend = "right",
                     align = "h",
                     nrow=3, ncol=1)

annotate_figure(figuref, top=text_grob("Pohjois-Savo(5), Base(15), Comp(0)",size=14))
