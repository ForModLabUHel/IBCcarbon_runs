# analyse mortality 
library(data.table)
library(Rprebasso)

load("C:/Users/checcomi/Documents/research/IBC-carbon/sampleRuns/sample_12_reg1_Base_CurrClim.rdata")




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

NSites1 <- c(mean((pMortX$pine1$nSites[period1])),mean((pMortX$piSp1$nSites[period1])),
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
             mean((pMortX$dec6$nSites[period1])))

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

NSites2 <- c(mean((pMortX$pine1$nSites[period2])),mean((pMortX$piSp1$nSites[period2])),
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
             mean((pMortX$dec6$nSites[period2])))

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

NSites3 <- c(mean((pMortX$pine1$nSites[period3])),mean((pMortX$piSp1$nSites[period3])),
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
             mean((pMortX$dec6$nSites[period3])))

#organize axis labels
df1$AgePeriod1 <- factor(df1$AgePeriod1,levels=c("40","60","80","120","140","160"))
df1$Species <- factor(df1$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))
df2$AgePeriod2 <- factor(df2$AgePeriod2,levels=c("40","60","80","120","140","160"))
df2$Species <- factor(df2$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))
df3$AgePeriod3 <- factor(df3$AgePeriod3,levels=c("40","60","80","120","140","160"))
df3$Species <- factor(df3$Species,levels=c("Pine","Pine-spruce","Spruce","Spruce-deciduous","Deciduous"))

#plot the bar charts
p1 <- ggplot(data=df1, aes(x=AgePeriod1, y=pMortality, fill=Species, label=sprintf("%0.00f", round(NSites1, digits = 0)))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(size=1.5, position=position_dodge(width=1), vjust=-0.25) +
  ylim(0, 1)
p1

p2 <- ggplot(data=df2, aes(x=AgePeriod2, y=pMortality, fill=Species, label=sprintf("%0.00f", round(NSites2, digits = 0)))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(size=1.5, position=position_dodge(width=1), vjust=-0.25) +
  ylim(0, 1)
p2

p3 <- ggplot(data=df3, aes(x=AgePeriod3, y=pMortality, fill=Species, label=sprintf("%0.00f", round(NSites3, digits = 0)))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(size=1.5, position=position_dodge(width=1), vjust=-0.25) +
  ylim(0, 1)
p3

figure <- ggarrange(p1, p2, p3, labels = c("A","B","C"), 
                    common.legend = TRUE, legend = "bottom",
                    ncol = 2, nrow = 2)

annotate_figure(figure, top="No Harvest Scenerio")


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


period1=1:9
period2=10:17
period3=18:28

# <<<<<<< HEAD
# 
# baMortAge <- baMortVarX(modOut,minX=20,maxX=160,stepX=20,varX=7,funX = "max")
# barplot(rowMeans(baMortAge$baMort[period1,]))
# barplot(rowMeans(baMortAge$baMort[period2,]))
# barplot(rowMeans(baMortAge$baMort[period3,]))
# 
# baMortD <- baMortVarX(modOut,minX=10,maxX=25,stepX=5,varX=12,funX = "mean")
# barplot(rowMeans(baMortAge$baMort[period1,]))
# barplot(rowMeans(baMortAge$baMort[period2,]))
# barplot(rowMeans(baMortAge$baMort[period3,]))
# 
# baMortBA <- baMortVarX(modOut,minX=10,maxX=30,stepX=5,varX=13,funX = "sum")
# propBA <- baMortBA$baMort/baMortBA$baTot  
# barplot(rowMeans(propBA[period1,]))####fig6 A
# barplot(rowMeans(baMortBA$baMort[period1,]))####fig6 B

# ###fig 6 C and D
# baMortBA <- baMortVarX(modOut,minX=10,maxX=30,stepX=5,varX=17,funX = "sum")
# propBA <- baMortBA$baMort/baMortBA$baTot  
# barplot(rowMeans(propBA[period3,]))####fig6 C
# barplot(rowMeans(baMortBA$baMort[period3,]))####fig6 D
# =======
##fig 5A
baMortAge <- baMortVarX(modOut,minX=20,maxX=240,stepX=20,varX=7,funX = "max")

df1 <- data.frame(Period=rep(c(1,2,3),each=13),
                  Age=rep(c("<20","20-39","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199","200-219","220-239",">240"),3),
                  BasalArea=c(colMeans(baMortAge$baMort[period1,]),colMeans(baMortAge$baMort[period2,]),colMeans(baMortAge$baMort[period3,])),
                  NSites=c(colMeans(baMortAge$nData[period1,]),colMeans(baMortAge$nData[period2,]),colMeans(baMortAge$nData[period3,])))

df11 <- data.frame(Period=rep(c("Mod"),13),
                   Age=c("<20","20-39","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199","200-219","220-239",">240"),
                   BasalArea=c(1.273,1.04,1.36,1.61,1.58,1.55,1.52,1.55,1.49,1.43,1.66,1.49,1.83),
                   NSites=c(227,763,1659,1872,1781,1650,1281,1022,454,168,63,9,18))

df5A <- rbind(df1, df11)
df5A$Age <- factor(df5A$Age,levels=c("<20","20-39","40-59","60-79","80-99","100-119","120-139","140-159","160-179","180-199","200-219","220-239",">240"))
coeff5A <- ((max(df5A$NSites, na.rm=TRUE))/2)
p5A <- ggplot(df5A, aes(x=Age)) +
  geom_bar(aes(y=BasalArea, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff5A, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="Basal area of dead trees",sec.axis =sec_axis(~.*coeff5A, name="NSites")) +
  theme(legend.position = "none") 

##fig 6A
baMortBA <- baMortVarX(modOut,minX=10,maxX=45,stepX=5,varX=13,funX = "sum")
propBA1 <- colMeans(baMortBA$baMort[period1,]/baMortBA$baTot[period1,])
propBA2 <- colMeans(baMortBA$baMort[period2,]/baMortBA$baTot[period2,])
propBA3 <- colMeans(baMortBA$baMort[period3,]/baMortBA$baTot[period3,])

df2 <- data.frame(Period=rep(c(1,2,3),each=9),
                  BasalArea=rep(c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"),3),
                  ProbBA=c(propBA1,propBA2,propBA3),
                  NSites=c(colMeans(baMortBA$nData[period1,]),colMeans(baMortBA$nData[period2,]),colMeans(baMortBA$nData[period3,])))

df21 <- data.frame(Period=rep(c("Mod"),9),
                   BasalArea=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"),
                   ProbBA=c(0.154,0.0897,0.0694,0.05958,0.05607,0.05327,0.053972,0.05607,NA),
                   NSites=c(564,944,1463,1708,1753,1361,1046,702,NA))

df6A <- rbind(df2, df21)
df6A$BasalArea <- factor(df6A$BasalArea,levels=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"))
coeff6A <- ((max(df6A$NSites, na.rm=TRUE))/0.2)
p6A <- ggplot(df6A, aes(x=BasalArea)) +
  geom_bar(aes(y=ProbBA, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff6A, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="Proportion of dead basal area",sec.axis =sec_axis(~.*coeff6A, name="NSites")) +
  theme(legend.position = "none") 

##fig 6B
df3 <- data.frame(Period=rep(c(1,2,3),each=9),
                  BasalArea=rep(c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"),3),
                  BaMort=c(colMeans(baMortBA$baMort[period1,]),colMeans(baMortBA$baMort[period2,]),colMeans(baMortBA$baMort[period3,])),
                  NSites=c(colMeans(baMortBA$nData[period1,]),colMeans(baMortBA$nData[period2,]),colMeans(baMortBA$nData[period3,])))

df31 <- data.frame(Period=rep(c("Mod"),9),
                   BasalArea=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"),
                   BaMort=c(0.966,1.076,1.185,1.317,1.507,1.705,1.976,2.334,NA),
                   NSites=c(565,630,690,771,882,998,1157,1367,NA))

df6B <- rbind(df3, df31)
df6B$BasalArea <- factor(df6B$BasalArea,levels=c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40-45",">45"))
coeff6B <- ((max(df6B$NSites, na.rm=TRUE))/4.5)
p6B <- ggplot(df6B, aes(x=BasalArea)) +
  geom_bar(aes(y=BaMort, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff6B, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="Basal area of dead trees",sec.axis =sec_axis(~.*coeff6B, name="NSites")) +
  theme(legend.position = "none") 

##fig 6C
baNmort <- baMortVarX(modOut,minX=200,maxX=1400,stepX=200,varX=17,funX = "sum")
propNBA1 <- colMeans(baNmort$baMort[period1,]/baNmort$baTot[period1,])
propNBA2 <- colMeans(baNmort$baMort[period2,]/baNmort$baTot[period2,])
propNBA3 <- colMeans(baNmort$baMort[period3,]/baNmort$baTot[period3,])

df4 <- data.frame(Period=rep(c("1","2","3"),each=8),
                  StemNumber=rep(c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),3),
                  BaMort=c(propNBA1,propNBA2,propNBA3),
                  NSites=c(colMeans(baNmort$nData[period1,]),colMeans(baNmort$nData[period2,]),colMeans(baNmort$nData[period3,])))

df41 <- data.frame(Period=rep(c("Mod"),8),
                   StemNumber=rep(c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),3),
                   BaMort=c(0.2401,0.129,0.0836,0.0639,0.0541,0.0484,0.0434,0.0409),
                   NSites=c(26.5,97,123,117,68,38,23,22))

df6C <- rbind(df4, df41)
df6C$StemNumber <- factor(df6C$StemNumber,levels=c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"))
coeff6C <- ((max(df6C$NSites, na.rm=TRUE))/0.25)
p6C <- ggplot(df6C, aes(x=StemNumber)) +
  geom_bar(aes(y=BaMort, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff6C, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="Proportion of dead basal area",sec.axis =sec_axis(~.*coeff6C, name="NSites")) +
  theme(legend.position = "none") 

##fig 6D
df5 <- data.frame(Period=rep(c("1","2","3"),each=8),
                  StemNumber=rep(c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),3),
                  BaMort=c(colMeans(baNmort$baMort[period1,]),colMeans(baNmort$baMort[period2,]),colMeans(baNmort$baMort[period3,])),
                  NSites=c(colMeans(baNmort$nData[period1,]),colMeans(baNmort$nData[period2,]),colMeans(baNmort$nData[period3,])))

df51 <- data.frame(Period=rep(c("Mod"),8),
                  StemNumber=rep(c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"),3),
                  BaMort=c(1.638,1.437,1.543,1.459,1.443,1.415,1.370,1.610),
                  NSites=c(186,1085,2270,2675,1817,1134,729,890))

df6D <- rbind(df5, df51)
df6D$StemNumber <- factor(df6D$StemNumber,levels=c("<200","200-399","400-599","600-799","800-999","1000-1119","1200-1399",">1400"))
coeff6D <- ((max(df6D$NSites, na.rm=TRUE))/2)
p6D <- ggplot(df6D, aes(x=StemNumber)) +
  geom_bar(aes(y=BaMort, fill=Period), stat="identity", position=position_dodge()) +
  geom_path(aes(y=NSites / coeff6D, group=Period, colour=Period), stat="identity") +
  scale_y_continuous(name="Basal area of dead trees",sec.axis =sec_axis(~.*coeff6D, name="NSites")) +
  theme(legend.position = "none") 

##Combine figure
figure1 <- ggarrange(p5A, labels = c("5A"),
                     hjust = -0.2, vjust = -0.25,
                     common.legend = TRUE, legend = "right",
                     nrow = 1, ncol=2)

figure2 <- ggarrange(p6A,p6B, labels = c("6A","6B"), 
                     hjust = -0.2, vjust = -0.25,
                     common.legend = TRUE, legend = "right",
                     nrow = 1, ncol=2)

figure3 <- ggarrange(p6C,p6D, labels = c("6C","6D"), 
                     hjust = -0.2, vjust = -0.25,
                     common.legend = TRUE, legend = "right",
                     nrow = 1, ncol=2)

figure <- ggarrange(figure1,figure2,figure3,
                     common.legend = FALSE, legend = "right",
                     align = "h",
                     nrow=3, ncol=1)

annotate_figure(figure, top=text_grob("Uusimaa - Base(15) - Comp(3)",size=14))
# >>>>>>> bdf80a2ffaa7c92f0a474673d66a51e87c9dc527
