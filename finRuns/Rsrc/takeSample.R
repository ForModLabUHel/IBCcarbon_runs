library(data.table)

# load("C:/Users/checcomi/Documents/research/IBC-carbon/test/data.all_maakunta_5.rdata")
age <- data.all$age
ageClass <- seq(0,quantile(data.all$age,0.99)
                ,by=10)

nSamples=25
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
sampleID=3
sampleX <- ops[[sampleID]]
hist(age,freq=0)
hist(ops[[sampleID]]$age,add=T,col=2,freq=0)


for(i in 1:length(ageClass)){
  if (i<length(ageClass)) sampleX[age %between% c(ageClass[i],ageClass[i+1]),class:=i]
  if (i==length(ageClass)) sampleX[age > ageClass[i],class:=i]
}

tabX <- sampleX[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(ageClass) + classNew]



sampleXyoung <- data.table()
nSample <- round(nrow(sampleX)/length(ageClass))
for(i in 1:length(ageClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  





sampleXuni <- data.table()
nSample <- round(nrow(sampleX)/length(ageClass))
for(i in 1:length(ageClass)){
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleX$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleX$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleX$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleX$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)







######BA based

library(data.table)

load("C:/Users/checcomi/Documents/research/IBC-carbon/test/data.all_maakunta_5.rdata")

baClass <- seq(0,quantile(data.all$ba,0.99)
                ,by=3)

nSamples=25
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
sampleID=3
sampleX <- ops[[sampleID]]
hist(data.all$ba,freq=0)
hist(ops[[sampleID]]$ba,add=T,col=2,freq=0)


for(i in 1:length(baClass)){
  if (i<length(baClass)) sampleX[ba %between% c(baClass[i],baClass[i+1]),class:=i]
  if (i==length(baClass)) sampleX[ba > baClass[i],class:=i]
}

tabX <- sampleX[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(baClass) + classNew]



sampleXyoung <- data.table()
nSample <- round(nrow(sampleX)/length(baClass))
for(i in 1:length(baClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  


sampleXuni <- data.table()
nSample <- round(nrow(sampleX)/length(baClass))
for(i in 1:length(baClass)){
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleX$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleX$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleX$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleX$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)

