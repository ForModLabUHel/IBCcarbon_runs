library(data.table)

datX <- fread("C:/Users/minunno/Documents/research/IBC-carbon/NFIageClasses/Age-classes-by-county-tree-species.txt")
unique(datX$maakunta_12)
myID <-   c(1,11,14,9,4,13,15,5,17, 7,18, 6,12,10, 3,19,16, 8, 2)
lukeID <- c(1, 2, 4,5,6, 7, 8,9,10,11,12,13,14,15,16,17,18,19,21)
IDs <- cbind(lukeID,myID)
datX$maakID <- myID[match(datX$maakunta_12,IDs)]
datX[,areaReg:=sum(Area_ha),by=maakID]
ageClass_probs <- datX[,unique(sum(Area_ha)/areaReg),by=.(age20,maakID)]
setnames(ageClass_probs,"V1","probAgeClass")
plot(probs[maakID==1]$V1)
save(ageClass_probs,file="C:/Users/minunno/Documents/research/IBC-carbon/NFIageClasses/ageClass_probs.rdata")
