library(data.table)
library(raster)
for(j in 1:19){
  print(j)
  load(paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/data.allOld/data.all_maakunta_",j,".rdata"))
  macRegs <- unique(data.all$regName)
  
  newDat <- data.table()
  for(i in 1:length(macRegs)){
    pseudoptyp <- fread(paste0("/scratch/project_2000994/MVMIsegments/mvmi15/",macRegs[i],"/segmaj1.pseudotype.hits"))
    pseudoptyp$V2=NULL
    setnames(pseudoptyp,c("segID","pseudoptyp"))
    pseudoptyp$regName <- macRegs[i]
    setkey(data.all,segID,regName)
    setkey(pseudoptyp,segID,regName)
    
    xx <- merge(data.all,pseudoptyp)
    newDat <- rbind(newDat,xx)
    print(i)
  }
  
  data.all <- newDat
  # ciao <- merge(data.all,newDat)
  save(data.all,file = paste0("/scratch/project_2000994/PREBASruns/finRuns/input/maakunta/data.all_maakunta_",j,".rdata"))
}
