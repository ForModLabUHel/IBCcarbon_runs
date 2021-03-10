r_no <- regions <- 1
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

pathFiles <- "outputDT/forCent1/"
varX <- "age"

fileXs <- list.files(path = pathFiles, pattern = varX)
outX <- data.table()
for(i in 1:length(fileXs)){
  load(paste0(pathFiles,fileXs[i]))
  outX <- rbind(outX,get(varX))
}
  
load(paste0("input/forCent_",r_no,"_IDsTab.rdata"))
forCentIDsTab <- forCentIDsTab[segID!=0]

setkey(outX,segID)
setkey(forCentIDsTab,segID)

tabX <- merge(outX,forCentIDsTab)

