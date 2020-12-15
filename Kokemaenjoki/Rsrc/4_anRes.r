.libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
libpath <- .libPaths()[1]
setwd("/scratch/project_2000994/PREBASruns/Kokemaenjoki/")
source("extractOutFunctions.R")
source("initializeMod.r")

clims <- "CurrClim" #c("CurrClim", "rcp26", "rcp45")
mans <- c("Low", "MaxSust","Base") #"Base"


load("segAreas.rdata")
rm(list=setdiff(ls(), "areas"))
gc()
calMean <- function(varX,hscenX,areas){
  load(paste0("outputDT/",varX,"_",hscenX,"_CurrClim.rdata"))
  varAreas <- get(varX)*areas
  # Vareas <- Vareas[-siteX]
  totX <- colSums(varAreas,na.rm = T)
  meanX <- totX/sum(areas)#co
  return(meanX)
}

nYears <- 84
dtX <- data.table()
# varX <- "age"
variables <- unique(sub("_.*","",list.files("outputDT/")))
variables[c(19,22)] <- c("W_croot","wf_STKG")
for(varX in variables){
  harvScen <- "Base"
  dtX <- rbind(dtX,cbind(as.numeric(calMean(varX,harvScen,areas)),harvScen,1:nYears,varX))
  harvScen <- "MaxSust"
  dtX <- rbind(dtX,cbind(as.numeric(calMean(varX,harvScen,areas)),harvScen,1:nYears,varX))
  harvScen <- "Low"
  dtX <- rbind(dtX,cbind(as.numeric(calMean(varX,harvScen,areas)),harvScen,1:nYears,varX))
  # dtX[,variable:=varX]
  print(varX)
}

setnames(dtX,c("values","harvScen","simYear","variable"))
dtX$values <- as.numeric(dtX$values); dtX$harvScen <- as.factor(dtX$harvScen)
dtX$simYear <- as.numeric(dtX$simYear)

save(dtX,file="results/anRes.rdata")

#create plots
pdf("results/hscenPlots.pdf")
for(varX in variables){
  print(ggplot(dtX[variable==varX],aes(x=simYear,y=values,col=harvScen)) + 
  geom_point() + ggtitle(varX))
}
dev.off()
