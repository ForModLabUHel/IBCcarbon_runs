devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")

load("segAreas.rdata")
# rm(list=setdiff(ls(), "areas"))
# gc()

clims <- "CurrClim" #c("CurrClim", "rcp26", "rcp45")
mans <- "Base"#c("Low", "MaxSust","Base") #"Base"


nYears <- 84
dtX <- data.table()
# varX <- "age"
variables <- unique(sub("_.*","",list.files("outputDT/")))
variables[c(19,22)] <- c("W_croot","wf_STKG")
for(varX in variables){
  for(harvScen in mans){
    dtX <- rbind(dtX,cbind(as.numeric(calMean(varX,harvScen,areas)),harvScen,1:nYears,varX))
  }
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
