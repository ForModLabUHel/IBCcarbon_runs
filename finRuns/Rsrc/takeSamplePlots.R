library(data.table)
library(Rprebasso)
library(dplyr)
library(ggplot2)

r_no <- "r_no7"
sims <- paste0(r_no,c("_sampleXuni","_sampleXyoung","_sampleXin"))#,
          # "_sampleXinNoAddHarv","_sampleXuniNoAddHarv","_sampleXyoungNoAddHarv"))
scenXs <- c("Base","NoHarv")#"Low","MaxSust",

sumry <- data.table()
for(simX in sims){
  # simX=sims[1]
  load(paste0("~/research/IBC-carbon/test/",simX,".rdata"))
  
  datAllScen$Clength <- datAllScen$H - datAllScen$Hc_base
  datAllScenNorm <- datAllScen
  
  setkey(areas,segID)
  setkey(datAllScenNorm,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas,allow.cartesian=T)
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen")]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  for(varX in vars){
    if(varX %in% c("WroundWood","VroundWood","WenergyWood","VenergyWood")){
      sumryX <- datAllScenNorm[,.(median=mean(get(varX)),
                                  p25=mean(get(varX)),
                                  p75=mean(get(varX))),
                               by=.(year, harScen)]
    }else{
      sumryX <- datAllScenNorm[,.(median=median(get(varX)),
                                  p25=quantile(get(varX),probs=0.25),
                                  p75=quantile(get(varX),probs=0.75)),
                               by=.(year, harScen)]
    }
    sumryX$variable <- varX
    sumryX$sim <- simX
    sumry <- rbind(sumry,sumryX)
    print(varX)
  }  
}

for(scenX in scenXs){
  plot.list <- list()
  i=0
  for(varX in vars){
    # varX=vars[1]
    datSel <- sumry[harScen==scenX & variable == varX]
  i=i+1
    plot.list[[i]] <- ggplot(datSel)+
      geom_ribbon(aes(x = year + 2016, ymin = p25, ymax = p75,fill= sim), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = median, color = sim)) +
      xlab("year") + ylab(varX)
    
  i=i+1
    plot.list[[i]] <- ggplot(datSel)+
      geom_line(aes(x = year+ 2016, y = median, color = sim)) + 
      xlab("year") + ylab(varX)
  }
  
  pdf(paste0("~/research/IBC-carbon/test/plots",r_no,"_",scenX,".pdf"))
  for(i in 1:length(plot.list)) print(plot.list[[i]])
  dev.off()
}


plot.list <- list()
i=0
for(varX in vars){
  # varX=vars[1]
  datSel <- sumry[harScen%in%scenXs & variable == varX]
  datSel$harScen <- as.factor(datSel$harScen)
  datSel$sim <- as.factor(datSel$sim)

  i=i+1
  plot.list[[i]] <- ggplot(datSel)+
    geom_point(aes(x = year+ 2016, y = median, color = sim,shape=harScen))+
    # geom_line(aes(x = year+ 2016, y = median, color = sim)) +
    xlab("year") + ylab(varX)
  for(harscenX in scenXs) plot.list[[i]] <- plot.list[[i]] + 
     geom_line(data=datSel[harScen==harscenX],aes(x = year+ 2016, y = median, color = sim))
}

pdf(paste0("~/research/IBC-carbon/test/plots",r_no,"_allScen.pdf"))
for(i in 1:length(plot.list)) print(plot.list[[i]])
dev.off()
