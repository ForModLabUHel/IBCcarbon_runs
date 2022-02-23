library(dplyr)
library(ggplot2)

r_no=5
load(paste0("/scratch/project_2000994/PREBASruns/finRuns/outSample/r_no",r_no".rdata"))

datAllScenNorm <- datAllScen
setkey(areas,segID)
setkey(datAllScenNorm,segID)
datAllScenNorm <- merge(datAllScenNorm,areas)
vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen")]
# datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
datAllScenNorm[, vars] <- 
  datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]

pdf(paste0("outSample/plots",r_no,".pdf"))
for(varX in vars){
  sumryX <- datAllScenNorm %>%   
    group_by(year, harScen) %>%
    summarise(medi = median(get(varX),na.rm=T),
              q0.25 = quantile(get(varX),probs=0.25,na.rm=T),
              q0.75 = quantile(get(varX),probs=0.75,na.rm=T))

  ggplot(sumryX)+
    geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
    geom_line(aes(x = year+ 2016, y = medi, color = harScen)) + 
    xlab("year") + ylab(varX)

  ggplot(sumryX)+
    # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
    geom_line(aes(x = year+ 2016, y = medi, color = harScen)) + 
    xlab("year") + ylab(varX)
}
dev.off()


