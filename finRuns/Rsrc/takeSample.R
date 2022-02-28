library(data.table)

# load("C:/Users/checcomi/Documents/research/IBC-carbon/test/data.all_maakunta_5.rdata")
if(!exists("r_no")) r_no <- 5 
if(!exists("sampleID")) sampleID=3
if(!exists("harvestscenarios")) harvestscenarios <- "Base"

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

age <- data.all$age
ageClass <- seq(0,quantile(data.all$age,0.99)
                ,by=10)

nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
set.seed(1)
ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
sampleXin <- ops[[sampleID]]
hist(age,freq=0)
hist(ops[[sampleID]]$age,add=T,col=2,freq=0)


for(i in 1:length(ageClass)){
  if (i<length(ageClass)) sampleXin[age %between% c(ageClass[i],ageClass[i+1]),class:=i]
  if (i==length(ageClass)) sampleXin[age > ageClass[i],class:=i]
}

tabX <- sampleXin[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(ageClass) + classNew]



sampleXyoung <- data.table()
nSample <- round(nrow(sampleXin)/length(ageClass))
for(i in 1:length(ageClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  


sampleXuni <- data.table()
nSample <- round(nrow(sampleXin)/length(ageClass))
for(i in 1:length(ageClass)){
  if(i<length(ageClass)) sampleNew <- data.all[age %between% c(ageClass[i],ageClass[i+1])][sample(nSample,replace = T)]
  if (i==length(ageClass)) sampleNew <- data.all[age > ageClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleXin$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleXin$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleXin$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleXin$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)




######BA based

# library(data.table)

baClass <- seq(0,quantile(data.all$ba,0.99)
                ,by=3)

sampleXin <- ops[[sampleID]]
hist(data.all$ba,freq=0)
hist(ops[[sampleID]]$ba,add=T,col=2,freq=0)

for(i in 1:length(baClass)){
  if (i<length(baClass)) sampleXin[ba %between% c(baClass[i],baClass[i+1]),class:=i]
  if (i==length(baClass)) sampleXin[ba > baClass[i],class:=i]
}

tabX <- sampleXin[,.N,by=class]
tabX[,classNew:=class-3]
tabX[classNew<1,classNew:=length(baClass) + classNew]

sampleXyoung <- data.table()
nSample <- round(nrow(sampleXin)/length(baClass))
for(i in 1:length(baClass)){
  nSample <- tabX[classNew==i]$N
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXyoung <- rbind(sampleXyoung,sampleNew)
}  


sampleXuni <- data.table()
nSample <- round(nrow(sampleXin)/length(baClass))
for(i in 1:length(baClass)){
  if(i<length(baClass)) sampleNew <- data.all[ba %between% c(baClass[i],baClass[i+1])][sample(nSample,replace = T)]
  if (i==length(baClass)) sampleNew <- data.all[ba > baClass[i]][sample(nSample,replace = T)]
  sampleXuni <- rbind(sampleXuni,sampleNew)
}  

hist(sampleXin$age,col=2)
hist(sampleXyoung$age,col=4,add=T)
hist(sampleXuni$age,col=3,add=T)

hist(sampleXin$h,col=2)
hist(sampleXyoung$h,col=4,add=T)
hist(sampleXuni$h,col=3,add=T)

hist(sampleXin$dbh,col=2)
hist(sampleXyoung$dbh,col=4,add=T)
hist(sampleXuni$dbh,col=3,add=T)

hist(sampleXin$ba,freq=0,col=2)
hist(sampleXyoung$ba,freq=0,col=4,add=T)
hist(sampleXuni$ba,freq=0,col=3,add=T)

sampleToRun <- "sampleXuni"
harvestscenarios <- "Base"
scens <- c("Base", "Low", "NoHarv", "MaxSust")
           # "adapt","adaptNoAdH","adaptTapio",
           # "Mitigation","MitigationNoAdH")
           # "protect","protectNoAdH")

for(sampleToRun in c("sampleXuni","sampleXyoung")){
  datAllScen <- data.table()
  sampleX <- get(sampleToRun)  
  outType="testRun"
  # setkey(sampleX,segID)
  ####run Base
  for(harvestscenarios in scens){
    # harvestscenarios="Base"
    
    if(harvestscenarios=="Base"){
      # sampleX=sampleXrun
      initSoilCin=NA
      
      
      print(paste("start sample ID",sampleID))
      harscen = harvestscenarios
      if(outType=="uncRun"){
        area_tot <- sum(data.all$area) # ha
        sampleX[,area := 16^2/10000] 
        cA <- 1/nrow(sampleX) #area_tot/nrow(sampleX) 
      } else {
        sampleX[,area := N*16^2/10000] 
      }
      sampleX[,id:=climID]
      HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
      nSample = nrow(sampleX)#200#nrow(data.all)
      ## ---------------------------------------------------------
      i = 0
      rcpfile = rcps
      load(paste(climatepath, rcpfile,".rdata", sep=""))  
      #if(outType != "uncRun"){
      if(!outType %in% c("uncRun","uncSeg")){
        if(rcpfile=="CurrClim"){
          #####process data considering only current climate###
          # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
          maxRday <- max(dat$rday)
          xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
          dat = rbind(dat,dat,dat)
          dat[,rday:=xday]
        }
      }
      gc()
      data.sample = sample_data.f(sampleX, nSample)
      if(rcpfile=="CurrClim") data.sample$id <- data.sample$CurrClimID
      areas <- data.sample$area
      totAreaSample <- sum(data.sample$area)
      
      clim = prep.climate.f(dat, data.sample, startingYear, nYears)
      
      Region = nfiareas[ID==r_no, Region]
      if(outType %in% c("uncRun","uncSeg")){
        pCrobasX <- pCROBASr[[sampleID]]
      }
      initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                         startingYear = startingYear,domSPrun=domSPrun,
                                         harv=harscen)
      
      
      opsna <- which(is.na(initPrebas$multiInitVar))
      initPrebas$multiInitVar[opsna] <- 0.
      
      if(harscen %in% c("adapt","protect","protectNoAdH",
                        "adaptNoAdH","adaptTapio")){
        sitesXs <- which(initPrebas$siteInfo[,3]>3)
        jj <- which(initPrebas$initCLcutRatio[sitesXs,2]>0.)
        initPrebas$initCLcutRatio[sitesXs[jj],2] <- 0.
        
        sitesXs <- which(initPrebas$siteInfo[,3]<3)
        jj <- which(initPrebas$initCLcutRatio[sitesXs,1]>0.)
        initPrebas$initCLcutRatio[sitesXs[jj],1] <- 0.
        
        xx <- 1/rowSums(initPrebas$initCLcutRatio)
        initPrebas$initCLcutRatio <- sweep(initPrebas$initCLcutRatio,MARGIN = 1,xx, `*`)
        jj <- which(is.na(rowSums(initPrebas$initCLcutRatio)))
        initPrebas$initCLcutRatio[jj,] <- 0.
        initPrebas$initCLcutRatio[jj,3] <- 1
        jj <- which(initPrebas$initCLcutRatio[,3]<0.2)
        xx <- 1-(0.2 - initPrebas$initCLcutRatio[jj,3])
        initPrebas$initCLcutRatio[jj,1:2] <- 
          sweep(initPrebas$initCLcutRatio[jj,1:2],MARGIN=1,xx, `*`)
        initPrebas$initCLcutRatio[jj,3] <- 0.2
      }
      if(rcpfile=="CurrClim"){
        #if(outType=="uncRun"){
        if(outType %in% c("uncRun","uncSeg")){
          resampleYear <- resampleYears[sampleID,] 
          #sample(1:nYears,nYears,replace=T)
        }else{
          set.seed(10)
          resampleYear <- sample(1:nYears,nYears)
        } 
        initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
        initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
        initPrebas$weather <- initPrebas$weather[,resampleYear,,]
        initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
      }
      
      
      # Loop management scenarios ------------------------------------------------
      # for(harscen in harvestscenarios) { ## MaxSust fails, others worked.
      # print(date())
      # print(harscen)
      i = i + 1
      # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
      # harscen ="Base"
      
      ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
      if(regSets!="maakunta"){
        Region = nfiareas[ID==r_no, Region]
        if(harscen=="NoHarv"){
          initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
          HarvLim1 = 0
        }else if(harscen=="Tapio"){
          HarvLim1 = 0
        }else{
          HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
          HarvLim0  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2015-2024"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- rep(as.numeric(HarvLim),10)
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2025-2034"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2035-2044"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2045-2054"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2055-2064"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),44))
        }
        ## In the model, harvests are always per hectar units. If 1000 pixels (nSample)
        ## are simulated it corresponds to 1000 hectars, although pixels are only 16x16 m2.
        ## Therefore, we need to apply the areal fraction of removals scenarios
        ## nfiareas are in 1000 ha, model takes Harvlim in m3, while removals from Mela are 1000 m3
        #      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
        if(year1harv==1){
          HarvLim1 <- HarvLimX
          if(harscen == "Low"){ HarvLim1 <- HarvLimX * 0.6}
          if(harscen == "MaxSust"){HarvLim1 <- HarvLimX * 1.2}
          if(harscen == "NoHarv"){
            HarvLim1 <- HarvLimX * 0.
            initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
          }
        }else{
          roundWood <- HarvLim1 * roundTotWoodRatio
          enWood <- HarvLim1 - roundWood
          HarvLim1 <- cbind(roundWood,enWood)
        }
      }else{
        HarvLim1 <- HarvLimMaak*1000*sum(areas)/sum(data.all$area)
        if(harscen == "Low"){ HarvLim1 <- HarvLim1 * 0.6}
        if(harscen == "MaxSust"){HarvLim1 <- HarvLim1 * 1.2}
        if(harscen == "NoHarv"){HarvLim1 <- HarvLim1 * 0.
        initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
        }
      }          
      
      ###calculate clearcutting area for the sample
      #if(!is.na(cutArX)){
      print("calculating clearcutting areas")
      clcutArX <- clcutAr * sum(areas)/sum(data.all$area)
      clcutArX <- cbind(clcutArX[1:nYears],0.)
      tendX <- tendingAr * sum(areas)/sum(data.all$area)
      tendX <- cbind(tendX[1:nYears],0.)
      fThinX <- firstThinAr * sum(areas)/sum(data.all$area)
      fThinX <- cbind(fThinX[1:nYears],0.)
      cutArX <- cbind(clcutArX,tendX)
      cutArX <- cbind(cutArX,fThinX)
      # }else{
      #   cutArX <- NA
      # } 
      # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
      # HarvLim1 <- rep(0,2)
      # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
      # region <- regionPrebas(initPrebas)
      ###run PREBAS
      if(!all(is.na(initSoilCin))){
        initPrebas$soilC[,1,,,] <- initSoilC <- initSoilCin
      }
      print(harscen)
      HarvLimX <- HarvLim1[1:nYears,]
      
      if(harscen %in% c("adapt","adaptNoAdH","adaptTapio")){
        if(harscen=="adaptNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        # HarvLimX[,2]=0.
        # initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        if(harscen=="adaptTapio"){
          region <- regionPrebas(initPrebas,
                                 fertThin = fertThin,nYearsFert = nYearsFert)
        }else{
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 cutAreas = cutArX,compHarv=compHarvX,
                                 fertThin = fertThin,nYearsFert = nYearsFert)
        }
      }else if(harscen %in% c("Mitigation","MitigationNoAdH")){
        if(harscen=="MitigationNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        HarvLimX[,2]=0.
        initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        load(paste0("input/",regSets,"/pClCut_mitigation/ClCutplots_maak",r_no,".rdata"))
        ClcutX <- updatePclcut(initPrebas,pClCut)
        initPrebas$inDclct <- ClcutX$inDclct
        initPrebas$inAclct <- ClcutX$inAclct
        initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
        region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                               cutAreas =cutArX,compHarv=compHarvX,
                               ageHarvPrior = ageHarvPriorX)
      }else if(harscen %in% c("protect","protectNoAdH")){
        if(harscen=="protectNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        ####no energy cuts
        HarvLimX[,2]=0.
        initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                               cutAreas =cutArX,compHarv=compHarvX,
                               ageHarvPrior = ageHarvPriorX,
                               oldLayer = 1)
      }else{
        ##Don't pass minDharvX if NA
        if (is.na(minDharvX)) {
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 cutAreas =cutArX,compHarv=compHarvX)
        } else {
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 minDharv = minDharvX,cutAreas =cutArX,
                                 compHarv=compHarvX)
        }
      }
      
      print(paste("runModel",sampleID,"completed"))
      ##calculate steady state carbon from prebas litter 
      if(harscen=="Base"){
        initSoilC <- stXX_GV(region, 1)
        print(paste("initSoilC",sampleID))
        # if(outType!="testRun"){
        #   if(!outType %in% c("uncRun","uncSeg")){
        #     save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
        #   } else {
        #     save(initSoilC,file=paste0("initSoilCunc/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
        #   }
        # }
        ###run yasso (starting from steady state) using PREBAS litter
        region <- yassoPREBASin(region,initSoilC)
        # out <- region$multiOut[,,,,1]
      }
      print(paste("all runs done",sampleID))
      
      #####start initialize deadWood volume
      ## identify managed and unmanaged forests
      manFor <-  which(sampleX$cons==0)
      unmanFor <- which(sampleX$cons==1)
      if(outType=="ststDeadW"){
        unmanDeadW <- initDeadW(region,unmanFor,yearsDeadW)
        manDeadW <- initDeadW(region,manFor,yearsDeadW)
        save(unmanDeadW,manDeadW,file=paste0("initDeadWVss/reg",
                                             r_no,"_deadWV.rdata"))
        return("deadWood volume at steady state saved")
      }else{
        load(paste0("initDeadWVss/reg",
                    r_no,"_deadWV.rdata"))
        deadWx <- aperm(replicate(length(manFor),(manDeadW$ssDeadW[1:nYears,])),c(3,1:2))
        region$multiOut[manFor,,8,1:3,1] <- region$multiOut[manFor,,8,1:3,1] + deadWx
        deadWx <- aperm(replicate(length(unmanFor),(unmanDeadW$ssDeadW[1:nYears,])),c(3,1:2))
        region$multiOut[unmanFor,,8,1:3,1] <- region$multiOut[unmanFor,,8,1:3,1] + deadWx
      }
      ####end initialize deadWood Volume
      
      
      
      initSoilCin=initSoilC
    }else{
      print(paste("start sample ID",sampleID))
      harscen = harvestscenarios
      if(outType=="uncRun"){
        area_tot <- sum(data.all$area) # ha
        sampleX[,area := 16^2/10000] 
        cA <- 1/nrow(sampleX) #area_tot/nrow(sampleX) 
      } else {
        sampleX[,area := N*16^2/10000] 
      }
      sampleX[,id:=climID]
      HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
      nSample = nrow(sampleX)#200#nrow(data.all)
      ## ---------------------------------------------------------
      i = 0
      rcpfile = rcps
      load(paste(climatepath, rcpfile,".rdata", sep=""))  
      #if(outType != "uncRun"){
      if(!outType %in% c("uncRun","uncSeg")){
        if(rcpfile=="CurrClim"){
          #####process data considering only current climate###
          # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
          maxRday <- max(dat$rday)
          xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
          dat = rbind(dat,dat,dat)
          dat[,rday:=xday]
        }
      }
      gc()
      data.sample = sample_data.f(sampleX, nSample)
      if(rcpfile=="CurrClim") data.sample$id <- data.sample$CurrClimID
      areas <- data.sample$area
      totAreaSample <- sum(data.sample$area)
      
      clim = prep.climate.f(dat, data.sample, startingYear, nYears)
      
      Region = nfiareas[ID==r_no, Region]
      if(outType %in% c("uncRun","uncSeg")){
        pCrobasX <- pCROBASr[[sampleID]]
      }
      initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                         startingYear = startingYear,domSPrun=domSPrun,
                                         harv=harscen)
      
      
      opsna <- which(is.na(initPrebas$multiInitVar))
      initPrebas$multiInitVar[opsna] <- 0.
      
      if(harscen %in% c("adapt","protect","protectNoAdH",
                        "adaptNoAdH","adaptTapio")){
        sitesXs <- which(initPrebas$siteInfo[,3]>3)
        jj <- which(initPrebas$initCLcutRatio[sitesXs,2]>0.)
        initPrebas$initCLcutRatio[sitesXs[jj],2] <- 0.
        
        sitesXs <- which(initPrebas$siteInfo[,3]<3)
        jj <- which(initPrebas$initCLcutRatio[sitesXs,1]>0.)
        initPrebas$initCLcutRatio[sitesXs[jj],1] <- 0.
        
        xx <- 1/rowSums(initPrebas$initCLcutRatio)
        initPrebas$initCLcutRatio <- sweep(initPrebas$initCLcutRatio,MARGIN = 1,xx, `*`)
        jj <- which(is.na(rowSums(initPrebas$initCLcutRatio)))
        initPrebas$initCLcutRatio[jj,] <- 0.
        initPrebas$initCLcutRatio[jj,3] <- 1
        jj <- which(initPrebas$initCLcutRatio[,3]<0.2)
        xx <- 1-(0.2 - initPrebas$initCLcutRatio[jj,3])
        initPrebas$initCLcutRatio[jj,1:2] <- 
          sweep(initPrebas$initCLcutRatio[jj,1:2],MARGIN=1,xx, `*`)
        initPrebas$initCLcutRatio[jj,3] <- 0.2
      }
      if(rcpfile=="CurrClim"){
        #if(outType=="uncRun"){
        if(outType %in% c("uncRun","uncSeg")){
          resampleYear <- resampleYears[sampleID,] 
          #sample(1:nYears,nYears,replace=T)
        }else{
          set.seed(10)
          resampleYear <- sample(1:nYears,nYears)
        } 
        initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
        initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
        initPrebas$weather <- initPrebas$weather[,resampleYear,,]
        initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
      }
      
      
      # Loop management scenarios ------------------------------------------------
      # for(harscen in harvestscenarios) { ## MaxSust fails, others worked.
      # print(date())
      # print(harscen)
      i = i + 1
      # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
      # harscen ="Base"
      
      ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
      if(regSets!="maakunta"){
        Region = nfiareas[ID==r_no, Region]
        if(harscen=="NoHarv"){
          initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
          HarvLim1 = 0
        }else if(harscen=="Tapio"){
          HarvLim1 = 0
        }else{
          HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
          HarvLim0  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2015-2024"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- rep(as.numeric(HarvLim),10)
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2025-2034"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2035-2044"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2045-2054"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
          HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2055-2064"]
          HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
          HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),44))
        }
        ## In the model, harvests are always per hectar units. If 1000 pixels (nSample)
        ## are simulated it corresponds to 1000 hectars, although pixels are only 16x16 m2.
        ## Therefore, we need to apply the areal fraction of removals scenarios
        ## nfiareas are in 1000 ha, model takes Harvlim in m3, while removals from Mela are 1000 m3
        #      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
        if(year1harv==1){
          HarvLim1 <- HarvLimX
          if(harscen == "Low"){ HarvLim1 <- HarvLimX * 0.6}
          if(harscen == "MaxSust"){HarvLim1 <- HarvLimX * 1.2}
          if(harscen == "NoHarv"){
            HarvLim1 <- HarvLimX * 0.
            initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
          }
        }else{
          roundWood <- HarvLim1 * roundTotWoodRatio
          enWood <- HarvLim1 - roundWood
          HarvLim1 <- cbind(roundWood,enWood)
        }
      }else{
        HarvLim1 <- HarvLimMaak*1000*sum(areas)/sum(data.all$area)
        if(harscen == "Low"){ HarvLim1 <- HarvLim1 * 0.6}
        if(harscen == "MaxSust"){HarvLim1 <- HarvLim1 * 1.2}
        if(harscen == "NoHarv"){HarvLim1 <- HarvLim1 * 0.
        initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
        }
      }          
      
      ###calculate clearcutting area for the sample
      #if(!is.na(cutArX)){
      print("calculating clearcutting areas")
      clcutArX <- clcutAr * sum(areas)/sum(data.all$area)
      clcutArX <- cbind(clcutArX[1:nYears],0.)
      tendX <- tendingAr * sum(areas)/sum(data.all$area)
      tendX <- cbind(tendX[1:nYears],0.)
      fThinX <- firstThinAr * sum(areas)/sum(data.all$area)
      fThinX <- cbind(fThinX[1:nYears],0.)
      cutArX <- cbind(clcutArX,tendX)
      cutArX <- cbind(cutArX,fThinX)
      # }else{
      #   cutArX <- NA
      # } 
      # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
      # HarvLim1 <- rep(0,2)
      # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
      # region <- regionPrebas(initPrebas)
      ###run PREBAS
      if(!all(is.na(initSoilCin))){
        initPrebas$soilC[,1,,,] <- initSoilC <- initSoilCin
      }
      print(harscen)
      HarvLimX <- HarvLim1[1:nYears,]
      
      if(harscen %in% c("adapt","adaptNoAdH","adaptTapio")){
        if(harscen=="adaptNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        # HarvLimX[,2]=0.
        # initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        if(harscen=="adaptTapio"){
          region <- regionPrebas(initPrebas,
                                 fertThin = fertThin,nYearsFert = nYearsFert)
        }else{
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 cutAreas = cutArX,compHarv=compHarvX,
                                 fertThin = fertThin,nYearsFert = nYearsFert)
        }
      }else if(harscen %in% c("Mitigation","MitigationNoAdH")){
        if(harscen=="MitigationNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        HarvLimX[,2]=0.
        initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        load(paste0("input/",regSets,"/pClCut_mitigation/ClCutplots_maak",r_no,".rdata"))
        ClcutX <- updatePclcut(initPrebas,pClCut)
        initPrebas$inDclct <- ClcutX$inDclct
        initPrebas$inAclct <- ClcutX$inAclct
        initPrebas$thinInt <- rep(thinIntX,initPrebas$nSites)
        region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                               cutAreas =cutArX,compHarv=compHarvX,
                               ageHarvPrior = ageHarvPriorX)
      }else if(harscen %in% c("protect","protectNoAdH")){
        if(harscen=="protectNoAdH"){
          compHarvX=0.
        }else{
          compHarvX=3.
        }
        ####no energy cuts
        HarvLimX[,2]=0.
        initPrebas$energyCut <- rep(0,length(initPrebas$energyCut))
        region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                               cutAreas =cutArX,compHarv=compHarvX,
                               ageHarvPrior = ageHarvPriorX,
                               oldLayer = 1)
      }else{
        ##Don't pass minDharvX if NA
        if (is.na(minDharvX)) {
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 cutAreas =cutArX,compHarv=compHarvX)
        } else {
          region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLimX),
                                 minDharv = minDharvX,cutAreas =cutArX,
                                 compHarv=compHarvX)
        }
      }
      
      print(paste("runModel",sampleID,"completed"))
      ##calculate steady state carbon from prebas litter 
      if(harscen=="Base"){
        initSoilC <- stXX_GV(region, 1)
        print(paste("initSoilC",sampleID))
        # if(outType!="testRun"){
        #   if(!outType %in% c("uncRun","uncSeg")){
        #     save(initSoilC,file=paste0("initSoilC/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
        #   } else {
        #     save(initSoilC,file=paste0("initSoilCunc/forCent",r_no,"/initSoilC_",sampleID,".rdata"))
        #   }
        # }
        ###run yasso (starting from steady state) using PREBAS litter
        region <- yassoPREBASin(region,initSoilC)
        # out <- region$multiOut[,,,,1]
      }
      print(paste("all runs done",sampleID))
      
      #####start initialize deadWood volume
      ## identify managed and unmanaged forests
      manFor <-  which(sampleX$cons==0)
      unmanFor <- which(sampleX$cons==1)
      if(outType=="ststDeadW"){
        unmanDeadW <- initDeadW(region,unmanFor,yearsDeadW)
        manDeadW <- initDeadW(region,manFor,yearsDeadW)
        save(unmanDeadW,manDeadW,file=paste0("initDeadWVss/reg",
                                             r_no,"_deadWV.rdata"))
        return("deadWood volume at steady state saved")
      }else{
        load(paste0("initDeadWVss/reg",
                    r_no,"_deadWV.rdata"))
        deadWx <- aperm(replicate(length(manFor),(manDeadW$ssDeadW[1:nYears,])),c(3,1:2))
        region$multiOut[manFor,,8,1:3,1] <- region$multiOut[manFor,,8,1:3,1] + deadWx
        deadWx <- aperm(replicate(length(unmanFor),(unmanDeadW$ssDeadW[1:nYears,])),c(3,1:2))
        region$multiOut[unmanFor,,8,1:3,1] <- region$multiOut[unmanFor,,8,1:3,1] + deadWx
      }
      ####end initialize deadWood Volume
      
    }
    
    # region <- modRun$region
    # rm(modRun); gc()
    datAll <- data.table()
    segID <- region$siteInfo[,1]
    for(i in 1:length(varSel)){
      # i=2
      datX <- outProcFun(region,varSel[i],funX[i])
      datX <- melt(datX,"segID")
      setnames(datX,c("variable","value"),c("year",varNames[varSel[i]]))
      if(i ==1){
        datAll <- datX
      }else{
        setkey(datX,segID,year)
        setkey(datAll,segID,year)
        datAll <- merge(datAll,datX)
      }
      # print(varNames[varSel[i]])
    }
    ####proc Spec vars
    ###dominant Species
    datX <- domFun(region,varX="species")  
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","domSp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ###age dominant species
    datX <- domFun(region,varX="age")
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","ageDom"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ###deciduous Volume Vdec
    datX <- vDecFun(region)
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","Vdec"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####WenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,2],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####VenergyWood
    datX <- data.table(segID=segID,apply(region$multiEnergyWood[,,,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","VenergyWood"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####GVgpp
    datX <- data.table(segID=segID,region$GVout[,,3])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVgpp"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####GVw
    datX <- data.table(segID=segID,region$GVout[,,4])
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","GVw"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    ####Wtot
    datX <- data.table(segID=segID,apply(region$multiOut[,,c(24,25,31,32,33),,1],1:2,sum))
    setnames(datX,c("segID",1:region$maxYears))
    datX <- melt(datX,"segID")
    setnames(datX,c("variable","value"),c("year","WtotTrees"))
    setkey(datX,segID,year)
    setkey(datAll,segID,year)
    datAll <- merge(datAll,datX)
    
    datAll$year <- as.numeric(as.character(datAll$year))
    datAll$maakID <- r_no 
    datAll$harScen <- harvestscenarios
    datAllScen <- rbind(datAllScen,datAll)
    
    print(harvestscenarios)
    # rm(list=setdiff(ls(), c(toMem,"toMem"))); gc()
  }
  
  areas <- data.table(segID=region$siteInfo[,1],area=region$areas)
  
  save(datAllScen,areas,
       file=paste0("outSample/r_no",r_no,"_",sampleToRun,".rdata"))
  
}





######Make plots
library(dplyr)
library(ggplot2)

for(sampleToRun in c("sampleXuni","sampleXyoung")){
  r_no=5
  load(paste0("/scratch/project_2000994/PREBASruns/finRuns/outSample/r_no"
              ,r_no,"_",sampleToRun,".rdata"))
  
  datAllScenNorm <- datAllScen
  # datAllScenNormProtect <- datAllScenProtect
  setkey(areas,segID)
  setkey(datAllScenNorm,segID)
  # setkey(areasProtect,segID)
  # setkey(datAllScenNormProtect,segID)
  datAllScenNorm <- merge(datAllScenNorm,areas)
  # datAllScenNormProtect <- merge(datAllScenNormProtect,areasProtect)
  vars <- colnames(datAllScenNorm)[!colnames(datAllScenNorm) %in% c("segID","area","year","maakID","harScen")]
  # datAllScenNorm[,normFact:=area*length(areas$area)/sum(areas$area)]
  datAllScenNorm[, vars] <- 
    datAllScenNorm[ ,lapply(.SD, `*`, area*length(areas$area)/sum(areas$area)), .SDcols = vars]
  
  # datAllScenNormProtect[, vars] <- 
    # datAllScenNormProtect[ ,lapply(.SD, `*`, area*length(areasProtect$area)/sum(areasProtect$area)), .SDcols = vars]
  
  plot.list <- list()
  i=0
  for(varX in vars){
    i=i+1
    sumryX <- datAllScenNorm %>%   
      group_by(year, harScen) %>%
      summarise(medi = median(get(varX),na.rm=T),
                q0.25 = quantile(get(varX),probs=0.25,na.rm=T),
                q0.75 = quantile(get(varX),probs=0.75,na.rm=T))
    # 
    # sumryXProtect <- datAllScenNormProtect %>%   
      # group_by(year, harScen) %>%
      # summarise(medi = median(get(varX),na.rm=T),
      #           q0.25 = quantile(get(varX),probs=0.25,na.rm=T),
      #           q0.75 = quantile(get(varX),probs=0.75,na.rm=T))
    
    # sumryX <- rbind(sumryX,sumryXProtect)
    plot.list[[i]] <- ggplot(sumryX)+
      geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen)) +
      xlab("year") + ylab(varX)
    
    i=i+1
    
    plot.list[[i]] <- ggplot(sumryX)+
      # geom_ribbon(aes(x = year + 2016, ymin = q0.25, ymax = q0.75,fill= harScen), alpha = 0.3)+
      geom_line(aes(x = year+ 2016, y = medi, color = harScen)) + 
      xlab("year") + ylab(varX)
  }
  
  pdf(paste0("outSample/plots",r_no,"_",sampleToRun,".pdf"))
  for(i in 1:length(plot.list)) print(plot.list[[i]])
  dev.off()
}


