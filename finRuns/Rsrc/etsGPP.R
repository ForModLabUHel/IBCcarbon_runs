library(data.table)
XdataX <- dataX <- data.table()
for(ijx in 1:15){
  r_no <- regions <- ijx
  print(r_no)
  sampleID <- 1
  manScen <- "Base"
  
  devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
  
  # setX=1
  nSitesRun = 20000
  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
  # sampleIDs <- split(1:nSamples,             # Applying split() function
  #                    cut(seq_along(1:nSamples),
  #                        nSetRuns,
  #                        labels = FALSE))[[setX]]
  set.seed(1)
  ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  
  
  
  
  sampleX <- ops[[sampleID]]
  sampleX[,area := N*16^2/10000]
  sampleX[,id:=climID]
  HarvLimX <- harvestLims * sum(sampleX$area)/sum(data.all$area)
  nSample = nrow(sampleX)#200#nrow(data.all)
  ## Loop management scenarios
  # harvestscenarios = c("Policy", "MaxSust", "Base","Low","Tapio","NoHarv") ## noharv must be the last element otherwise cons area are ignored
  # WRITEREGIONDATA = TRUE
  
  # climatepath = "/scratch/project_2000994/RCP/"
  
  # regionsummaries = data.table()
  
  
  ## ---------------------------------------------------------
  i = 0
  # load("/scratch/project_2000994/PREBASruns/metadata/initSoilCstst.rdata")
  # load("outSoil/InitSoilCstst_Base.rdata")
  rcpfile = rcps
  # print(rcpfile)
  if(rcpfile=="CurrClim"){
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
    #####process data considering only current climate###
    # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
    maxRday <- max(dat$rday)
    xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
    dat = rbind(dat,dat,dat)
    dat[,rday:=xday]
    
  } else{
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
  }
  # load("C:/Users/minunno/Documents/research/lukeDB/example #2/CanESM2.rcp45.rdata")
  
  ## Loop regions -------------------------------------------------------
  # for (r_no in regions) {
  # print(date())
  # print(paste("Region", r_no) )
  # r_no=7
  ## Load samples from regions; region-files include every 1000th pixel
  ## Pixel data are from 16 m x 16 m cells, but all numbers are per unit area.
  ## Model also produces per values  per hectar or m2.
  ## Note also that some of the pixels are non-forest (not metsamaa, kitumaa, joutomaa)
  ## or not inside Finland (32767) or may be cloudcovered (32766).
  
  # data.all = fread(paste(regiondatapath, "data.proc.", r_no, ".txt", sep=""))
  # data.all = fread(paste("data.proc.", r_no, ".txt",sep=""))
  # dat = dat[id %in% data.all[, unique(id)]]
  gc()
  ## Prepare the same initial state for all harvest scenarios that are simulated in a loop below
  data.sample = sample_data.f(sampleX, nSample)
  if(rcpfile=="CurrClim") data.sample$id <- data.sample$CurrClimID
  areas <- data.sample$area
  totAreaSample <- sum(data.sample$area)
  
  clim = prep.climate.f(dat, data.sample, startingYear, nYears)
  
  Region = nfiareas[ID==r_no, Region]
  
  ## Second, continue now starting from soil SS
  initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                     startingYear = startingYear,domSPrun=domSPrun)
  
  ###set parameters
  #    initPrebas$pCROBAS <- pCROBAS
  
  
  opsna <- which(is.na(initPrebas$multiInitVar))
  initPrebas$multiInitVar[opsna] <- 0.
  
  # initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
  # initSoil[,,1,,1] <- initSoilCstst[[r_no]]
  # initSoil <- aperm(initSoil,c(4,5,1:3))
  # initPrebas$soilC <- initSoil
  # if(exists("soilCststXX")) initPrebas$soilC[,1,,,] <- soilCststXX[[sampleID]]$soilC
  
  ##here mix years for weather inputs for Curr Climate
  if(rcpfile=="CurrClim"){
    set.seed(10)
    resampleYear <- sample(1:nYears,nYears)
    initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
    initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
    initPrebas$weather <- initPrebas$weather[,resampleYear,,]
    initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
  }
  
  
  # Loop management scenarios ------------------------------------------------
  harscen = harvestscenarios
  # print(date())
  # print(harscen)
  i = i + 1
  # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
  # harscen ="Base"
  ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
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
  }else{
    roundWood <- HarvLim1 * roundTotWoodRatio
    enWood <- HarvLim1 - roundWood
    HarvLim1 <- cbind(roundWood,enWood)
  }
  
  # initPrebas$energyCut <- rep(0.,length(initPrebas$energyCut))
  # HarvLim1 <- rep(0,2)
  # save(initPrebas,HarvLim1,file=paste0("test1",harscen,".rdata"))
  # region <- regionPrebas(initPrebas)
  region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1),minDharv = 1.)
  ets <- region$multiOut[,1,5,1,1]
  gpp <- apply(region$multiOut[,1,44,,1],1, sum)
  age <- region$multiOut[,1,7,1,1]
  siteID <- region$multiOut[,1,1,1,1]
  npp <- apply(region$multiOut[,1,18,,1],1, sum)
  nwL <- apply(region$multiOut[,1,26:27,,1],1, sum)
  fwL <- apply(region$multiOut[,1,28,,1],1, sum)
  cwL <- apply(region$multiOut[,1,29,,1],1, sum)
  gppGV <- region$GVout[,1,3]
  gvL <- region$GVout[,1,2]
  gpp2 <- apply(region$multiOut[,1,10,,1]*1000,1, sum)
  
  Xets <- region$multiOut[,1:10,5,1,1]
  Xgpp <- apply(region$multiOut[,1:10,44,,1],1:2, sum)
  Xage <- region$multiOut[,1:10,7,1,1]
  XsiteID <- region$multiOut[,1:10,1,1,1]
  Xnpp <- apply(region$multiOut[,1:10,18,,1],1:2, sum)
  XnwL <- apply(region$multiOut[,1:10,26:27,,1],1:2, sum)
  XfwL <- apply(region$multiOut[,1:10,28,,1],1:2, sum)
  XcwL <- apply(region$multiOut[,1:10,29,,1],1:2, sum)
  XgppGV <- region$GVout[,1:10,3]
  XgvL <- region$GVout[,1:10,2]
  Xgpp2 <- apply(region$multiOut[,1:10,10,,1]*1000,1:2, sum)

    dataX <- rbind(dataX,data.table(siteID= as.vector(siteID),
                      ETS=as.vector(ets),
                      GPP=as.vector(gpp),
                      GPP2=as.vector(gpp2),
                      region=as.vector(r_no),
                      age = as.vector(age),
                      gppGV = as.vector(gppGV),
                      gvL = as.vector(gvL),
                      siteID = as.vector(siteID),
                      npp = as.vector(npp),
                      nwL = as.vector(nwL),
                      fwL = as.vector(fwL),
                      cwL = as.vector(cwL)
                      ))
    XdataX <- rbind(XdataX,data.table(siteID= as.vector(XsiteID),
                                    ETS=as.vector(Xets),
                                    GPP=as.vector(Xgpp),
                                    GPP2=as.vector(Xgpp2),
                                    region=as.vector(r_no),
                                    age = as.vector(Xage),
                                    gppGV = as.vector(XgppGV),
                                    gvL = as.vector(XgvL),
                                    siteID = as.vector(XsiteID),
                                    npp = as.vector(Xnpp),
                                    nwL = as.vector(XnwL),
                                    fwL = as.vector(XfwL),
                                    cwL = as.vector(XcwL)
    ))
    print(r_no)
}

dataX[ETS<450, ETSgroup:=400]
dataX[ETS>=450 & ETS<550, ETSgroup:=500]
dataX[ETS>=550 & ETS<650, ETSgroup:=600]
dataX[ETS>=650 & ETS<750, ETSgroup:=700]
dataX[ETS>=750 & ETS<850, ETSgroup:=800]
dataX[ETS>=850 & ETS<950, ETSgroup:=900]
dataX[ETS>=950 & ETS<1050, ETSgroup:=1000]
dataX[ETS>=1050 & ETS<1150, ETSgroup:=1100]
dataX[ETS>=1150 & ETS<1250, ETSgroup:=1200]
dataX[ETS>=1250 & ETS<1350, ETSgroup:=1300]
dataX[ETS>=1350 & ETS<1450, ETSgroup:=1400]
dataX[ETS>=1450 & ETS<1550, ETSgroup:=1500]
dataX[ETS>=1550, ETSgroup:=1600]

XdataX[ETS<450, ETSgroup:=400]
XdataX[ETS>=450 & ETS<550, ETSgroup:=500]
XdataX[ETS>=550 & ETS<650, ETSgroup:=600]
XdataX[ETS>=650 & ETS<750, ETSgroup:=700]
XdataX[ETS>=750 & ETS<850, ETSgroup:=800]
XdataX[ETS>=850 & ETS<950, ETSgroup:=900]
XdataX[ETS>=950 & ETS<1050, ETSgroup:=1000]
XdataX[ETS>=1050 & ETS<1150, ETSgroup:=1100]
XdataX[ETS>=1150 & ETS<1250, ETSgroup:=1200]
XdataX[ETS>=1250 & ETS<1350, ETSgroup:=1300]
XdataX[ETS>=1350 & ETS<1450, ETSgroup:=1400]
XdataX[ETS>=1450 & ETS<1550, ETSgroup:=1500]
XdataX[ETS>=1550, ETSgroup:=1600]




dataXbyETS <- dataX[,mean(GPP),by=ETSgroup]
setnames(dataXbyETS,"V1","GPPtrees")
dataXbyETS$GPPtreesPreles <- dataX[,mean(GPP2),by=ETSgroup]$V1/3
dataXbyETS$gppGV <- dataX[,mean(gppGV),by=ETSgroup]$V1
dataXbyETS$gvL <- dataX[,mean(gvL),by=ETSgroup]$V1
dataXbyETS$NPPtrees <- dataX[,mean(npp,na.rm=T),by=ETSgroup]$V1
dataXbyETS$nwL <- dataX[,mean(nwL),by=ETSgroup]$V1
dataXbyETS$fwL <- dataX[,mean(fwL),by=ETSgroup]$V1
dataXbyETS$cwL <- dataX[,mean(cwL),by=ETSgroup]$V1
dataXbyETS$age <- dataX[,mean(age),by=ETSgroup]$V1
# write.csv(dataXbyETS,file="~/research/IBC-carbon/IBC-carbonByETS.csv")
save(dataXbyETS,dataX,file="dataGPPAnnikkicue.rdata")


XdataXbyETS <- XdataX[,mean(GPP),by=ETSgroup]
setnames(XdataXbyETS,"V1","GPPtrees")
XdataXbyETS$GPPtreesPreles <- XdataX[,mean(GPP2),by=ETSgroup]$V1/3
XdataXbyETS$gppGV <- XdataX[,mean(gppGV),by=ETSgroup]$V1
XdataXbyETS$gvL <- XdataX[,mean(gvL),by=ETSgroup]$V1
XdataXbyETS$NPPtrees <- XdataX[,mean(npp,na.rm=T),by=ETSgroup]$V1
XdataXbyETS$nwL <- XdataX[,mean(nwL),by=ETSgroup]$V1
XdataXbyETS$fwL <- XdataX[,mean(fwL),by=ETSgroup]$V1
XdataXbyETS$cwL <- XdataX[,mean(cwL),by=ETSgroup]$V1
XdataXbyETS$age <- XdataX[,mean(age),by=ETSgroup]$V1
# write.csv(XdataXbyETS,file="~/research/IBC-carbon/IBC-carbonByETS.csv")
save(XdataXbyETS,XdataX,file="XdataGPPAnnikkicue.rdata")



# library(ggplot2)
# library(data.table)
# ciao <- sample(1:nrow(dataX[GPP>0.]),20000)
# 
# xx <- lm(dataX[GPP>0.][ciao]$GPP ~dataX[GPP>0.][ciao]$ETS)
# summary(xx)
# 
# plotX <- ggplot(data=dataX[GPP>0.][ciao], aes(x=ETS, y=GPP,col=as.factor(region)))+#, fill=run,col=run)) +
#   # geom_bar(stat="identity", color="black", position=position_dodge()) +
#   # geom_smooth(method = "lm", fullrange = TRUE, se = TRUE) + 
#   geom_point() +
#   # geom_smooth(method = "lm", color = NA) #+
#   geom_abline(intercept = xx$coefficients[1],slope =xx$coefficients[2])
#   # scale_fill_manual(values=alpha(colX,.3))
# plotX
#   # load("C:/Users/checcomi/Documents/research/IBC-carbon/dataForAnnikkiCUE.rdata")
# save(plotX,dataX,file = "dataForAnnikkiCUE.rdata")
# 
# 
