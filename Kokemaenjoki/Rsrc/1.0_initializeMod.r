# load("/scratch/project_2000994/PREBASruns/resDE_n4_pMAP.rdata")

# setwd("C:/Users/minunno/Documents/research/ibc-carbon/EvoRun")
setwd(pathtoken)

data.all <- fread("kokeInputs")
setnames(data.all,"consArea","cons")
cloudpixels = data.all[, sum(ba==32766)]
nonforest = data.all[, sum(ba==32767)]
forest = data.all[, sum(ba< 32766)]
AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
AREA_1000ha = AREA / 10000 / 1000
data.all[,area:=N*16^2/10000]
areaTot <- sum(data.all$N)
## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
data.all = data.all[ba < 32766]
data.all = data.all[!is.na(cons)]

# data.shape <- readOGR("C:/Users/minunno/Documents/research/ibc-carbon/EVOarea/hyperlentoehdotus.shp")
# # data.shape <- readOGR("/wrk/mpeltoni/DONOTREMOVE/Ilmastopaneeli_toprocess/codeshare/EVOarea/hyperlentoehdotus.shp")
# 
# data.luke <- fread("lukeInputs/data.proc.2.txt")
# 
# coordinates(data.luke) = c("x", "y")
# crs(data.luke) <- data.shape@proj4string
# evo_subset <- data.luke[data.shape, ]
# dim(evo_subset)
# data.evo <- as.data.table(evo_subset)

###how to extract data from raster
# ciao <- raster("ba2.tif")
# r2 <- crop(ciao, extent(data.shape))
# r3 <- mask(r2, data.shape)

# data.evo <- fread("/wrk/mpeltoni/DONOTREMOVE/Ilmastopaneeli_toprocess/codeshare/EVOarea/input_evo.txt")


# model for Hc  estimation
HcMod <- function(H,D,N,BA,V,Age){
  b0 <- -3.2697
  bH <- 0.4125
  bD <- 0.3769
  bH.D <- -0.7335
  bN <- 0.1117
  bBA <- -0.1052
  bV <- 0.4796
  bAge <- 0.1568
  stand <- -3.14E-12
  plotx <- -7.07E-13
  Cb = exp(b0 + bH*log(H)+ bD *log(D) + bH.D *(H/D) +
             bN * log(N)+ bBA * log(BA) + bV * log(V) +  bAge * log(Age) +stand+plotx)
  
  return(pmin(Cb,0.2*H))
}

sample_data.f = function(data.all, nSample) {
  cloudpixels = data.all[, sum(ba==32766)]
  nonforest = data.all[, sum(ba==32767)]
  forest = data.all[, sum(ba< 32766)]
  AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
  AREA_1000ha = AREA / 10000 / 1000
  
  ## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
  data.all = data.all[ba < 32766]
  data.all = data.all[!is.na(cons)]
  
  ## REDUCE SAMPLE FOR TESTING ---------------------------------------
  smp = floor(seq(1, dim(data.all)[1], len=nSample))
  data.sample = data.all[smp]
  
  # summary(data.sample[, 3:11])
  
  for (col in colnames(data.sample)[c(3, 5:11)]) set(data.sample, j=col,
                                                     value=as.double(data.sample[[col]]))
  
  ## -----------------------------------------------------------------
  
  
  ## AVOID ZERO CASES
  
  data.sample$dbh = as.double(data.sample$dbh)
  
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert ==1, decid:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert <= 3 & fert > 1, spruce:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert >= 4, pine:=1  ]
  siteX <- union(which(data.sample$ba <=0.041),which(data.sample$h<= 15))
  siteX <- union(siteX,which(data.sample$dbh<=0.5))
  data.sample$nTree <- data.sample$ba/(pi/4*( data.sample$dbh/100)^2)
  siteNN <- which(data.sample$nTree>5000)
  siteX <- union(siteX,siteNN)
  data.sample[siteX,h:=15]
  data.sample[siteX,dbh:=0.5]
  data.sample[siteX,ba:=0.0431969]
  data.sample
}


# StartingYear = climate data that detrermines simulation period must have year greater than this.
create_prebas_input.f = function(r_no, clim, data.sample, nYears, startingYear=0,domSPrun=0) { # dat = climscendataset
  #domSPrun=0 initialize model for mixed forests according to data inputs 
  #domSPrun=1 initialize model only for dominant species 
  nSites <- nrow(data.sample)
  
  ###site Info matrix. nrow = nSites, cols: 1 = siteID; 2 = climID; 3=site type;
  ###4 = nLayers; 5 = nSpecies;
  ###6=SWinit;   7 = CWinit; 8 = SOGinit; 9 = Sinit
  
  siteInfo <- matrix(c(NA,NA,NA,160,0,0,20,3,3,413,0.45,0.118),nSites,12,byrow = T)
  #siteInfo <- matrix(c(NA,NA,NA,3,3,160,0,0,20),nSites,9,byrow = T)
  siteInfo[,1] <- 1:nSites
  siteInfo[,2] <- as.numeric(data.sample[,id])
  siteInfo[,3] <- data.sample[,fert]
  
  litterSize <- matrix(0,3,3)
  litterSize[1,1:2] <- 30
  litterSize[1,3] <- 10
  litterSize[2,] <- 2
  
  ###Initialise model
  # initVardension nSites,variables, nLayers
  # variables: 1 = species; 2 = Age; 3 = H; 4=dbh; 5 = ba; 6 = Hc
  initVar <- array(NA, dim=c(nSites,7,3))
  data.sample[,baP:= (ba * pine/(pine+spruce+decid))]
  data.sample[,baSP:= (ba * spruce/(pine+spruce+decid))]
  data.sample[,baB:= (ba * decid/(pine+spruce+decid))]
  data.sample[,dbhP:= dbh]
  data.sample[,dbhSP:= dbh]
  data.sample[,h:= h/10]
  data.sample[,hP:= h]
  data.sample[,hSP:= h]
  
  data.sample[,N:=ba/(pi*(dbh/2)^2/10000)]
  
  areas <- data.sample$area
  
  initVar[,1,] <- as.numeric(rep(1:3,each=nSites))
  initVar[,2,] <- round(as.numeric(data.sample[,age]))
  initVar[,3,] <- as.numeric(data.sample[,h])
  # initVar[,3,][which(initVar[,3,]<1.5)] <- 1.5  ####if H < 1.5 set to 1.5
  
  initVar[,4,] <- as.numeric(data.sample[,dbh])
  if(domSPrun==1){
    ##initialize model only for dominant species##
    initVar[,5,] = 0.
    ix = unlist(data.sample[, which.max(c(pine, spruce, decid)), by=1:nrow(data.sample)] [, 2])
    for(jx in 1:nSites) initVar[jx,5,ix[jx]] = as.numeric(data.sample[, ba])[jx]
  } else{
    ###initialize model for mixed forest runs
    initVar[,5,1] <- as.numeric(data.sample[,(ba * pine/(pine+spruce+decid))])
    initVar[,5,2] <- as.numeric(data.sample[,(ba * spruce/(pine+spruce+decid))])
    initVar[,5,3] <- as.numeric(data.sample[,(ba * decid/(pine+spruce+decid))])
    ####increase spruce dbh 10% for spruce sitetype 1:2
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,X:=(ba-1.1*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,dbhP:=X*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,dbhSP:=1.1*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5  & baSP > baP & dbhP<0.5,dbhSP:=((ba-(0.5/dbh)*baP-baB)/baSP)*dbh]
    data.sample[pine>0. & spruce >0. & fert<2.5  & baSP > baP & dbhP<0.5,dbhP:=0.5]
  
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhSP:=dbh * (ba - 0.9*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,dbhP:=pmax(0.9*dbh,0.3)]
    
    ####increase spruce h 10% for spruce sitetype 1:2
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,X:=(ba-1.1*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,hP:=X*h]   
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP,hSP:=1.1*h]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP & hP<1.5,hSP:=((ba-(1.5/h)*baP-baB)/baSP)*h]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP > baP & hP<1.5,hP:=1.5]
    
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hSP:=h * (ba - 0.9*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert<2.5 & baSP <= baP,hP:=pmax(0.9*h,1.3)]
    
    ####increase spruce dbh 5% for spruce sitetype 3
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,X:=(ba-1.05*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,dbhP:=X*dbh]   
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP,dbhSP:=1.05*dbh]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP & dbhP<0.5,dbhSP:=((ba-(0.5/dbh)*baP-baB)/baSP)*dbh]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP > baP & dbhP<0.5,dbhP:=0.5]
    
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhSP:=dbh * (ba - 0.95*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,dbhP:=pmax(0.95*dbh,0.3)]
    
    ####increase spruce h 5% for spruce sitetype 3
    data.sample[pine>0. & spruce >0. & fert==3,X:=(ba-1.05*baSP-baB)/baP]
    data.sample[pine>0. & spruce >0. & fert==3,hP:=X*h]  
    data.sample[pine>0. & spruce >0. & fert==3,hSP:=1.05*h]  
    data.sample[pine>0. & spruce >0. & fert==3 & hP<1.5,hSP:=((ba-(1.5/h)*baP-baB)/baSP)*h]
    data.sample[pine>0. & spruce >0. & fert==3 & hP<1.5,hP:=1.5]
    
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hSP:=h * (ba - 0.95*baP - baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert==3 & baSP <= baP,hP:=pmax(0.95*h,1.3)]
    
    ####increase pine dbh 10% for sitetype >= 4
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,X:=(ba-1.1*baP-baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhSP:=X*dbh]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhP:=1.1*dbh]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & dbhSP<0.5,dbhP:=((ba-(0.5/dbh)*baSP-baB)/baP)*dbh]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & dbhSP<0.5,dbhSP:=0.5]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhP:=dbh * (ba - 0.9*baSP - baB)/baP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,dbhSP:=pmax(0.9*dbh,0.3)]
    ####increase pine h 10% for sitetype >= 4
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,X:=(ba-1.1*baP-baB)/baSP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hSP:=X*h]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hP:=1.1*h]   
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & hSP<1.5,hP:=((ba-(1.5/h)*baSP-baB)/baP)*h]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP & hSP<1.5,hSP:=1.5]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hP:=h * (ba - 0.9*baSP - baB)/baP]
    data.sample[pine>0. & spruce >0. & fert>3.5 & baP > baSP,hSP:=pmax(0.9*h,1.3)]
    initVar[,3,1] <- as.numeric(data.sample[,hP])
    initVar[,3,2] <- as.numeric(data.sample[,hSP])
    initVar[,4,1] <- as.numeric(data.sample[,dbhP])
    initVar[,4,2] <- as.numeric(data.sample[,dbhSP])
    
  }
  
  # initVar[,6,] <- as.numeric(data.sample[,hc])
  
  ###check which BA ==0. and set to 0 the rest of the variables
  NoPine <- which(initVar[,5,1]==0.)
  NoSpruce <- which(initVar[,5,2]==0.)
  NoDecid <- which(initVar[,5,3]==0.)
  
  siteInfo[NoPine,8] <- siteInfo[NoPine,8] - 1
  siteInfo[NoSpruce,8] <- siteInfo[NoSpruce,8] - 1
  siteInfo[NoDecid,8] <- siteInfo[NoDecid,8] - 1
  
  #siteInfo[NoPine,4] <- siteInfo[NoPine,4] - 1
  #siteInfo[NoSpruce,4] <- siteInfo[NoSpruce,4] - 1
  #siteInfo[NoDecid,4] <- siteInfo[NoDecid,4] - 1
  initVar[NoPine,3:6,1] <- 0.
  initVar[NoSpruce,3:6,2] <- 0.
  initVar[NoDecid,3:6,3] <- 0.
  initVar[NoSpruce,,2] <- initVar[NoSpruce,,3]
  initVar[NoPine,,1:2] <- initVar[NoPine,,2:3]
  
  nLay1 <- which(siteInfo[,8]==1)
  nLay2 <- which(siteInfo[,8]==2)
  initVar[nLay1,3:6,2:3] <- 0
  initVar[nLay2,3:6,3] <- 0
  # initVar[which(initVar[,5,1]==0.),,1] <- initVar[which(initVar[,5,1]==0.),,2]
  # initVar[which(initVar[,5,1]==0.),,2] <- initVar[which(initVar[,5,1]==0.),,3]
  # initVar[which(initVar[,5,1]==0.),1,3] <- 1
  # initVar[which(initVar[,5,1]==0.),3:6,3] <- 0
  
  if (FALSE) {
    dat = dat[id %in% data.sample[, unique(id)]]
    
    if(rcps!= "CurrClim.rdata"){
      # dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
      # dat[, DOY:= as.numeric(format(pvm, "%j"))]
      dat[, Year:= as.numeric(floor(rday/366)+1971)]
      dat = dat[Year >= startingYear]
      dat[DOY==366, DOY:=365]
    }
    PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
                       value.var="PAR")[, -1])
    TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
                        value.var="TAir")[, -1])
    VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
                       value.var="VPD")[, -1])
    Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
                          value.var="Precip")[, -1])
    CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
                       value.var="CO2")[, -1])
  }
  siteInfo[, 2]  = match(as.numeric(siteInfo[, 2]), as.numeric(rownames(clim[[1]])))
  # siteInfo[, 2]  = match(siteInfo[,2], unique(dat$id))
  
  defaultThin=as.numeric(1-data.sample[, cons])
  energyCut <- ClCut <- as.numeric(1-data.sample[, cons])
  ## Set to match climate data years
  initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),siteInfo=siteInfo,
                              litterSize = litterSize,#pAWEN = parsAWEN,
                              defaultThin=defaultThin,
                              ClCut = ClCut, areas =areas,
                              energyCut = energyCut, 
                              multiInitVar = as.array(initVar),
                              PAR = clim$PAR[, 1:(nYears*365)],
                              TAir=clim$TAir[, 1:(nYears*365)],
                              VPD=clim$VPD[, 1:(nYears*365)],
                              Precip=clim$Precip[, 1:(nYears*365)],
                              CO2=clim$CO2[, 1:(nYears*365)],
                              yassoRun = 1)
  initPrebas
}

yasso.mean.climate.f = function(dat, data.sample, startingYear, nYears){
  dat = dat[id %in% data.sample[, unique(id)]]
  dat[, DOY:=rep(1:365, len=dim(dat)[1])]
  dat[, Year:=rep(1980:2099, each=365)]
  #dat[, Year:= as.numeric(format(pvm, "%Y"))]
  dat = dat[Year >= startingYear & Year <= startingYear+nYears]
  dat[, pvm:= as.Date(paste(Year, '-01-01', sep="")) - 1 + DOY ]
  #dat[, DOY:= as.numeric(format(pvm, "%j"))]
  dat[, Mon:= as.numeric(format(pvm, "%m"))]
  #dat[DOY==366, DOY:=365]
  Tmean = dat[, mean(TAir), by = Year]
  Tsum = dat[, sum(ifelse(TAir>5, TAir-5, 0)), by=.(id, Year)][, mean(V1), by=Year]
  PAR = dat[, mean(PAR), by = Year]
  VPD = dat[, mean(VPD), by = Year]
  CO2 = dat[, mean(CO2), by = Year]
  Precip = dat[, sum(Precip), by = .(id, Year)][, mean(V1), by=Year]
  Tampl = dat[, .(mean(TAir)), by = .(id, Year, Mon)][, (max(V1)-min(V1))/2, by=Year]
  
  out = cbind(Tmean, Precip[, -1], Tampl[, -1], CO2[, -1], PAR[, -1], VPD[, -1], Tsum[, -1])
  colnames(out) = c('Year','Tmean','Precip','Tampl', 'CO2', "PAR", "VPD", "Tsum5")
  out
}


prep.climate.f = function(dat, data.sample, startingYear, nYears){
  dat = dat[id %in% data.sample[, unique(id)]]
  # if(rcps== "CurrClim.rdata"){
  #   dat[, Year:= as.numeric(floor(rday/366)+1971)]
  #   dat = dat[Year >= startingYear]
  #   
  # }else{
  dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
  dat[, DOY:= as.numeric(format(pvm, "%j"))]
  dat[, Year:= as.numeric(format(pvm, "%Y"))]
  dat = dat[Year >= startingYear]
  dat[DOY==366, DOY:=365]
  # }
  id = dat[,unique(id)]
  PARtran = t( dcast(dat[, list(id, rday, PAR)], rday ~ id,
                     value.var="PAR")[, -1])
  TAirtran = t( dcast(dat[, list(id, rday, TAir)], rday ~ id,
                      value.var="TAir")[, -1])
  VPDtran = t( dcast(dat[, list(id, rday, VPD)], rday ~ id,
                     value.var="VPD")[, -1])
  Preciptran = t( dcast(dat[, list(id, rday, Precip)], rday ~ id,
                        value.var="Precip")[, -1])
  CO2tran = t( dcast(dat[, list(id, rday, CO2)], rday ~ id,
                     value.var="CO2")[, -1])
  list(PAR=PARtran, TAir=TAirtran, VPD=VPDtran, 
       Precip=Preciptran, CO2=CO2tran,id=id)
}


simSummary.f = function(region=region, r_no, nYears, startingYear, rcpfile, harscen) {
  
  out = region[['multiOut']]
  VOL = out[, , 30, , 1]
  VOL = apply(VOL, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  VOL = apply(VOL, 2, mean)
  ## Multiply by area (tha)
  VOL_INAREA = VOL * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  ## at the beginning 207.7 mill m3, vrt 189.9 according to NFI (for region 7 = Keski-Suomi)
  
  Vmort = out[, , 42, , 1]
  Vmort = apply(Vmort, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  Vmort = apply(Vmort, 2, mean)
  Vmort_INAREA = Vmort * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  
  ## WHY THIS IS NOT THE SAME AS har?
  Vharvested = out[, , 37, , 1]
  Vharvested = apply(Vharvested, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  Vharvested = apply(Vharvested, 2, mean)
  Vharvested_INAREA = Vharvested * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  grossgrowth = out[, , 43, , 1]
  grossgrowth = apply(grossgrowth, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  grossgrowth = apply(grossgrowth, 2, mean)
  grossgrowth_INAREA = grossgrowth * nfiareas[ID==r_no, AREA] * 1000 / 1000000 ## mill m3
  
  dbh = out[, , 12, , 1]
  dbh = apply(dbh, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  dbh = apply(dbh, 2, mean)
  
  age = out[, , 7, , 1]
  age = apply(age, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  age = apply(age, 2, mean)
  
  gpp = out[, , 10, , 1]
  gpp = apply(gpp, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  gpp = apply(gpp, 2, mean)
  #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
  
  
  npp = out[, , 18, , 1]
  npp = apply(npp, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  npp = apply(npp, 2, mean)
  #npp_INAREA = npp * nfiareas[ID==7, AREA] * 1000 / 1000000 ## mill m3
  
  
  nep = out[, , 46, , 1]
  nep = apply(nep, c(1,2), sum, na.rm=TRUE)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  nep = apply(nep, 2, mean)
  
  
  B_tree = out[, , 35, , 1]
  B_tree = apply(B_tree, c(1,2), sum)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  B_tree = apply(B_tree, 2, mean)
  
  lproj = out[, , 21, , 1]
  lproj = apply(lproj, c(1,2), mean)
  ## SO THIS IS NOW MEAN VOL PER HA OF nSample SIMULATED SAMPLES (by YEAR):
  lproj = apply(lproj, 2, mean)
  data.table(r_no, rcpfile, harscen, year=startingYear + (1:nYears),
             VOL, VOL_INAREA, Vharvested, Vmort, Vmort_INAREA,
             grossgrowth_INAREA, dbh, age, gpp, npp, nep, B_tree, lproj)
}

