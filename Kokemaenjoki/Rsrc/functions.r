## ---------------------------------------------------------------------
## FUNCTIONS
## ---------------------------------------------------------------------
sample_data.f = function(data.all, nSample) {
  cloudpixels = data.all[, sum(ba==32766)]
  nonforest = data.all[, sum(ba==32767)]
  forest = data.all[, sum(ba< 32766)]
  AREA = (forest + cloudpixels) * 16 * 16 * 1000 #m2
  AREA_1000ha = AREA / 10000 / 1000
  
  ## REMOVE CLOUD COVERED, AND WHERE cons = NA (...? why)
  data.all = data.all[ba < 32766]
  data.all = data.all[!is.na(cons)]
  data.all = data.all[landclass == 1]
  
  ## REDUCE SAMPLE FOR TESTING ---------------------------------------
  smp = floor(seq(1, dim(data.all)[1], len=nSample))
  data.sample = data.all[smp]
  
  # summary(data.sample[, 3:11])
  
  for (col in colnames(data.sample)[c(3, 5:11)]) set(data.sample, j=col,
                                                     value=as.double(data.sample[[col]]))
  
  ## -----------------------------------------------------------------
  
  
  ## AVOID ZERO CASES
  
  data.sample$dbh = as.double(data.sample$dbh)
  data.sample[ba <=0.041, ba:=0.041]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert ==1, decid:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert <= 3 & fert > 1, spruce:=1  ]
  data.sample[pine == 0 & spruce == 0 & decid ==0 & fert >= 4, pine:=1  ]
  data.sample[ h<= 15, h:=15]
  data.sample[dbh<=0.5, dbh:=0.5]
  data.sample
}

# StartingYear = climate data that detrermines simulation period must have year greater than this.
create_prebas_input.f = function(r_no, clim, data.sample, nYears, startingYear=0) { # dat = climscendataset
  
  nSites <- nrow(data.sample)
  
  ###site Info matrix. nrow = nSites, cols: 1 = siteID; 2 = climID; 3=site type;
  ###4 = nLayers; 5 = nSpecies;
  ###6=SWinit;   7 = CWinit; 8 = SOGinit; 9 = Sinit
  
  siteInfo <- matrix(c(NA,NA,NA,160,0,0,20,3,3),nSites,9,byrow = T)
  #siteInfo <- matrix(c(NA,NA,NA,3,3,160,0,0,20),nSites,9,byrow = T)
  siteInfo[,1] <- 1:nSites
  siteInfo[,2] <- data.sample[,id]
  siteInfo[,3] <- data.sample[,fert]
  
  litterSize <- matrix(0,3,3)
  litterSize[1,1:2] <- 30
  litterSize[1,3] <- 10
  litterSize[2,] <- 2
  
  ###Initialise model
  # initVardension nSites,variables, nLayers
  # variables: 1 = species; 2 = Age; 3 = H; 4=dbh; 5 = ba; 6 = Hc
  initVar <- array(NA, dim=c(nSites,6,3))
  data.sample[,baP:= (ba * pine/(pine+spruce+decid))]
  data.sample[,h:= h/10]
  data.sample[,N:=ba/(pi*(dbh/2)^2/10000)]
  data.sample[,VforHc:= N*(pi*(dbh/2)^2/10000)*h/3]
  data.sample[,hc:= pmax(0,HcMod(h,dbh,N,ba,VforHc,age))]
  
  initVar[,1,] <- as.numeric(rep(1:3,each=nSites))
  initVar[,2,] <- as.numeric(data.sample[,age])
  initVar[,3,] <- as.numeric(data.sample[,h])
  initVar[,3,][which(initVar[,3,]<1.5)] <- 1.5  ####if H < 1.5 set to 1.5
  
  initVar[,4,] <- as.numeric(data.sample[,dbh])
  # initVar[,5,1] <- as.numeric(data.sample[,(ba * pine/(pine+spruce+decid))])
  # initVar[,5,2] <- as.numeric(data.sample[,(ba * spruce/(pine+spruce+decid))])
  # initVar[,5,3] <- as.numeric(data.sample[,(ba * decid/(pine+spruce+decid))])
  initVar[,5,] = 0.
  ix = unlist(data.sample[, which.max(c(pine, spruce, decid)), by=1:nrow(data.sample)] [, 2])
  for(jx in 1:nSites) initVar[jx,5,ix[jx]] = as.numeric(data.sample[, ba])[jx]
  initVar[,6,] <- as.numeric(data.sample[,hc])
  
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
  
  # initVar[which(initVar[,5,1]==0.),,1] <- initVar[which(initVar[,5,1]==0.),,2]
  # initVar[which(initVar[,5,1]==0.),,2] <- initVar[which(initVar[,5,1]==0.),,3]
  # initVar[which(initVar[,5,1]==0.),1,3] <- 1
  # initVar[which(initVar[,5,1]==0.),3:6,3] <- 0
  
  if (FALSE) {
    dat = dat[id %in% data.sample[, unique(id)]]
    
    
    dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
    dat[, DOY:= as.numeric(format(pvm, "%j"))]
    dat[, Year:= as.numeric(format(pvm, "%Y"))]
    dat = dat[Year >= startingYear]
    dat[DOY==366, DOY:=365]
    
    
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
  siteInfo[, 2]  = match(siteInfo[, 2], as.numeric(rownames(clim[[1]])))
  
  defaultThin=as.numeric(1-data.sample[, cons])
  ClCut = as.numeric(1-data.sample[, cons])
  ## Set to match climate data years
  
  initPrebas <- InitMultiSite(nYearsMS = rep(nYears,nSites),siteInfo=siteInfo,
                              litterSize = litterSize,#pAWEN = parsAWEN,
                              defaultThin=defaultThin,
                              ClCut = ClCut,
                              multiInitVar = as.array(initVar),
                              PAR = clim[[1]][, 1:(nYears*365)],
                              TAir=clim[[2]][, 1:(nYears*365)],
                              VPD=clim[[3]][, 1:(nYears*365)],
                              Precip=clim[[4]][, 1:(nYears*365)],
                              CO2=clim[[5]][, 1:(nYears*365)],
                              yassoRun = 1,lukeRuns = 1.)
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
  dat[, pvm:= as.Date('1980-01-01') - 1 + rday ]
  dat[, DOY:= as.numeric(format(pvm, "%j"))]
  dat[, Mon:= as.numeric(format(pvm, "%m"))]
  dat[, Year:= as.numeric(format(pvm, "%Y"))]
  dat = dat[Year >= startingYear & Year <= startingYear+nYears]
  dat[DOY==366, DOY:=365]
  
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
  list(PARtran, TAirtran, VPDtran, Preciptran, CO2tran)
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
