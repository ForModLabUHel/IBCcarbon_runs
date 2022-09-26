

distr_correction <- function(Y,Xtrue,Xdiscr=FALSE){
  # y is the matrix of new sample - observations in lines, variables in columns
  # x is the matrix of the measured values = truth
  X <- Xtrue[sample(1:nrow(Xtrue),nrow(Xtrue),replace = TRUE),]
  m <- ncol(X)
  n <- nrow(X)
  ny <- nrow(Y)
  
  # Create cdf for each marginal distr. and use it to generate 
  # ny random variables from that distribution:
  Ynew <- matrix(0,nrow=ny,ncol=m)
  
  # Do post-processing variable by variable
  for(j in 1:m){
    xu <- unique(X[,j]) # choose the unique values in sample
    xu <- xu[order(xu)] # and sort them from smallest to biggest
    # create empirical cdf:
    cdf <- matrix(0,length(xu),1) # empty matrix
    for(i in 1:length(xu)){
      cdf[i] <- sum(X[,j] <= xu[i]) # count the sample values less than 
      # given value,
    }
    cdf <- cdf/(n+1) # cdf goes from 0 to 1, here are the inner points
    
    r <- matrix(ny,1)
    for(i in 1:nrow(Y)){
      r[i] <- sum(Y[,j]<=Y[i,j])/ny # Define the eCDF of each observation 
    }
    # use discr. or cont. inverse cdf to generate new sample
    if(Xdiscr[j] | length(Xdiscr)<m){ # discrete values 
      #interp <- approx(c(0,cdf), c(xu,max(xu)+1), r, method = "constant", yleft = xu[1], 
      #                 yright = xu[length(xu)], rule = 2:1)
      interp <- approx(c(0,cdf,1), c(min(xu),xu,max(xu)+1), r, method = "constant", yleft = xu[1], 
                       yright = xu[length(xu)], rule = 2:1)
    } else { # continuous values 
      #interp <- approx(c(0,cdf), c(xu,max(xu)+1), r, yleft = xu[1], 
      #                 yright = xu[length(xu)], rule = 2:1)
      interp <- approx(c(0,cdf,1), c(min(xu),xu,max(xu)*1.01), r, yleft = xu[1], 
                       yright = xu[length(xu)], rule = 2:1)
    }
    Ynew[,j] <- interp$y
  }
  return(Ynew)
}


UncOutProc <- function(varSel=c(46,39,30,37), funX=rep("sum",4),
                       modOut,sampleID=1,finPeats=finPeats,
                       sampleX=sampleX,vname = "", evalSegs = 0){
  if(length(evalSegs)==1) evalSegs <- 1:nrow(modOut$multiOut)
  nYears <-  max(modOut$nYears)
  nSites <-  length(evalSegs)
  marginX= 1:2
  #print(nSites)
  nVarSel <- length(varSel)
  varsX <- rep(NA,(nVarSel+7))
  xx <- matrix(NA,(nVarSel+7),nYears)

  for (ij in 1:nVarSel) {
    if(funXX[ij]=="baWmean"){
      outX <- data.table(segID=sampleX$segID,baWmean(modOut,varSel[ij]))
      #outX <- baWmean(modOut,varSel[ij])
    }
    if(funXX[ij]=="sum"){
      #outX <- apply(modOut$multiOut[evalSegs,,varSel[ij],,1],1:2,sum)
      outX <- data.table(segID=sampleX$segID,apply(modOut$multiOut[,,varSel[ij],,1],marginX,sum))
    }
    outX <- colMeans(outX,na.rm = TRUE)
    varsX[ij] <- paste0(vname,varNames[varSel[ij]])
    xx[ij,1:nYears] <- outX[-1]
    # assign(varNames[varSel[ij]],outX)
  }
  
  ####process and save special variables: 
  ###age
  outX <- modOut$multiOut[evalSegs,1:nYears,7,1,1]
  outX <- colMeans(outX,na.rm = TRUE)
  varsX[(nVarSel+1)] <- paste0(vname,"age")
  xx[(nVarSel+1),1:nYears] <- outX
  ####VenergyWood
  outX <- apply(modOut$multiEnergyWood[evalSegs,,,1],1:2,sum)
  outX <- colMeans(outX,na.rm = TRUE)
  xx[(nVarSel+2),1:nYears] <- outX
  varsX[(nVarSel+2)] <- paste0(vname,"VenergyWood")
  ####GVbiomass
  outX <- modOut$GVout[evalSegs,,4]
  outX <- colMeans(outX,na.rm = TRUE)
  xx[(nVarSel+3),1:nYears] <- outX
  varsX[(nVarSel+3)] <- paste0(vname,"wGV")
  ####Wtot trees
  outX <- apply(modOut$multiOut[evalSegs,,c(24,25,31,32,33),,1],1:2,sum)
  outX <- colMeans(outX,na.rm = TRUE)
  xx[(nVarSel+4),1:nYears] <- outX
  varsX[(nVarSel+4)] <- paste0(vname,"Wtot")
  ####WenergyWood
  outX <- apply(modOut$multiEnergyWood[evalSegs,,,2],1:2,sum)
  outX <- colMeans(outX,na.rm = TRUE)
  xx[(nVarSel+5),1:nYears] <- outX
  varsX[(nVarSel+5)] <- paste0(vname,"Wenergywood")

  #print(paste0("peatland CH4 and N2O postprocessing sampleID", sampleID))
  #### Peatland post-processing
  #marginX= 1:2#(length(dim(out$annual[,,varSel,]))-1)
  fert<-sampleX$fert[evalSegs] #modOut$multiOut[,1,"sitetype",1,1]
  ECN2O1 <- 0.23 + 0.04*rnorm(1) #g N2O m−2 year−1
  ECN2O2 <- 0.077 + 0.004*rnorm(1) #g N2O m−2 year−1
  ECCH4 <- 0.34 + 0.12 * rnorm(1)
  N2O <- data.frame(matrix(0,nrow = nSites, ncol = 1))
  CH4 <- data.frame(matrix(0,nrow = nSites, ncol = 1))
  # estimate N2O and CHe for the first year pixels 
  N2O[,1] <- processPeatUQ_N2O_CH4(sampleX$peatID[evalSegs], fert, drPeatID, type = "N2O", 
                                            ECN2O1 = ECN2O1, ECN2O2 = ECN2O2)
  CH4[,1] <- processPeatUQ_N2O_CH4(sampleX$peatID[evalSegs], fert, drPeatID, type = "CH4",
                                            ECCH4 = ECCH4)
  N2O <- colMeans(N2O)
  CH4 <- colMeans(CH4)
  # N2O and CH4 emissions remain constant for the whole simulation period
  xx[(nVarSel+6),1:nYears] <- N2O
  varsX[(nVarSel+6)] <- paste0(vname,"N2O")
  xx[(nVarSel+7),1:nYears] <- CH4
  varsX[(nVarSel+7)] <- paste0(vname,"CH4")

  # all results as output    
  outX <- data.table(t(xx))
  names(outX) <- varsX
  return(outX)
} 

UncOutProcSeg <- function(varSel=c(46,39,30,37), funX=rep("sum",4),
                          modOut,sampleX,colsOut1,colsOut2,colsOut3){
  nYears <-  max(modOut$nYears)
  nSites <-  max(modOut$nSites)
  nVarSel <- length(varSel)
  marginX=1:2
  varsX <- list()
  for (ij in 1:length(varSel)) {
    # print(varSel[ij])
    if(funX[ij]=="baWmean"){
      outX <- data.table(segID=sampleX$segID,baWmean(modOut,varSel[ij]))
    }
    if(funX[ij]=="sum"){
      outX <- data.table(segID=sampleX$segID,apply(modOut$multiOut[,,varSel[ij],,1],marginX,sum))
    }
    ####test plot
    # print(outX)
    p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
    p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
    p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
    
    pX <- data.table(p1,p2[,2],p3[,2]) # can be the same segment multiple times
    
    varsX[[ij]] <- pX
    
  }
  
  ##process and save special variables: 
  ###age dominant species
  outX <- domFun(modOut,varX="age")
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  varsX[[(ij+1)]] <- pX
  
  # species
  outX <- domFun(modOut,varX="species")  
  ####test plot
  #if(sampleID==sampleForPlots){testPlot(outX,"domSpecies",areas)}
  ###take the most frequent species in the periods
  p1 <- outX[,.(per1 = Mode(as.numeric(.SD),na.rm=T)[1]),.SDcols=colsOut1,by=segID]
  p2 <- outX[,.(per2 = Mode(as.numeric(.SD),na.rm=T)[1]),.SDcols=colsOut2,by=segID]
  p3 <- outX[,.(per3 = Mode(as.numeric(.SD),na.rm=T)[1]),.SDcols=colsOut3,by=segID]
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  varsX[[(ij+2)]] <- pX
  
  ###deciduous Volume Vdec
  outX <- vDecFun(modOut)
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  varsX[[(ij+3)]] <- pX
  
  ####GVw
  outX <- data.table(segID=sampleX$segID,modOut$GVout[,,4])
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  varsX[[(ij+4)]] <- pX
  
  ####Wtot
  outX <- data.table(segID=sampleX$segID,apply(modOut$multiOut[,,c(24,25,31,32,33),,1],1:2,sum))
  p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
  p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
  p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
  pX <- merge(p1,p2)
  pX <- merge(pX,p3)
  varsX[[(ij+5)]] <- pX
  
  pX <- cbind(sampleX$segID, sampleX$fert)
  varsX[[(ij+6)]] <- pX
  
  outX <- varsX
  names(outX) <- c(varNames[varSel],"domage","domspecies","Vdec","GVw","Wtot","fert")
  return(outX)
} 

uncVariables <- function(ops = ops, sampleIDs = sampleIDs, rage = 0.1,
                         uncInput = TRUE, uncSiteType = TRUE, uncAge = TRUE){
  if(uncInput){ # Input uncertainty covariance matrix
    load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/inputUncer.rdata"))
    CovX <- errData$all$sigmaFSVda
    C <- chol(CovX)
  }
  if(uncSiteType){
    ###load the fittet probit models to estimate the Site fertility class
    load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
  }
  
  for(ij in sampleIDs){ 
    if(uncInput){
      X <- copy(ops[[ij]])
      X <- cbind(X$ba, X$dbh, X$h/10, X$pine, X$spruce, X$birch) # h as decimeters in data.all -> convert to meters as in cov matrix C
      mx <- ncol(X)
      Y <- X + matrix(rnorm(nrow(X)*mx),nrow(X),mx)%*%C
      Y <- distr_correction(Y,X)
      ops[[ij]][,':=' (ba=Y[,1],dbh=Y[,2],h=Y[,3]*10,pine=Y[,4],
                       spruce=Y[,5],birch=Y[,6])] # the height converted back to meters
    } 
    if(uncSiteType){
      ###load the fittet probit models to estimate the Site fertility class
      #load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
      ####generate sample input data
      dataSample <- data.table(st=ops[[ij]]$fert,
                               H=ops[[ij]]$h,
                               D=ops[[ij]]$dbh,
                               BAtot=ops[[ij]]$ba,
                               BApPer=ops[[ij]]$pine)
      modX <- step.probit[["all"]]
      ###run model -> returns the probability for each site type so you can sample using that probability
      ###model inputs:
      # st = site type
      # H = average height
      # D = average dbh
      #BAtot = total basal area
      #BApPer = % of pine basal area
      probs <- predict(modX,type='p',dataSample)
      str <- matrix(0,nrow(ops[[ij]]),1)
      for(ri in 1:nrow(ops[[ij]])){
        str[ri] <- sample(1:5,1,prob = probs[ri,])
      }
      ops[[ij]][,fert:=str]
    } 
    if(uncAge){
      ops[[ij]][,age:=ops[[ij]]$age*(1+rage*rnorm(nrow(ops[[ij]])))]
    }
  } # end for(ij in sampleIDs)
  return(ops)
}


processPeatUQ <- function(peatXf, fertf, nppf, nepf, littersumf, peatval, fertval, EC1, EC2) {
  # peatXf = raster with peat soils
  # fertf =  soilType
  # nppf = npp
  # nepf= nep
  # peatval = ID to identify the drained peatlands -> tells which peat soil you want to treat
  # fertval = soilType ID -> tells which siteType you want to treat
  
  if(fertval==1){
    drPeatNeg <- which(peatXf == peatval & fertf <= 3)  ###selecting the pixels that match the conditions of peat and siteType
  } else if (fertval==2){
    drPeatNeg <- which(peatXf == peatval & fertf > 3)  ###selecting the pixels that match the conditions of peat and siteType
  }
  drPeat <- nppf[drPeatNeg,]  ###raster with only the pixel of interest
  #print(paste0("Fraction of peatland pixels", fertval," = ",nrow(drPeat)/nrow(nppf)))
  ###calculate the new NEP according to the siteType (fertval)
  if(fertval == 1){
    drPeat <- drPeat + EC1*12/44 - littersumf[drPeatNeg,]/10#-240 #g C m-2 year-1  
  } else if(fertval == 2){
    drPeat <- drPeat + EC2*12/44 - littersumf[drPeatNeg,]/10#70
  }
  nepf[drPeatNeg] <- drPeat
  return(nepf)#merge(drPeat,nepf))
}

processPeatUQ_N2O_CH4 <- function(peatXf, fertf, peatval, type = "N2O", 
                                  ECN2O1 = 0.23, ECN2O2 = 0.077, ECCH4 = 0.34) {
  
  drPeatInd <- peatXf == peatval # & fertf == fertval  ###selecting the pixels that match the conditions of peat and siteType
  emission <- matrix(0,nrow = length(fertf),1) ### zero vector  
  ###calculate the new NEP according to the siteType (fertval)
  if(type == "N2O"){
    emission[which(drPeatInd & fertf<=3)] <- ECN2O1 #g N2O m−2 year−1
    emission[which(drPeatInd & fertf>3)] <- ECN2O2 #g N2O m−2 year−1
    emission[drPeatInd == 0] <- 0
  } else if (type == "CH4"){
    emission[drPeatInd > 0] <- ECCH4 #g CH4 m-2 year-1
    emission[drPeatInd == 0] <- 0
  }
  
  return(emission)#merge(drPeat,nepf))
}
