

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
                       modOut,sampleID=1,finPeats=finPeats,sampleX=sampleX,EC1=-240,EC2=70){
  nYears <-  max(modOut$nYears)
  nSites <-  max(modOut$nSites)
  nVarSel <- length(varSel)
  varsX <- rep(NA,(nVarSel+5))
  xx <- matrix(NA,(nVarSel+5),nYears)
  for (ij in 1:nVarSel) {
    # print(varSel[ij])
    if(funX[ij]=="baWmean"){
      outX <- colMeans(baWmean(modOut,varSel[ij]))
    }
    if(funX[ij]=="sum"){
      outX <- colMeans(apply(modOut$multiOut[,,varSel[ij],,1],1:2,sum))
    }
    ####test plot
    # print(outX)
    #outX <- c(mean(outX[simYear1]),mean(outX[simYear2]),mean(outX[simYear3]))
    # names(outX) <- paste0("p",1:3)
    varsX[ij] <- varNames[varSel[ij]]
    xx[ij,1:nYears] <- outX
    # assign(varNames[varSel[ij]],outX)
  }
  
  ####process and save special variables: 
  ###age
  outX <- colMeans(modOut$multiOut[,1:nYears,7,1,1])
  #  outX <- c(mean(modOut$multiOut[,simYear1,7,1,1]),
  #    mean(modOut$multiOut[,simYear2,7,1,1]),
  #    mean(modOut$multiOut[,simYear3,7,1,1]))
  varsX[(nVarSel+1)] <- "age"
  xx[(nVarSel+1),1:nYears] <- outX
  # save(domAge,file=paste0("outputDT/forCent",r_no,"/domAge_",
  #                         harscen,"_",rcpfile,"_",
  #                         "sampleID",sampleID,".rdata"))
  ####VenergyWood
  outX <- colMeans(apply(modOut$multiEnergyWood[,,,1],1:2,sum))
  # outX <- c(mean(outX[simYear1]),mean(outX[simYear2]),mean(outX[simYear3]))
  xx[(nVarSel+2),1:nYears] <- outX
  varsX[(nVarSel+2)] <- "VenergyWood"
  # names(outX) <- paste0("p",1:3)
  # VenergyWood <- outX
  # save(VenergyWood,file=paste0("outputDT/forCent",r_no,
  #                              "/VenergyWood_",harscen,"_",rcpfile,"_",
  #                              "sampleID",sampleID,".rdata"))
  ####GVbiomass
  outX <- colMeans(modOut$GVout[,,4])
  #outX <- c(mean(outX[simYear1]),
  #     mean(outX[simYear2]),
  #     mean(outX[simYear3]))
  xx[(nVarSel+3),1:nYears] <- outX
  varsX[(nVarSel+3)] <- "wGV"
  # names(GVw) <- paste0("p",1:3)
  # save(GVgpp,file=paste0("outputDT/forCent",r_no,
  #                        "/GVgpp_",harscen,"_",rcpfile,"_",
  #                        "sampleID",sampleID,".rdata"))
  ####Wtot trees
  outX <- apply(modOut$multiOut[,,c(24,25,31,32,33),,1],1:2,sum)
  #nas <- length(which(is.na(rowSums(outX))))
  #outX <- colMeans(outX[which(!is.na(rowSums(outX))),])
  outX <- colMeans(outX)
  xx[(nVarSel+4),1:nYears] <- outX
  varsX[(nVarSel+4)] <- "Wtot"
  
  outX <- colMeans(apply(modOut$multiEnergyWood[,,,2],1:2,sum))
  #print(outX)
  xx[(nVarSel+5),1:nYears] <- outX
  varsX[(nVarSel+5)] <- "Wenergywood"
  
  # save(Wtot,file=paste0("outputDT/forCent",r_no,"/Wtot_",
  #                       harscen,"_",rcpfile,"_",
  #                       "sampleID",sampleID,".rdata"))
  # rm(domSpecies,domAge,Vdec,WenergyWood,Wtot,pX,p1,p2,p3); gc()
  # if(sampleID==sampleForPlots){dev.off()}
  
  if("NEP" %in% varsX){
    print(paste0("peatland postprocessing ", sampleID))
    #### Peatland post-processing
    coords <- cbind(sampleX$x, sampleX$y)
    marginX= 1:2#(length(dim(out$annual[,,varSel,]))-1)
    
    pX <- data.table(segID=sampleX$segID,apply(modOut$multiOut[,,"npp",,1],marginX,sum))
    #p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
    #p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
    #p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
    #pX <- data.table(p1,p2[,2],p3[,2]) # can be the same segment multiple times
    assign("NPP",pX)
    
    pX <- data.table(data.table(segID=sampleX$segID,apply(modOut$multiOut[,,"NEP",,1],marginX,sum)))
    #p1 <- outX[, .(per1 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut1, by = segID] 
    #p2 <- outX[, .(per2 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut2, by = segID] 
    #p3 <- outX[, .(per3 = rowMeans(.SD,na.rm=T)), .SDcols = colsOut3, by = segID] 
    #pX <- data.table(p1,p2[,2],p3[,2]) # can be the same segment multiple times
    assign("NEP",pX)
    
    ##!!###step to reduce the size of the peat raster 
    ###load npp first outside loop to get peatX
    peatX <- extract(finPeats, coords)
    ##!!## end
    
    ###load site type raster
    fert<-modOut$multiOut[,1,"sitetype",1,1]
    
    N2O <- data.frame()
    CH4 <- data.frame()
    #####Loop along periods
    for(curr in 2:(nYears+1)) {
      #curr <- paste0("per",i)
      npp <- NPP[,..curr]
      nep <- NEP[,..curr]
      
      nep = processPeatUQ(peatX,fert,npp,nep,drPeatID,1,EC1,EC2)
      nep = processPeatUQ(peatX,fert,npp,nep,drPeatID,2,EC1,EC2)
      NEP[,curr] <- nep
      N2O[,curr] <- processPeatUQ_N2O_CH4(peatX, fert, peatval, type = "N2O")
      CH4[,curr] <- processPeatUQ_N2O_CH4(peatX, fert, peatval, type = "CH4")
    }  
    NEP <- colMeans(NEP)
    N2O <- colMeans(N2O)
    CH4 <- colMeans(CH4)
    #xx[which(varsX == "NEP"),] <- NEP[2:(nYears+1)]
    xx[(nVarSel+6),1:nYears] <- NEP
    varsX[(nVarSel+6)] <- "NEPprocPeat [g C m−2 year−1]"
    xx[(nVarSel+7),1:nYears] <- N2O
    varsX[(nVarSel+7)] <- "N2O [g N2O m−2 year−1]"
    xx[(nVarSel+8),1:nYears] <- CH4
    varsX[(nVarSel+8)] <- "CH4 [g CH4 m−2 year−1]"
    
  }
  
  outX <- data.table(t(xx))
  names(outX) <- varsX
  print(outX[1,])
  #outX[,periods:=paste0('p',1:3)]
  
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
  p1 <- outX[,.(per1 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut1,by=segID]
  p2 <- outX[,.(per2 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut2,by=segID]
  p3 <- outX[,.(per3 = Mode(as.numeric(.SD))[1]),.SDcols=colsOut3,by=segID]
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


processPeatUQ <- function(peatXf, fertf, nppf, nepf, peatval, fertval, EC1, EC2) {
  # peatXf = raster with peat soils
  # fertf =  soilType
  # nppf = npp
  # nepf= nep
  # peatval = ID to identify the drained peatlands -> tells which peat soil you want to treat
  # fertval = soilType ID -> tells which siteType you want to treat
  
  if(fertval==1){
    drPeatNeg <- peatXf == peatval & fertf <= 3  ###selecting the pixels that match the conditions of peat and siteType
  } else if (fertval==2){
    drPeatNeg <- peatXf == peatval & fertf > 3  ###selecting the pixels that match the conditions of peat and siteType
  }
  drPeatNeg[drPeatNeg==0] <- NA  ### assign NA to the remaining pixels
  drPeat <- nppf[drPeatNeg]  ###raster with only the pixel of interest
  
  ###calculate the new NEP according to the siteType (fertval)
  #if (fertval <= 3) {         
  if(fertval == 1){
    drPeat <- drPeat + EC1*12/44#-240 #g C m-2 year-1  
  } else if(fertval == 2){
    #} else if (fertval > 3) {
    drPeat <- drPeat + EC2*12/44#70
  }
  #}
  nepf[drPeatNeg] <- drPeat
  return(nepf)#merge(drPeat,nepf))
}

processPeatUQ_N2O_CH4 <- function(peatXf, fertf, peatval, type = "N2O") {
  
  drPeatInd <- peatXf == peatval # & fertf == fertval  ###selecting the pixels that match the conditions of peat and siteType
  emission <- 0*drPeatInd  ### zero vector  
  ###calculate the new NEP according to the siteType (fertval)
  if(type == "N2O"){
    emission[which(drPeatInd & fertf<=3)] <- 0.23 + 0.04*rnorm(1) #g N2O m−2 year−1
    emission[which(drPeatInd & fertf>3)] <- 0.077 + 0.004*rnorm(1) #g N2O m−2 year−1
    emission[drPeatInd == 0] <- 0
  } elseif (type == "CH4"){
    emission[drPeatInd > 0] <- 0.34 ± 0.12 rnorm(1) #g CH4 m-2 year-1
    emission[drPeatInd == 0] <- 0
  }
  
  return(emission)#merge(drPeat,nepf))
}
