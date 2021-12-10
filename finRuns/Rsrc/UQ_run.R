rm(list=ls())
sampleID <- 4
rcpfile="CurrClim"
library(data.table)
library(devtools)
library(MASS)

ststDeadW<-FALSE
source("localSettings.r")

UQanalysis <- "True"

#devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/sampleRun.r")
##### From GitHub

devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
#source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")

for(r_no in r_nos){
  regions_no <- r_no
  source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
  nSitesRun <- nSitesRunr
  print(paste("start region",r_no,"- set size",nSitesRun,"- no of samples", nSamplesr))
  
  # Give new set of outputs ------------------------------------------------
  varOuts <- c("NEP","V","npp","VroundWood") # Wtot!
  #cS <- c(-100^2*44/12, 1, 1, 1) # multipliers of areas (&NEE C->CO2eq) for tot.sums
  
  varSel <- match(varOuts,varNames)
  funX <- rep("sum",length(varSel))
  funX[match(varNames[c(7,11:12)],varNames[varSel])] <- "baWmean"
  #----------------------------------------------------------------------------
  
  set.seed(10)
  if(uncInput){
    load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/inputUncer.rdata"))
    C <- chol(errData$all$sigmaFSVda)
    X <- cbind(data.all$ba, data.all$dbh, data.all$h, data.all$pine, data.all$spruce, data.all$birch)
    mx <- ncol(X)
  }
  
  #----------------------------------------------------------------------------
  if(uncRun){ 
    sampleIDs <- 1:nSamplesr
    area_total <- sum(data.all$area)
    areas <- data.all$area
    areas <- areas/area_total
    print(paste0("Sample size ",nSitesRunr," pixels"))
    if(!loadUnc){
      opsInd <- list() #matrix(0, nSitesRun, nSamples) 
      pCROBASr <- list()
      inputr <- list()
      str <- list()
      resampleYears <-  t(matrix(1:nYears,nYears,nSamplesr))
      for(ij in 1:nSamplesr){ 
        #opsInd[,ij] <- sample(1:nrow(data.all), nSitesRun, replace = FALSE, prob = areas)
        #opsInd[,ij] <- sample(1:nrow(data.all), nSitesRun, replace = TRUE, prob = areas)
        opsInd[[ij]] <- sample(1:nrow(data.all), nSitesRunr, replace = TRUE, prob = areas)
        if(uncPCrobas){
          load("input/pCROBASr.rdata")
        } else {
          pCROBASr[[ij]] <- pCROB
        }
        if(uncInput){
          Y <- X[opsInd[[ij]],] + matrix(rnorm(nSitesRunr*mx),nSitesRunr,mx)%*%C
          inputr[[ij]] <- distr_correction(Y,X[opsInd[[ij]],])
        } else {inputr[[ij]] <- X}
        if(uncSiteType){
          ###load the fittet probit models to estimate the Site fertility class
          load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
          ####generate sample input data
          dataSample <- data.table(st=data.all$fert[opsInd[[ij]]],
                                   H=inputr[[ij]][,3],
                                   D=inputr[[ij]][,2],
                                   BAtot=inputr[[ij]][,1],
                                   BApPer=inputr[[ij]][,4]
          )
          #X <- cbind(data.all$ba, data.all$dbh, data.all$h, data.all$pine, data.all$spruce, data.all$birch)
          ##select the model (all is the most generic)
          modX <- step.probit[["all"]]
          ###run model -> returns the probability for each site type so you can sample using that probability
          ###model inputs:
          # st = site type
          # H = average height
          # D = average dbh
          #BAtot = total basal area
          #BApPer = % of pine basal area
          str[[ij]] <- max.col(predict(modX,type='p',dataSample))
        } else { str[[ij]] <- data.all$fert[opsInd[[ij]]]}
        if(uncClim){
          resampleYears[ij,] <- sample(1:nYears,nYears,replace=T)
        }
      }
      save(opsInd,pCROBASr,inputr,resampleYears,str,file=paste0("uncRuns/opsInd_reg",r_no,".rdata")) 
    }
  } else { # if(uncRun)
    setX=1
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    sampleIDs <- split(1:nSamples,             # Applying split() function
                       cut(seq_along(1:nSamples),
                           nSetRuns,
                           labels = FALSE))[[setX]]
    ops <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
  }
  
  rcpfile = rcps
  # for(rcpfile in rcps) { ## ---------------------------------------------
  if(rcpfile=="CurrClim"){
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
    #####process data considering only current climate###
    maxRday <- max(dat$rday)
    xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
    dat = rbind(dat,dat,dat)
    dat[,rday:=xday]
  }else{
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
  }
  ##
  niter <- ceiling(nSamplesr/nParRuns)
  
  sampleOutput <- list()
  
  
  if(testRun){
    if(loadUnc){
      load(paste0("uncRuns/opsInd_reg",r_no,".rdata")) 
    }
    toMem <- ls()
    startRun <- Sys.time() 
    sampleX <- runModel(sampleID,sampleRun=F, uncRun = uncRun)
  } else {
    for(nii in 1:niter){
      toMem <- ls()
      print(paste0("Start running iter ",nii,"/",niter,"..."))
      startRun <- Sys.time() 
      #sampleX <- runModel(sampleID,sampleRun=F, uncRun = uncRun)
      # #sampleXs <- lapply(sampleIDs[1:4], function(jx) {
      #  runModel(jx, uncRun = TRUE, ststDeadW=FALSE)})      
      sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):(nii*nParRuns)], function(jx) {
            runModel(jx, uncRun = uncRun)}, 
            mc.cores = nCores,mc.silent=FALSE)      ## Split this job across 10 cores
      timeRun <- Sys.time() - startRun
      print(paste0("Run time for ",nParRuns," samples of size ", nSitesRunr," = ",timeRun))
      print("End running...")
    
      m <- nrow(sampleXs[[1]])
      n <- length(sampleXs)
      varNams <-  sampleXs[[1]][,"vari"]
      # g /m2 /year -> -44/12*16^2/10^12
      for(j in 1:m){
        x <- data.frame()
        for(k in 1:n){
          x <- rbind(x, sampleXs[[k]][j,])
        }
        #x[,3:5] <- x[,3:5]*cS[j]
        if(nii == 1){
          sampleOutput[[j]] <- x
        } else {
          sampleOutput[[j]] <- rbind(sampleOutput[[j]], x)
        }
      }
    
      save(sampleOutput,file=paste0("uncRuns/samplexout",r_no,
              "samplesize",nSitesRunr,
              "_pr",uncPcrobas,"_Xr",uncInput,
              "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
    }

    print("make histograms...")
    m <- length(sampleOutput)
    n <- nrow(sampleOutput[[1]])
    units_hist <- c(-area_total*100^2*44/12*10^-12,1,1,area_total*10^-6)
    units_hist_label <- c("NEE [Tg CO2eq]","V [m3 ha-1]",
                        "npp [gC m-2]","VroundWood [10^6 m3]") 

    for(indj in 1:m){
      x <- sampleOutput[[indj]]
      varNams <- x[1,"vari"]
      xnas <- which(is.na(x[,3]))
      x <- x[which(!is.na(x[,3])),]
      x[,3:5] <- x[,3:5]*units_hist[indj]
      png(file = paste0("uncRuns/hists_regionID",r_no,"_",varNams,
                        "_",nSitesRunr,
                        "_pr",uncPcrobas,"_Xr",uncInput,
                        "_Cr",uncClim,"_str",uncSiteType,".png"))
      xlims <- c(min(x[,3:5]),max(x[,3:5]))
      xlims[1] <- xlims[1]*(1-0.1*sign(xlims[1]))
      xlims[2] <- xlims[2]*(1+0.1*sign(xlims[2]))
      par(mfrow=c(3,1))
      for(per in 1:3){
        if(per==1 & length(xnas)>0){
          hist(as.matrix(x[, paste0("per", per), with = FALSE]),
             main = paste0("period",per," nas: sampleIDs ",xnas), 
             xlab = units_hist_label[indj],
             xlim = xlims)  
        } else {
          hist(as.matrix(x[, paste0("per", per), with = FALSE]),
            main = paste0("period",per), 
            xlab = units_hist_label[indj],
            xlim = xlims)  
        }
      }
      dev.off()
      #print(colMeans(x[,3:5]))
    }
    print("histograms made")
  }
  setwd("Rsrc/virpiSbatch/")
  
}