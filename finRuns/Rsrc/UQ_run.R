# Uncertainty run settings for regional and segment level PREBAS-runs.
# In segment level runs, only no-harvest scenario is used in three regions. 
# In each region, The parameter and weather sets are the same in the xth 
# repetition. Local initial values are sampled independent of those.
# In segment level runs, the parameter, weather and initial value uncertainty
# samples are generated in baseline scenario run, and those are used also for 
# other harvest  scenarios.

#rm(list=ls())
#sampleID <- 4
rcpfile="CurrClim"
library(data.table)
library(devtools)
library(MASS)
library(stringr)
ststDeadW<-FALSE
#source("localSettings.r")

outType <- "uncRun" # Setting for the runModel-function
if(uncSeg) outType <- "uncSeg"

##### From GitHub
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
HcFactor <- 1
#source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")


nSitesRun <- nSitesRunr
print(paste("start region",r_no,"run",outType,"-",harvscen,"- set size",nSitesRun,"- no of repetitions", nSamplesr))

# Give new set of outputs ------------------------------------------------
if(uncSeg){
  varOuts <- c("NEP","DeadWoodVolume","soilC","V","D","BA","H") 
  # added; Wtot, age, Vdec, species, sitetype!
} else {
  varOuts <- c("NEP","V","npp","VroundWood","WroundWood",
               "grossGrowth","soilC") # Wtot!
}
#cS <- c(-100^2*44/12, 1, 1, 1) # multipliers of areas (&NEE C->CO2eq) for tot.sums

varSel <- match(varOuts,varNames)
funX <- rep("sum",length(varSel))
funX[match(varNames[c(7,11:12)],varNames[varSel])] <- "baWmean"
#----------------------------------------------------------------------------
parPath <- "/scratch/project_2000994/PREBASruns/metadata/paramUnc/"

set.seed(10)
if(uncRun){ # load distribution data
  
  if(loadUnc){
    if(!uncSeg){
      load(paste0("uncRuns/regRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
      sampleIDs <- 1:nSamplesr
      area_total <- sum(data.all$area)
      areas <- data.all$area
      areas <- areas/area_total
      #load(paste0("uncRuns/parids_reg",r_no,".rdata"))
    } else {
      load(paste0("uncRuns/parids_reg",r_no,"uncSeg.rdata"))
    }
  } else {
    parids <- sample(1:999,1000, replace = TRUE)
    if(!uncSeg){
      #save(parids, file=paste0("uncRuns/parids_reg",r_no,".rdata"))
    } else {
      save(parids, file=paste0("uncRuns/parids_reg",r_no,"uncSeg.rdata"))
    }
  }
  #pCROBASr <- list()
  if(uncPCrobas){
    #pCROBASr <- uncParCrobas(nSamples = nSamplesr)
    load(paste0(parPath,"pCROB_unc.rdata"))
    parindCrob <- parind
    pCrobdim <- nrow(pCROBbirch)
  }
  if(uncPPrel){
    load(paste0(parPath,"pPREL_unc.rdata"))
    parindPrel <- parind
    pCrobdim <- nrow(pPREL_unc)
  }
  if(uncPYas){
    data <- read.delim(paste0(parPath,"Yasso15.dat"), header = TRUE, sep="\t") 
    pYasdim <- length(data[[1]])
    pYas_unc <- matrix(0,pYasdim,35)
    for(ind in 1:pYasdim){ # read parameter values to matrix
      pYas_unc[ind,] <-as.numeric(unlist(str_split(data[[1]][[ind]], pattern = "  ")))[2:36]
    }
  }
} # if(uncRun){ # load distribution data
#----------------------------------------------------------------------------
# Generate nSamples sample initial value sets
if(!uncSeg & !loadUnc){ # sample pixel indices
  ops <- list()
  sampleIDs <- 1:nSamplesr
  area_total <- sum(data.all$area)
  areas <- data.all$area
  areas <- areas/area_total
  #print(paste0("Sample size ",nSitesRunr," pixels"))
  #if(!loadUnc){
  opsInd <- list() #matrix(0, nSitesRun, nSamples) 
  load(paste0("input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
  for(ij in 1:1000){ 
    opsInd[[ij]] <- sample(1:nrow(data.all), nSitesRunr, replace = TRUE, prob = areas)
    ops[[ij]] <- copy(data.all[opsInd[[ij]],])
    ops[[ij]] <- cbind(ops[[ij]],data.IDs[match(ops[[ij]]$segID, data.IDs$maakuntaID),4:5])
  }
  #} else {
  #  load(paste0("input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
  #  load(paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
  #  for(ij in 1:nSamplesr){ 
  #    ops[[ij]] <- copy(data.all[opsInd[[ij]],])
  #    ops[[ij]] <- cbind(ops[[ij]],data.IDs[match(ops[[ij]]$segID, data.IDs$maakuntaID),4:5])
  #  }
  #}
} else if(uncSeg){ # if(!uncSeg & !loadUnc)
  #setX=1
  if(!loadUnc){
    nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
    sampleIDs <- split(1:nSamples,             # Applying split() function
                       cut(seq_along(1:nSamples),
                           10,#nSetRuns,
                           labels = FALSE))[[setX]]
    ops_orig <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
    save(ops_orig,nSamples,sampleIDs,file=paste0("uncRuns/segRuns/opsdata",r_no,".rdata"))
  } else {
    load(paste0("uncRuns/segRuns/opsdata",r_no,".rdata"))
  }
  if(testRun){
    sampleIDs <- c(1:min(sampleID,nSamples))
  } else {
    sampleIDs <- c(1:nSamples)
  }
}

# Load peatland post-processing raster for uncRun and allocate emission factor vectors
if(uncRun & !uncSeg & uncPeat){
  soilSyke <- FALSE  ####If TRUE uses Syke peatland database if FALSE uses luke database
  # luke database pseudoptyp.img: Whole Finland, 100 = mineral soil, 400 = drained peatland, 700=other peatland, 0=non-forest
  # syke database peatSyke16res.tif: 1 = Undrained peatland; 2 = Drained peatland; 3 = Peat extraction area 
  if(soilSyke){
    finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/peatSyke16res.tif")
    drPeatID <- 2  ### ID = 2 for syke database
  }else{
    finPeats <- raster("/scratch/project_2000994/MVMIsegments/segment-IDs/pseudopty.img")
    drPeatID <- 400  ### ID = 400 for luke database; 
  }
  #Emission coefficients for peatland post-processing
  EC1 <- matrix(-240, 1, nSamplesr)#270  
  EC2 <- matrix(70, 1, nSamplesr)#70
}

if(uncRun & !loadUnc){
  resampleYears <-  t(matrix(1:nYears,nYears,1000))# save enought of random weather series
  if(uncClim){ # weather for iterations
    for(ij in 1000){
      resampleYears[ij,] <- sample(1:nYears,nYears,replace=T)
    }
    if(uncSeg) resampleYears1 <- resampleYears
  }
  if(uncSeg){
    save(resampleYears1, file=paste0("uncRuns/segRuns/resampleyears_reg",r_no,".rdata"))
  } else {
    save(resampleYears, file=paste0("uncRuns/regRuns/resampleyears_reg",r_no,".rdata"))
  }
} else {
  if(uncSeg){
    load(paste0("uncRuns/segRuns/resampleyears_reg",r_no,".rdata"))
  } else {
    load(paste0("uncRuns/regRuns/resampleyears_reg",r_no,".rdata"))
  }
}



if(uncRun){# sample model parameters, HcFactor and peatland emission coefficients
  pCROBASr <- list()
  pPRELr <- data.frame()
  pYASr <- data.frame()
  for(ij in 1:nSamplesr){ 
    pCROBASr[[ij]] <- pCROB
    if(uncPCrobas){
      pCROBASr[[ij]][parindCrob,1:3]<-t(rbind(pCROBpine[parids[ij],],pCROBspruce[parids[ij],],pCROBbirch[parids[ij],]))
    }
    pPRELr <- rbind(pPRELr, pPREL)
    if(uncPPrel){
      pPRELr[ij,parindPrel] <- pPREL_unc[parids[ij],]
    }
    if(uncPYas){
      pYASr <- rbind(pYASr, pYas_unc[parids[ij],])
    } else {
      pYASr <- rbind(pYASr, pYAS)
    }
    
    if(uncPeat & !uncSeg){  
      EC1[ij] <- -240 + 70*rnorm(1)#270  
      EC2[ij] <- 70 + 30*rnorm(1)#70
    }
  }
  
  if(!uncSeg & !loadUnc){ # if region level uncertainty run, sample input variables
    # sample input values for the samples
    if(uncInput){
      print(paste0("input uncertainties for ",length(sampleIDs),"..."))
      ops <-  uncVariables(ops=copy(ops), sampleIDs = sampleIDs) 
      print("... done.")
    }
    if(uncHcFactor){
      HcFactorr <- 1 + rHcFactor*rnorm(length(sampleIDs))
    } else {
      HcFactorr <- matrix(1,1,length(sampleIDs))
    }
    save(opsInd,parids,ops,EC1,EC2,HcFactorr,file=paste0("uncRuns/regRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
  } #if(!uncSeg & !loadUnc) 
}

# load weather data  
rcpfile = rcps
if(rcpfile=="CurrClim"){
  load(paste(climatepath, rcpfile,".rdata", sep=""))  
  maxRday <- max(dat$rday)
  xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
  dat = rbind(dat,dat,dat)
  dat[,rday:=xday]
}else{
  load(paste(climatepath, rcpfile,".rdata", sep=""))  
}
##
if(!uncSeg){
  niter <- ceiling(nSamplesr/nParRuns)
} else {
  niter <- nSamplesr
}
sampleOutput <- list()

if(uncSeg){
  pCROBASrseg <- copy(pCROBASr)
  pPRELrseg <- copy(pPRELr)
  pYASrseg <- copy(pYASr)
}

#if(loadUnc){ # if needed to load previous sample
#  load(paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
#}
for(nii in 1:niter){
  toMem <- ls()
  startRun <- Sys.time() 
  print(paste0("Start running iter ",nii,"/",niter,"..."))
  if(uncSeg){ # load random input data
    resampleYears<-matrix(resampleYears1[nii,], nrow= tail(sampleIDs,n=1), 
                          ncol=length(resampleYears1[nii,]), byrow=TRUE)
    
    ops <- copy(ops_orig)
    if(uncInput){
      print(paste0("input uncertainties for ",length(sampleIDs),"..."))
      ops <-  uncVariables(ops=ops, sampleIDs = sampleIDs)
      print("... done.")
    }
    if(uncHcFactor){
      HcFactorr <- (1 + rHcFactor*rnorm(1))*matrix(1,1,length(sampleIDs))#length(sampleIDs))
    } else {
      HcFactorr <- matrix(1,1,length(sampleIDs))
    }
    for(ij in sampleIDs){ 
      pCROBASr[[ij]] <- pCROBASrseg[[nii]]
      pPRELr[ij,] <- pPRELrseg[nii,]
      pYASr[ij,] <- pYASrseg[nii,]
    }
    
  }
  set.seed(.Random.seed[r_no])
  
  #sampleXs <- lapply(sampleIDs[1:3], function(jx) { runModel(jx, outType=outType)})      
  #sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):(nii*nParRuns)], function(jx) {
  print("start runModel")
  if(testRun){ # if needed to test an individual sample
    sampleXs <- lapply(sampleIDs, function(jx) { runModel(jx, outType=outType)})
    #sampleXs <- runModel(sampleIDs,outType=outType)
    print(sampleXs)
  } else if(uncSeg){
    sampleXs <- mclapply(sampleIDs, function(jx) {
      runModel(jx, outType=outType)}, 
      mc.cores = nCores,mc.silent=FALSE)      ## Split this job across 10 cores
  } else {
    sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):(nii*nParRuns)], function(jx) {
      runModel(jx, outType=outType)}, 
      mc.cores = nCores,mc.silent=FALSE)      ## Split this job across 10 cores
  }
  timeRun <- Sys.time() - startRun
  
  if(!uncSeg){
    print(sampleXs[[1]])
    m <- ncol(sampleXs[[1]])
    n <- length(sampleXs)
    varNams <- names(sampleXs[[1]])
    #varNams <-  sampleXs[[1]][,"vari"]
    # g /m2 /year -> -44/12*16^2/10^12
    for(j in 1:m){
      x <- data.frame()
      for(k in 1:n){
        x <- rbind(x, t(sampleXs[[k]][,j,with=FALSE]))
        #rownames(x)[k] <- paste0(varNams[j],k)
      }
      #names(x) <- sampleXs[[1]]$periods
      #x[,3:5] <- x[,3:5]*cS[j]
      if(nii == 1){
        sampleOutput[[j]] <- x
      } else {
        sampleOutput[[j]] <- rbind(sampleOutput[[j]], x)
      }
    }
    
    save(area_total,sampleOutput,file=paste0("uncRuns/regRuns/samplexout_reg",r_no,
                                             "_",harvscen,"_",                                    
                                             "samplesize",nSitesRunr,"_iters",nSamplesr,
                                             "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                             "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
    
    #print("make histograms...")
    print("make plots")
    m <- length(sampleOutput)
    print(paste(m,"variables"))
    n <- nrow(sampleOutput[[1]])
    pdf(file = paste0("uncRuns/regRuns/plots_regionID",r_no,"_",varNams,
                      "_",nSitesRunr,"_",harvscen,"_",
                      "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                      "_Cr",uncClim,"_str",uncSiteType,
                      "_uncPeat",uncPeat,".pdf"))
    
    for(indj in 1:m){
      x <- sampleOutput[[indj]]
      varNams <- rownames(x)[1]
      xnas <- which(is.na(x[,1]))
      x <- x[which(!is.na(x[,1])),]
      #png(file = paste0("uncRuns/plots_regionID",r_no,"_",varNams,
      #                  "_",nSitesRunr,"_",harvscen,"_",
      #                  "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
      #                  "_Cr",uncClim,"_str",uncSiteType,
      #                  "_uncPeat",uncPeat,".png"))
      time <- 2015 + 1:ncol(x)
      for(iter in 1:n){
        if(iter==1){
          plot(time,x[iter,], ylab = rownames(x)[1], xlab = "year")
        } else{
          points(time,x[iter,])
        }
      }
    }
    dev.off()
    print("plots made")
  } else { # if uncSeg
    n <- length(sampleXs)
    varNams <- names(sampleXs[[1]])
    for(j in 1:length(varNams)){
      x <- data.frame()
      for(k in 1:n){
        x <- rbind(x, sampleXs[[k]][[j]])
        #rownames(x)[k] <- paste0(varNams[j],k)
      }
      if(nii==1){
        sampleOutput[[j]] <- x
        names(sampleOutput)[j]<-varNams[j]
      } else {
        sampleOutput[[j]] <- cbind(sampleOutput[[j]], x[,2:4])
      }
    }
    save(sampleOutput,file=paste0("uncRuns/segRuns/samplexout_uncSeg_reg",r_no,
                                  "_iters",nSamplesr,"_",harvscen,                                    
                                  #"_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                  #"_Cr",uncClim,"_str",uncSiteType,
                                  ".rdata")) 
    
  }
  print(paste0("Run time for ",length(sampleIDs)," samples of size ", nSitesRunr," = ",timeRun))
  print("End running...")
}

setwd("Rsrc/virpiSbatch/")


