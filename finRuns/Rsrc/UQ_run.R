rm(list=ls())
sampleID <- 4
rcpfile="CurrClim"
library(data.table)
library(devtools)
library(MASS)

ststDeadW<-FALSE
source("localSettings.r")

outType <- "uncRun" # Setting for the runModel-function
if(uncSeg) outType <- "uncSeg"

##### From GitHub
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
#source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
source_url("https://raw.githubusercontent.com/virpi-j/IBCcarbon_runs/master/general/functions.r")

nSitesRun <- nSitesRunr
print(paste("start region",r_no,"run",outType,"- set size",nSitesRun,"- no of repetitions", nSamplesr))

# Give new set of outputs ------------------------------------------------
varOuts <- c("NEP","V","npp","VroundWood","WroundWood",
             "grossGrowth") # Wtot!
#cS <- c(-100^2*44/12, 1, 1, 1) # multipliers of areas (&NEE C->CO2eq) for tot.sums

varSel <- match(varOuts,varNames)
funX <- rep("sum",length(varSel))
funX[match(varNames[c(7,11:12)],varNames[varSel])] <- "baWmean"
#----------------------------------------------------------------------------

set.seed(10)
if(uncInput){ # Input uncertainty covariance matrix
  load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/inputUncer.rdata"))
  C <- chol(errData$all$sigmaFSVda)
}

#----------------------------------------------------------------------------
if(!uncSeg){ # sample pixel indices
  ops <- list()
  sampleIDs <- 1:nSamplesr
  #Emission coefficients for peatland post-processing
  EC1 <- matrix(-240, 1, nSamplesr)#270  
  EC2 <- matrix(70, 1, nSamplesr)#70
  
  area_total <- sum(data.all$area)
  areas <- data.all$area
  areas <- areas/area_total
  #print(paste0("Sample size ",nSitesRunr," pixels"))
  if(!loadUnc){
    opsInd <- list() #matrix(0, nSitesRun, nSamples) 
    load(paste0("input/maakunta/maakunta_",r_no,"_IDsTab.rdata"))
    for(ij in 1:nSamplesr){ 
      opsInd[[ij]] <- sample(1:nrow(data.all), nSitesRunr, replace = TRUE, prob = areas)
      ops[[ij]] <- data.all[opsInd[[ij]],]
      ops[[ij]] <- cbind(ops[[ij]],data.IDs[match(ops[[ij]]$segID, data.IDs$maakuntaID),4:5])
    }
  }
} else { # if(uncRun)
  #setX=1
  nSamples <- ceiling(dim(data.all)[1]/nSitesRun)
  sampleIDs <- split(1:nSamples,             # Applying split() function
                     cut(seq_along(1:nSamples),
                         10,#nSetRuns,
                         labels = FALSE))[[setX]]
  ops_orig <- split(data.all, sample(1:nSamples, nrow(data.all), replace=T))
}

# Load peatland post-processing raster for uncRun
if(uncRun){
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
}


if(uncRun & !loadUnc){
  pCROBASr <- list()
  if(uncPCrobas){
    load("input/pCROBASr.rdata")
    pdim <- nrow(pCROBASr[[1]])
  }
  if(uncSiteType){
    ###load the fittet probit models to estimate the Site fertility class
    load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
  }
  resampleYears <-  t(matrix(1:nYears,nYears,nSamplesr))
  if(uncClim){ # weather for iterations
    for(ij in nSamplesr){
      resampleYears[ij,] <- sample(1:nYears,nYears,replace=T)
    }
    if(uncSeg) resampleYears1 <- resampleYears
  }
  for(ij in sampleIDs){ 
    if(uncPCrobas){
      pCROBASr[[ij]] <- rbind(pCROBASr[[ij]],pCROB[(pdim+1):(pdim+3),])
    }else {
      pCROBASr[[ij]] <- pCROB
    }
    if(!uncSeg){
      if(uncInput){
        X <- copy(ops[[ij]])
        X <- cbind(X$ba, X$dbh, X$h/10, X$pine, X$spruce, X$birch) # h as decimeters in data.all -> convert to meters as in cov matrix C
        mx <- ncol(X)
        Y <- X + matrix(rnorm(nrow(X)*mx),nrow(X),mx)%*%C
        X <- distr_correction(Y,X)
        ops[[ij]][,':=' (ba=X[,1],dbh=X[,2],h=X[,3]*10,pine=X[,4],spruce=X[,5],birch=X[,6])] # the height converted back to meters
      } 
      if(uncSiteType){
        ###load the fittet probit models to estimate the Site fertility class
        #load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
        ####generate sample input data
        dataSample <- data.table(st=ops[[ij]]$fert,
                                 H=ops[[ij]]$h,
                                 D=ops[[ij]]$dbh,
                                 BAtot=ops[[ij]]$ba,
                                 BApPer=ops[[ij]]$pine
        )
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
    }
  }
  if(!uncSeg){
    save(opsInd,pCROBASr,ops,resampleYears,file=paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
  }
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

if(testRun){ # if needed to test an individual sample
  if(loadUnc){ # if needed to load previous sample
    load(paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
  }
  toMem <- ls()
  startRun <- Sys.time() 
  sampleX <- runModel(sampleID,outType=outType)
} else {
  if(loadUnc){ # if needed to load previous sample
    load(paste0("uncRuns/opsInd_reg",r_no,"_uncSeg",uncSeg,".rdata")) 
  }
  for(nii in 1:niter){
    toMem <- ls()
    print(paste0("Start running iter ",nii,"/",niter,"..."))
    if(uncSeg){
      resampleYear<-matrix(resampleYears1[nii,], nrow=length(sampleIDs), 
                           ncol=length(resampleYears1[nii,]), byrow=TRUE)
      
      ops <- copy(ops_orig)
      for(ij in sampleIDs){ 
        if(uncInput){
          Xt <- ops[[ij]]
          Xt <- cbind(Xt$ba, Xt$dbh, Xt$h/10, Xt$pine, Xt$spruce, Xt$birch) # h as decimeters in data.all -> convert to meters as in cov matrix C
          mx <- ncol(Xt)
          Y <- Xt + matrix(rnorm(nrow(Xt)*mx),nrow(Xt),mx)%*%C
          X <- distr_correction(Y,Xt)
          ops[[ij]][,':=' (ba=X[,1],dbh=X[,2],h=X[,3]*10,pine=X[,4],spruce=X[,5],birch=Xt[,6])] # the height converted back to meters
          #          ops[[ij]] <- opsX
        } 
        if(uncSiteType){
          ###load the fittet probit models to estimate the Site fertility class
          #load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
          ####generate sample input data
          dataSample <- data.table(st=ops[[ij]]$fert,
                                   H=ops[[ij]]$h,
                                   D=ops[[ij]]$dbh,
                                   BAtot=ops[[ij]]$ba,
                                   BApPer=ops[[ij]]$pine
          )
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
      }
    }
    startRun <- Sys.time() 
    #sampleXs <- lapply(sampleIDs[1:3], function(jx) { runModel(jx, outType=outType)})      
    #sampleXs <- mclapply(sampleIDs[(1+(nii-1)*nParRuns):(nii*nParRuns)], function(jx) {
    if(uncSeg){
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
      
      save(sampleOutput,file=paste0("uncRuns/samplexout",r_no,
                                    "_",harvscen,"_",                                    
                                    "samplesize",nSitesRunr,
                                    "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                    "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
      
      print("make histograms...")
      m <- length(sampleOutput)
      print(paste(m,"variables"))
      n <- nrow(sampleOutput[[1]])
      #varNams: "NEP" "soilC" 
      #"V" "VroundWood" "age"        
      #"VenergyWood" "wGV" 
      #"Wtot" "periods"   
      units_hist <- c(-area_total*100^2*44/12*10^-12,1,
                      1,area_total*10^-6, 1,
                      area_total*10^-6, 1, 1, 1)
      units_hist_label <- c("NEE [Tg CO2eq]", "mean soilC [kgC ha-1]",
                            "mean V [m3 ha-1]", "Tot. Vroundwood [10^6 m3]","mean age [years]",
                            "tot. VenergyWood [10^6 m3]","mean GV biomass [kgC ha-1]",
                            "mean tree biomass [kgC ha-1]") 
      
      for(indj in 1:(m-1)){
        x <- sampleOutput[[indj]]
        varNams <- rownames(x)[1]
        xnas <- which(is.na(x[,1]))
        x <- x[which(!is.na(x[,1])),]
        x <- x*units_hist[indj]
        png(file = paste0("uncRuns/hists_regionID",r_no,"_",varNams,
                          "_",nSitesRunr,"_",harvscen,"_",
                          "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                          "_Cr",uncClim,"_str",uncSiteType,".png"))
        xlims <- c(min(x),max(x))
        xlims[1] <- xlims[1]*(1-0.1*sign(xlims[1]))
        xlims[2] <- xlims[2]*(1+0.1*sign(xlims[2]))
        par(mfrow=c(3,1))
        for(per in 1:3){
          if(per==1 & length(xnas)>0){
            hist(as.matrix(x[, paste0("p", per)]),#, with = FALSE]),
                 main = paste0("region",r_no,"_",harvscen,"_",
                               "period",per," nas: sampleIDs ",xnas), 
                 xlab = units_hist_label[indj],
                 xlim = xlims)  
          } else {
            hist(as.matrix(x[, paste0("p", per)]),#, with = FALSE]),
                 main = paste0("region",r_no,"_",harvscen,"_period",per), 
                 xlab = units_hist_label[indj],
                 xlim = xlims)  
          }
        }
        dev.off()
      }
      print("histograms made")
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
      save(sampleOutput,file=paste0("uncRuns/segRuns/samplexout_uncSeg_",r_no,
                                    "set",setX,"_",harvscen,                                    
                                    "_pr",uncPCrobas,"_Xr",uncInput,"_ager",uncAge,
                                    "_Cr",uncClim,"_str",uncSiteType,".rdata")) 
      
    }
    print(paste0("Run time for ",nParRuns," samples of size ", nSitesRunr," = ",timeRun))
    print("End running...")
  }
}
setwd("Rsrc/virpiSbatch/")
