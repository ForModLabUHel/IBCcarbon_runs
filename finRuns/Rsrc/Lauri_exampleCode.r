# define some settings that you might want to manipulate later
Lauris_sample = T
CSCrun = T
r_no = regions = 1 ### forest center ID
nSetRuns = 10 #number of set runs
regSets <- "maakunta" ### "forCent", "maakunta"
minDharvX <- 15
harvScen = "Base" ### c("Low","MaxSust","NoHarv","Base","Mitigation", "MitigationNoAdH","adapt","protect")
harvInten = "Base"

##load general settings
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
##load functions
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")

###this is the sample you need to generate from your data
sampleX <- data.all[1:1000]
if(Lauris_sample) load("/scratch/project_2000994/PREBASruns/finRuns/Lauris_sampleX.rdata")

####run the model with base scenario and initialize the soilC
modRun_Base <- runModel(sampleID,outType="testRun",forceSaveInitSoil = T,
                        harvScen=harvScen,harvInten=harvInten,sampleX = sampleX)

####run the model with No harvest scenario
modRun_NoHarv <- runModel(sampleID,outType="testRun",
                          harvScen="NoHarv",harvInten=harvInten,sampleX = sampleX)


siteX= 235
varX = 11

ylimX <- range(modRun_Base$region$multiOut[siteX,,varX,,1])
plot(modRun_Base$region$multiOut[siteX,,varX,1,1],ylim=ylimX)
points(modRun_Base$region$multiOut[siteX,,varX,2,1],col=2)
points(modRun_Base$region$multiOut[siteX,,varX,3,1],col=3)

ylimX <- range(modRun_NoHarv$region$multiOut[siteX,,varX,,1])
plot(modRun_NoHarv$region$multiOut[siteX,,varX,1,1],ylim=ylimX)
points(modRun_NoHarv$region$multiOut[siteX,,varX,2,1],col=2)
points(modRun_NoHarv$region$multiOut[siteX,,varX,3,1],col=3)


Vtot_base <- apply(modRun_Base$region$multiOut[,,30,,1],2,sum) 

Vtot_noharv <- apply(modRun_NoHarv$region$multiOut[,,30,,1],2,sum) 

ylimX <-range(Vtot_base,Vtot_noharv)

plot(Vtot_base,ylim=ylimX)
points(Vtot_noharv,col=2)


