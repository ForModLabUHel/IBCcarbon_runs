# define some settings that you might want to manipulate later
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

####run the model with base scenario and initialize the soilC
modRun_Base <- runModel(sampleID,outType="testRun",forceSaveInitSoil = T,
                        harvScen=harvScen,harvInten=harvInten,sampleX = sampleX)

####run the model with No harvest scenario
modRun_NoHarv <- runModel(sampleID,outType="testRun",
                          harvScen="NoHarv",harvInten=harvInten,sampleX = sampleX)






