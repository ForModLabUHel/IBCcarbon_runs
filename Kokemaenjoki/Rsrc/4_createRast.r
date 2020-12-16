devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")

yearOut=2016:2099
variables <- c(18,24:25,30:33,37:39,43:44,46,48:49)#,47)
# variables <- c(30:33,37:39,43:44,46,48:49)
varNames[47] <- "Wtot"
varNames[48] <- "WenergyWood"
varNames[49] <- "VenergyWood"
species="tot"
varNames[c(9,26:29,43:46)] <- c("ResTot","LitFol","LitFroot","LitBranch","LitWood",
                                "grossGrowth","GPP","Rh","NEP")
load("input/segIDkoke.rdata")
setnames(kokeIDsTab,"ls_seg2","segID")
kokeShp <- readOGR(dsn = "shapes", layer = "Koke_Paavesistoalue_VALUE")

# for(variable in variables){
#   aTOTfromDT(yearOut, variable, species, startingYear)
# }
# print("plots done")

per1=2017:2025
per2=2026:2033
per3=2034:2050
# per1=2017:2030
# per2=2040:2069
# per3=2070:2098
clims <- rps#"CurrClim" #c("CurrClim", "rcp26", "rcp45")
mans <- harvestscenarios#"Base"#c("MaxSust", "Base","Low") #"Base"
# for(variable in variables){
for(climate in clims){
 for(management in mans){
  yearOut=per1
  mclapply(variables, function(jx) {
    createTifFromDT(climate, management, yearOut, jx, species, startingYear)
  }, mc.cores = nCores)      ## Split this job across 10 cores
  print(varNames[management])
  }
}
# print(varNames[variable])
# }
print("tif period1 done")

for(climate in clims){
  for(management in mans){
    yearOut=per2
    mclapply(variables, function(jx) {
      createTifFromDT(climate, management, yearOut, jx, species, startingYear)
    }, mc.cores = nCores)      ## Split this job across 10 cores
    print(varNames[management])
  }
}
print("tif period2 done")

for(climate in clims){
  for(management in mans){
    yearOut=per3
    mclapply(variables, function(jx) {
      createTifFromDT(climate, management, yearOut, jx, species, startingYear)
    }, mc.cores = nCores)      ## Split this job across 10 cores
    print(varNames[management])
  }
}
print("tif period3 done")
