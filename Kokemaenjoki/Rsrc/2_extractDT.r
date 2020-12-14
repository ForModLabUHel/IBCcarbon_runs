devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/Kokemaenjoki/Rsrc/functions.r")

clims <- "CurrClim" #c("CurrClim", "rcp26", "rcp45")
mans <- c("MaxSust", "Base","Low") #"Base"
yearOut <- 2060
 variable <- c(43,7,8,18,17,24:33,37,38,39,41,44:46)
 # variable <- c(30,37,42,43)
varNames[47] <- "Wtot"
species <- "tot" #1,2,3
startingYear <- 2015

for(climate in clims){
  for(management in mans){
    createDT(climate,management,variable,species,startingYear)

    print(management)
  }
}

##Create data.table energyWood

  # files <- intersect(list.files(path= "output/", pattern = climate), list.files(path= "output/",pattern = management))
  # 
  # VenergyWood <- WenergyWood <- data.table()
  # 
  # for(i in 1:length(files)){
  #   sampleID <- paste0("sample",i,".")
  #   
  #   fileX <- files[grep(sampleID,files,fixed = T)]
  #   
  #   load(paste0("output/",fileX))
  #   
  #   ###sum harvests
  #   if(i==1){
  #     harvest <- out$harvest
  #   }else{
  #     harvest <- harvest+out$harvest  
  #   }
  #   
  #   margin= 1:2#(length(dim(out$annual[,,variable,]))-1)
  #   VenergyWood <- rbind(VenergyWood,apply(out$energyWood[,,,1],1:2,sum))
  #   WenergyWood <- rbind(WenergyWood,apply(out$energyWood[,,,2],1:2,sum))
  #   
  #   print(i)
  # }
  # 
  # 
  # save(VenergyWood,file=paste0("outputDT/","VenergyWood","_",management,"_",climate,".rdata"))
  # save(WenergyWood,file=paste0("outputDT/","WenergyWood","_",management,"_",climate,".rdata"))
  # 
# manClim <- "_Base_CurrClim.rdata"
# Wtot(manClim)



# createDTbySp("CurrClim","NoHArv",c(8,30),species,2015)

# manClim <- "_NoHarv_rcp45.rdata"
# Wtot(manClim)
# manClim <- "_Base_rcp45.rdata"
# Wtot(manClim)
# manClim <- "_Base_CurrClim.rdata"
# Wtot(manClim)
# manClim <- "_Base_rcp26.rdata"
# Wtot(manClim)
# manClim <- "_NoHarv_CurrClim.rdata"
# Wtot(manClim)
# manClim <- "_NoHarv_rcp26.rdata"
# Wtot(manClim)
