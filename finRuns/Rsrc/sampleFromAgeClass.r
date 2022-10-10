source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/finRuns/Rsrc/settings.r")
source_url("https://raw.githubusercontent.com/ForModLabUHel/IBCcarbon_runs/master/general/functions.r")
load("/scratch/project_2000994/PREBASruns/metadata/maakunta/ageClass_probs.rdata")
probs <- ageClass_probs[maakID==r_no]
nSample=20000
probs[,NbyAgeClass:= round(nSample*probAgeClass)]
set.seed(123450)
sampleX <- data.all[age==0][sample(.N,probs[age20==0]$NbyAgeClass,replace=T)]
set.seed(123451)
sampleX <- rbind(sampleX,data.all[age>0 & age<=20][sample(.N,probs[age20==10]$NbyAgeClass,replace=T)])
set.seed(123452)
sampleX <- rbind(sampleX,data.all[age>20 & age<=40][sample(.N,probs[age20==30]$NbyAgeClass,replace=T)])
set.seed(123453)
sampleX <- rbind(sampleX,data.all[age>40 & age<=60][sample(.N,probs[age20==50]$NbyAgeClass,replace=T)])
set.seed(123454)
sampleX <- rbind(sampleX,data.all[age>60 & age<=80][sample(.N,probs[age20==70]$NbyAgeClass,replace=T)])
set.seed(123455)
sampleX <- rbind(sampleX,data.all[age>80 & age<=100][sample(.N,probs[age20==90]$NbyAgeClass,replace=T)])
set.seed(123456)
sampleX <- rbind(sampleX,data.all[age>100 & age<=120][sample(.N,probs[age20==110]$NbyAgeClass,replace=T)])
set.seed(123457)
sampleX <- rbind(sampleX,data.all[age>120 & age<=140][sample(.N,probs[age20==130]$NbyAgeClass,replace=T)])
set.seed(123458)
sampleX <- rbind(sampleX,data.all[age>140][sample(.N,probs[age20==150]$NbyAgeClass,replace=T)])
sampleX$area <- 1
sampleX[,N:= as.double(N)]
sampleX[,N:= 1/(16^2/10000)]
nSample <- nrow(sampleX)
