library(dplyr)
library(data.table)
library(raster)
library(Rprebasso)
r_no <- 4
varID <- 42
varX <- varNames[varID]
periods <- c("2017-2025", "2026-2033","2034-2050")
harvScen <- "Base"
climScen <- "CurrClim"
pathX <- "/scratch/project_2000994/PREBASruns/finRuns/rasters/"



mort1 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[1],"_",harvScen,"_",climScen,".tif"))
mort2 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[2],"_",harvScen,"_",climScen,".tif"))
mort3 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[3],"_",harvScen,"_",climScen,".tif"))


varID <- 12
varX <- varNames[varID]

D1 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[1],"_",harvScen,"_",climScen,".tif"))
D2 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[2],"_",harvScen,"_",climScen,".tif"))
D3 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[3],"_",harvScen,"_",climScen,".tif"))


par(mfrow=c(2,2))
plot(D1,mort1,pch=".",main="period 1",ylab="Vmort",xlab="D")
plot(D2,mort2,pch=".",main="period 2",ylab="Vmort",xlab="D")
plot(D3,mort3,pch=".",main="period 3",ylab="Vmort",xlab="D")


mort1tab <- data.table(rasterToPoints(mort1) )
D1tab <- data.table(rasterToPoints(D1) )
tab1 <- merge(mort1tab,D1tab)
tab1[, Drange := cut(D_2017.2025_Base_CurrClim, 
                     breaks = c(-1,10,15,20,25,30,999), 
                     labels = c("0-10","10-15","15-20",
                                "20-25","25-30",">30"))]
mean1 <- tab1[,mean(Vmort_2017.2025_Base_CurrClim,na.rm=T),by=Drange]
mean1$Drange <- factor(mean1$Drange, levels = c("0-10","10-15","15-20",
                                                "20-25","25-30",">30"))
setkey(mean1,Drange)



mort2tab <- data.table(rasterToPoints(mort2) )
D2tab <- data.table(rasterToPoints(D2) )
tab2 <- merge(mort2tab,D2tab)
tab2[, Drange := cut(D_2026.2033_Base_CurrClim, 
                     breaks = c(-1,10,15,20,25,30,999), 
                     labels = c("0-10","10-15","15-20",
                                "20-25","25-30",">30"))]
mean2 <- tab2[,mean(Vmort_2026.2033_Base_CurrClim,na.rm=T),by=Drange]
mean2$Drange <- factor(mean2$Drange, levels = c("0-10","10-15","15-20",
                                                "20-25","25-30",">30"))
setkey(mean2,Drange)



mort3tab <- data.table(rasterToPoints(mort3) )
D3tab <- data.table(rasterToPoints(D3) )
tab3 <- merge(mort3tab,D3tab)
tab3[, Drange := cut(D_2034.2050_Base_CurrClim, 
                     breaks = c(-1,10,15,20,25,30,999), 
                     labels = c("0-10","10-15","15-20",
                                "20-25","25-30",">30"))]
mean3 <- tab3[,mean(Vmort_2034.2050_Base_CurrClim,na.rm=T),by=Drange]
mean3$Drange <- factor(mean3$Drange, levels = c("0-10","10-15","15-20",
                                              "20-25","25-30",">30"))
setkey(mean3,Drange)

par(mfrow=c(1,1))
plot(mean1$V1, col=2, type='l',ylim=c(0,2),ylab="Vmort",xlab="Dclass",
     xaxt = "n")
lines(mean2$V1, col=3)
lines(mean3$V1, col=4)
legend("topleft",c("17-25","26-33","33-50"),col=2:4,lty=1)
axis(1, at=1:6, labels=c("0-10","10-15","15-20",
                          "20-25","25-30",">30"))

png(filename = "NEPboxplot.png")
par(mfrow=c(2,2))
rast1 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2017-2025_Base_CurrClim.tif")
rast2 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2026-2033_Base_CurrClim.tif")
rast3 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2034-2050_Base_CurrClim.tif")
val1 <- getValues(rast1)
val2 <- getValues(rast2)
val3 <- getValues(rast3)
boxplot(val1,val2,val3,names=c("per1","per2","per3"),ylab="gCm-2a-1",main="Base NEP")

rast1 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2017-2025_Low_CurrClim.tif")
rast2 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2026-2033_Low_CurrClim.tif")
rast3 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2034-2050_Low_CurrClim.tif")
val1 <- getValues(rast1)
val2 <- getValues(rast2)
val3 <- getValues(rast3)
boxplot(val1,val2,val3,names=c("per1","per2","per3"),ylab="gCm-2a-1",main="Low NEP")

rast1 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2017-2025_MaxSust_CurrClim.tif")
rast2 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2026-2033_MaxSust_CurrClim.tif")
rast3 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2034-2050_MaxSust_CurrClim.tif")
val1 <- getValues(rast1)
val2 <- getValues(rast2)
val3 <- getValues(rast3)
boxplot(val1,val2,val3,names=c("per1","per2","per3"),ylab="gCm-2a-1",main="MaxSust NEP")
rast1 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2017-2025_NoHarv_CurrClim.tif")
rast2 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2026-2033_NoHarv_CurrClim.tif")
rast3 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2034-2050_NoHarv_CurrClim.tif")
val1 <- getValues(rast1)
val2 <- getValues(rast2)
val3 <- getValues(rast3)
boxplot(val1,val2,val3,names=c("per1","per2","per3"),ylab="gCm-2a-1",main="NoHarv NEP")
dev.off()


library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
### Create an asymmetric color range

NEPp1 <- raster("C:/Users/checcomi/Documents/research/IBC-carbon/outRast/NEP sp_2017-2025_Base_CurrClim.tif")


pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(NEPp1),
                    na.color = "transparent")
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(NEPp1, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(NEPp1),
#             title = "NEP p1")

## Make vector of colors for values smaller than 0 (20 colors)
rc1 <- colorRampPalette(colors = c("red", "white"), space = "Lab")(20)

## Make vector of colors for values larger than 0 (180 colors)
rc2 <- colorRampPalette(colors = c("white", "green"), space = "Lab")(180)

## Combine the two color palettes
rampcols <- c(rc1, rc2)

mypal <- colorNumeric(palette = rampcols, domain = c(-569.3691,368.43))

leaflet() %>% addTiles() %>%
  addRasterImage(NEPp1, colors = pal, opacity = 0.8) %>%
  addLegend(pal = mypal, values = values(NEPp1),
            title = "NEP p1")
