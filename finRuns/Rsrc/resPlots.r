library(raster)
library(Rprebasso)
r_no <- 4
varID <- 46
varX <- varNames[46]
periods <- c("2017-2025", "2026-2033","2034-2050")
harvScen <- "Base"
climScen <- "CurrClim"
pathX <- "/scratch/project_2000994/PREBASruns/finRuns/rasters/"

rast1 <- raster(paste0(pathX,"forCent",r_no,"/",varX,"_",periods[1],"_",harvScen,"_",climScen,".tif"))
rast2 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2026-2033_Base_CurrClim.tif")
rast3 <- raster("/scratch/project_2000994/PREBASruns/finRuns/rasters/forCent4/NEP_2034-2050_Base_CurrClim.tif")









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
