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

# Virpi's code starts here:

rm(list = ls())
library(raster)
library(ggplot2)
library(ff)
library(stringr)

outputfolder <- "/scratch/project_2000994/PREBASruns/finRuns/plots/" # path to figure saving folder 
datafolder <- "/scratch/project_2000994/PREBASruns/finRuns/rasters/" # path to result data

# which regions are plotted
reg_inds <- 4 #c(1:19)

scenarionames <- c("Base","NoHarv","MaxSust","Low") # scenario names for plots
scenarios <- c("baseRuns","NoHarv","MaxSust","Low") # scenario names in files that are uploaded one by one

region_names <- c("Uusimaa","Ahvenanmaa","Keski-Pohjanmaa",
                  "Pirkanmaa","Etela-Karjala","Keski-Suomi",
                  "Pohjois-Savo","Lappi","Kanta-Hame","Pohjanmaa",
                  "Varsinais-Suomi","Etela-Pohjanmaa","Paijat-Hame",
                  "Satakunta","Kymenlaakso","Kainuu","Etela-Savo",
                  "Pohjois-Karjala","Pohjois-Pohjanmaa")

region_ids <- c('01','21','16',
                '06','09','13',
                '11','19','05','15',
                '02','14','07',
                '04','08','18','10',
                '12','17') # official region numbering - if needed


# variables that are plotted
varnames <- c("NEP","grossGrowth","V_","D_",
              "Venergy","Vround",
              "DeadWood","Wtot","soilC",
              "Vdec","domAge") 

# units of tha variables
var_units <- c("gC m-2 a-1","m3 ha-1 a-1","m3 ha-1 a-1","cm",
               "m3 ha-1","m3 ha-1",
               "m3 ha-1","kgC ha-1","kgC ha-1",
               "m3 ha-1","years")

# figure axis limits for different variables
var_limits <- data.frame(vari = varnames, 
                         mins = c(-600,0,0,0,
                                  0,0,
                                  0,0,0,
                                  0,0),
                         maxs = c(600,20,600,40,
                                  15,100,
                                  220,150000,150000,
                                  200,150),
                         dx = c(300,10,200,10,
                                5,25,
                                50,50000,50000,
                                50,50))


for(hi in 1:length(scenarios)){
  
  harvscen <- scenarionames[hi]
  print(harvscen)
  
  for(reg_id in reg_inds){
    forCent <- list.files(datafolder, pattern = paste0("forCent",reg_id))[1]
    ij <- as.numeric(strsplit(forCent,"forCent")[[1]][2])
    print(paste0(forCent,"=",region_names[ij]))
    
    for(varij in 1:length(varnames)){
      vari_files <- list.files(file.path(datafolder, forCent), pattern = varnames[varij])
      vari_files <- vari_files[str_detect(vari_files, harvscen)]
      for(jj in 1:length(vari_files)){
        var_file <- vari_files[jj]
        vari <- raster(file.path(datafolder, forCent, var_file))
        assign(paste0("r",jj),vari)
        if(varnames[varij]=="DeadWood"){
          png(file=paste0(outputfolder,"map_reg",reg_id,"_",strsplit(var_file, split = "_CurrClim.tif"),".png"))
          plot(vari,
               col = terrain.colors(5),# main = region_names[ij],
               axes = FALSE,
               main = paste0("Scenario: ",harvscen),
               xlab = paste0(strsplit(var_file, split = "_CurrClim.tif")))
          dev.off()
        }
      }
      s <- stack(r1, r2, r3)
      names(s) <- c('per1', 'per2', 'per3')
      assign(paste0("var",varij),s)
      
      png(file=paste0(outputfolder,"boxplot_reg",reg_id,"_",harvscen,"_",varnames[varij],".png"))
      boxplot(s, notch=TRUE, col='gray',#col=c('red', 'blue', 'orange'), 
              main=paste0(harvscen,":",strsplit(vari_files[[1]], split = paste0("_2017-2025_",harvscen,"_CurrClim.tif"))), 
              ylab= var_units[varij],
              ylim = c(var_limits[varij,2],var_limits[varij,3]),
              yaxt = "n")
      yat <- seq(var_limits[varij,2],var_limits[varij,3],var_limits[varij,4])
      axis(2, at=yat,labels=yat, col.axis="black", las=2)
      grid(nx=NA, ny=NULL) #grid over boxplot
      par(new=TRUE)  
      boxplot(s, notch = TRUE,col="gray",
              ylim = c(var_limits[varij,2],var_limits[varij,3]),          
              yaxt = "n")
      dev.off()
    }
  }
  
  # plot total roundwood
  png(file=paste0(outputfolder,"bar_reg",reg_id,"_",harvscen,"_",varnames[6],".png"))
  vals_mat <- na.omit(getValues(var6))
  vals_mat_sums <- colSums(vals_mat)/10^6*0.16*0.16
  barplot(vals_mat_sums, 
          ylim = c(0, 6),
          ylab = paste0("million m3"),
          main = paste0(harvscen,": ",varnames[6]),
          yaxt = "n")
  yat <- c(0,2,4,6)
  axis(2, at=yat,labels=yat, col.axis="black", las=2)
  grid(nx=NA, ny=NULL)
  par(new=TRUE)
  barplot(vals_mat_sums,
          ylim = c(0, 6),
          yaxt = "n")
  dev.off()
  
  # plot total energywood
  png(file=paste0(outputfolder,"bar_reg",reg_id,"_",harvscen,"_",varnames[5],".png"))
  vals_mat <- na.omit(getValues(var5))
  vals_mat_sums <- colSums(vals_mat)/10^6*0.16*0.16
  barplot(vals_mat_sums, 
          ylab = paste0("million m3"),
          ylim = c(0,1.5),
          yaxt = "n",
          main = paste0(harvscen,": ",varnames[5]))
  yat <- c(0,0.5,1,1.5)
  axis(2, at=yat,labels=yat, col.axis="black", las=2)
  grid(nx=NA, ny=NULL)
  par(new=TRUE)
  barplot(vals_mat_sums,                
          ylim = c(0,1.5),
          yaxt = "n")
  dev.off()
  
  # plot total NEE
  png(file=paste0(outputfolder,"bar_reg",reg_id,"_",harvscen,"_",varnames[1],".png"))
  vals_mat <- na.omit(getValues(var1))
  vals_mat_sums <- colSums(vals_mat)*(-1)*16*16*44/12/10^12
  barplot(vals_mat_sums, 
          ylab = paste0("TgCO2eq"),
          yaxt = "n",
          ylim = c(-8.5,1),
          main = paste0(harvscen,": ",varnames[1]))
  yat <- seq(-8,0,2)
  axis(2, at=yat,labels=yat, col.axis="black", las=2) 
  grid(nx=NA, ny=NULL)
  par(new=TRUE)
  barplot(vals_mat_sums, 
          yaxt = "n",
          ylim = c(-8.5,1)
  )
  dev.off()
  
  # plot total biomass
  vals_mat <- na.omit(getValues(var8))
  vals_mat_means <- colMeans(vals_mat)*0.16*0.16
  png(file=paste0(outputfolder,"bar_reg",reg_id,"_",harvscen,"_",varnames[8],".png"))
  barplot(vals_mat_means, 
          ylab = paste0("kgC ha-1"),
          yaxt = "n",
          ylim = c(0,2000),
          main = paste0(harvscen,": ",varnames[8]))
  yat <- seq(0,2000,500)
  axis(2, at=yat,labels=yat, col.axis="black", las=2)
  grid(nx=NA, ny=NULL)
  par(new=TRUE)
  barplot(vals_mat_means, 
          yaxt = "n",
          ylim = c(0,2000)
  )
  dev.off()
  
  # variable y w respect variable x                
  
  inds <- c(11,2) # give variable (x,y) indexes
  ss <- stack(var11,var2) # give same indexes here
  vals_mat <- na.omit(getValues(ss))
  png(file=paste0(outputfolder,"scatter_reg",reg_id,"_",harvscen,"_",varnames[inds[1]],"_",varnames[inds[2]],"per1.png"))
  nn <- sample(1:nrow(vals_mat),10000)
  lims1 <- c(min(vals_mat[nn,1:3]), max(vals_mat[nn,1:3]))
  lims2 <- c(min(vals_mat[nn,4:6]), max(vals_mat[nn,4:6]))
  
  plot(vals_mat[nn,1],vals_mat[nn,4], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"), 
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"),
       xlim = lims1,
       ylim = lims2,
       main="2017-2025")
  grid(nx=NULL, ny=NULL) #grid over 
  par(new=TRUE)
  plot(vals_mat[nn,1],vals_mat[nn,4], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"),
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"), 
       xlim = lims1,
       ylim = lims2,
       main="2017-2025")
  dev.off()
  
  png(file=paste0(outputfolder,"scatter_reg",reg_id,"_",harvscen,"_",varnames[inds[1]],"_",varnames[inds[2]],"per2.png"))
  plot(vals_mat[nn,2],vals_mat[nn,5], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"),
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"), 
       xlim = lims1,
       ylim = lims2,
       main="2026-2033")
  grid(nx=NULL, ny=NULL) #grid over 
  par(new=TRUE)
  plot(vals_mat[nn,2],vals_mat[nn,5], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"),
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"), 
       xlim = lims1,
       ylim = lims2,
       main="2026-2033")
  dev.off()
  
  png(file=paste0(outputfolder,"scatter_reg",reg_id,"_",harvscen,"_",varnames[inds[1]],"_",varnames[inds[2]],"per3.png"))
  plot(vals_mat[nn,3],vals_mat[nn,6], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"),
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"), 
       xlim = lims1,
       ylim = lims2,
       main="2034-2050")
  grid(nx=NULL, ny=NULL) #grid over 
  par(new=TRUE)
  plot(vals_mat[nn,3],vals_mat[nn,6], 
       xlab = paste0(varnames[inds[1]]," [",var_units[inds[1]],"]"),
       ylab = paste0(varnames[inds[2]]," [",var_units[inds[2]],"]"), 
       xlim = lims1,
       ylim = lims2,
       main="2034-2050")
  dev.off()
  
}        



