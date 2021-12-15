library(rgdal)
library(raster)
library(ggpubr)
library(ggplot2)
library(devtools)
library(data.table)
library(ggridges)
library(parallel)
library(sp)
library(sf)

###FUNCTION FOR EXTRACTING DATA FROM AREAS IN MAAKUNTA REGIONS BASED ON PERIOD, HARVEST, & CLIMATE SCENARIOS
    ##areax is the shapefile or raster of the area you would like to extract data from
    ##pathdata is the directory of files
    ##VARx is the variable of data to extract (Soil Carbon, Wood Harvested, etc.)
    ##periodx is the period of time for the scenario (2017-2025, 2026-2033, 2034-2050)
    ##harvscenx is the harvest scenario (Base, Low, MaxSust, NoHarv)
    ##climscenx is the climate scenario (CurrClim)

##Extraction Function
extractraster <- function(areax, pathdata, VARx, periodx, harvscenx, climscenx, conx, savex){

    ##Crop the country shapefile to just the desired area
    crop <- crop(conx, areax)

    ##Extract vector of region IDs present in the desired area
    regs <- c(crop$maakID)
    
    ##The number of regions within the area
    nreg <- length(regs)
    
    ##Create an empty list of the rasters to fill and merge
    xrast <- list()
    
    ##Loop to extract data from maakunta regions
    ##If more than 1 region, the rasters for the same time, variables, and scenarios are merged into one raster
    if(nreg > 1){
        for(j in 1:nreg) xrast[[j]] <- raster(paste0(pathdata,"forCent",regs[j],"/",VARx,"_",periodx,"_",harvscenx,"_",climscenx,".tif"))
            rast1 <- do.call(merge, xrast)
            
    } else{
        rast1 <- raster(paste0(pathdata,"forCent",regs[1],"/",VARx,"_",periodx,"_",harvscenx,"_",climscenx,".tif"))
        }
    
    ##The region (raster or polygon) is cropped to the desired area
    if(class(areax) %in% c("raster","RasterLayer")){
        crop1 <- crop(rast1, extent(areax))
    }else{
        crop1 <- mask(crop(rast1, areax),areax)
    }
    
    
    ##Save the new raster for that particular area
    writeRaster(crop1, paste0(savex, VARx,"_",periodx,"_",harvscenx,"_",climscenx,".tif"),overwrite=T)
}

###INPUTS FOR FUNCTION
##Path for file upload
pathdata <- "https://a3s.fi/swift/v1/AUTH_70b34f161b3643938200c2ec96aa2ca0/PREBASruns/finRuns/rasters_v0.2/"

##List of variables
VAR <- c("WenergyWood","Wtot","soilC")

##Periods of time
periods <- c("2017-2025", "2034-2050")

##Desired Area
areax <- raster("/scratch/project_2000994/PREBASruns/ZonUnc/Testmaps/EVO/OUTPUT.tif")

##Upload the country-wide shapefile
con <- shapefile("/scratch/project_2000994/PREBASruns/ZonUnc/Testmaps/SuomenMaakuntajako_2021_10k.shp")

##Path to save completed files
save <- "/scratch/project_2000994/PREBASruns/ZonUnc/Testmaps/outRast/"

###RUN FUNCTION
##Loop to input multiple variables and periods into the function
for(i in 1:length(VAR)){
    for(j in 1:length(periods)){
        extractraster(areax, pathdata, VAR[i], periods[j], "Base", "CurrClim", con, save)
    }
}
