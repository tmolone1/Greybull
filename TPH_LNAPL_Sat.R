library(readxl)
library(tidyverse)
library(sp)
library(rgdal)
tph_data<-read_excel("M:/OtoR/Phillips66/Projects/Greybull/ProjectDocuments/SiteCharterztn/Reports/OldVersions/DRAFT_2013_SiteChar/Refs-TechMemos/SmearZone/NewLNAPLBodyEval_GPworking.xlsx", sheet=2)
tph_data$`X Coordinate`<-as.numeric(tph_data$`X Coordinate`)
tph_data$`Y Coordinate`<-as.numeric(tph_data$`Y Coordinate`)


tph_shp<- SpatialPointsDataFrame(tph_data[!is.na(tph_data$`X Coordinate`),c("X Coordinate","Y Coordinate")],
                                        data= tph_data[!is.na(tph_data$`X Coordinate`),],
                                        proj4string = CRS("+init=EPSG:3737")) # wyoming state plane east central NAD83
plot(tph_shp)

max(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`)-min(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`)
max(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`)-min(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`)
# CONVERSION of TPH to LNAPL Saturation
## see http://naplansr.com/conversion-of-tph-to-napl-saturation-volume-2-issue-1-january-2012/
sn<-function (TPH, phi, rho, rhop) {
  sn=TPH*(1-phi)*rhop*10^-6/(phi*rho)
  return(sn)
  }
TPH<-mean(tph_shp[tph_shp$`DRO+GRO`>1000,]$`DRO+GRO`) # mean TPH in soil samples with total TPH above 1000, i.e., indicative of LNAPL
rhop<-2.65  # particle density
rho<-0.8  # LNAPL density
phi<-0.43  # porosity
sn(TPH,phi,rho,rhop)  # average LNAPL saturation