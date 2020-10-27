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
writeOGR(tph_shp, dsn = getwd(), layer = "tph", driver = "ESRI Shapefile", overwrite_layer=TRUE)
tph_data$`Location ID`[tph_data$`DRO+GRO`>1000]
points(tph_shp[tph_shp$`DRO+GRO`>1000,], pch=19)
which(tph_shp$`Location ID` %Like% 'AOC5-BH6%')
tph_shp$`Location ID`[which(grepl('^AOC5-BH6',tph_shp$`Location ID`))]
points(tph_shp[c(28,29),],pch="X")

tph_shp$`Location ID`=='AOC5-BH9%'

max(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`)-min(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`)
max(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`)-min(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`)
# CONVERSION of TPH to LNAPL Saturation
## see http://naplansr.com/conversion-of-tph-to-napl-saturation-volume-2-issue-1-january-2012/
sn<-function (TPH, phi, rho, rhop) {
  sn=TPH*(1-phi)*rhop*10^-6/(phi*rho)
  return(sn)
  }
TPH<-mean(tph_shp[tph_shp$`DRO+GRO`>1000,]$`DRO+GRO`) # mean TPH in soil samples with total TPH above 1000, i.e., indicative of LNAPL
TPH<-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`DRO+GRO`,.9)
rhop<-2.65  # particle density
rho<-0.8  # LNAPL density
phi<-0.43  # porosity
sn(TPH,phi,rho,rhop)  # average LNAPL saturation
quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`,.75)-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`,0.25)
quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`,.75)-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`,0.25)
