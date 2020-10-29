library(readxl)
library(tidyverse)
library(sp)
library(rgdal)
library(rmarkdown)
tph_data<-read_excel("M:/OtoR/Phillips66/Projects/Greybull/ProjectDocuments/SiteCharterztn/Reports/OldVersions/DRAFT_2013_SiteChar/Refs-TechMemos/SmearZone/NewLNAPLBodyEval_GPworking.xlsx", sheet=2)
tph_data$`X Coordinate`<-as.numeric(tph_data$`X Coordinate`)
tph_data$`Y Coordinate`<-as.numeric(tph_data$`Y Coordinate`)
fgdb<-"M:/OtoR/Phillips66/Projects/Greybull/GIS/Projects/SiteCharacterization/Report_2014/Data/Greybull_SCR_Data.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
fc <- readOGR(dsn=fgdb,layer="Smear_Zone_Eval")
dirty<-fc$Location[fc$SZ_Presence_Analytical=="Dirty"]
tbl1<-as_tibble(tph_shp[tph_shp$`DRO+GRO`>250,c(1,4,17)])
tbl1$Date.Sampled<-as.Date(tbl1$Date.Sampled)

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
TPH<-mean(tph_shp[tph_shp$`DRO+GRO`>250,]$`DRO+GRO`) # mean TPH in soil samples with total TPH above 1000, i.e., indicative of LNAPL
TPH<-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`DRO+GRO`,.9)
rhop<-2.65  # particle density
rho<-0.8  # LNAPL density
phi<-0.348  # porosity
sn(TPH,phi,rho,rhop)  # average LNAPL saturation
quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`,.75)-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`X Coordinate`,0.25)
quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`,.75)-quantile(tph_shp[tph_shp$`DRO+GRO`>1000,]$`Y Coordinate`,0.25)

library(knitr)
rmarkdown::render('Markdown.Rmd', output_file = "output.html")
save(tbl1,file="tbl1.Rda")

import<-read_csv("SECOR_AppendixE_data.csv")
