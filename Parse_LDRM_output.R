rm(list=ls())
library(readr)
library(dplyr)
library(readxl)
library(tidyverse)
#read data
out1<-read.csv("Greybull.csv", header = FALSE, col.names = seq(1,16))
tbl<-read_excel("201312_Tables1to3_TBL.xls")
SZ<-read_csv("SZ_Thickness_vertices.csv") # smear zone thickness 
grnd<-read_csv("site_ground.csv") # ground elevations above smear zone
SZ_pts<-read_csv("SZ_vertices.csv") # smear zone elevations
#find where the table breaks in the output file are
idx1<-max(which(out1$X1=="Source Mass Losses Through Time"))
idx2<-max(which(out1$X1=="Source Zone Composition Through Time"))

## extract source mass loss through time table 
source_mass_losses_though_time<- as_tibble(out1[(idx1+2):nrow(out1),])
colnames(source_mass_losses_though_time)<-source_mass_losses_though_time[1,]
source_mass_losses_though_time<-source_mass_losses_though_time[2:nrow(source_mass_losses_though_time),]
source_mass_losses_though_time <- source_mass_losses_though_time %>%
  mutate_all(as.double)

## extract Source Zone Composition Through Time table 
composition<-as_tibble(out1[(idx2+2):(idx1-1),1:6])
colnames(composition)<-composition[1,]
composition<-composition[2:nrow(composition),]
composition <- composition %>%
  mutate_all(as.double)

# data input tables
# solute transport properties
idx1<-max(which(out1$X1=="Solute Transport Properties"))
idx2<-max(which(out1$X1=="Miscellaneous Calculation Options"))
transport_props<-as_tibble(out1[(idx1+2):(idx2-1),1:2])
colnames(transport_props)<-c("property", "value")
transport_props$value<-parse_double(transport_props$value)

# LNAPL constituents
idx2<-max(which(out1$X1=="Constituent"))
constituent_props<-as_tibble(out1[(idx2):(idx1-1),1:7])
colnames(constituent_props)<-constituent_props[1,]
constituent_props<-constituent_props[2:nrow(constituent_props),]
constituent_props[,2:7] <- constituent_props[,2:7] %>%
  mutate_all(as.double)

# LNAPL properties
idx1<-max(which(out1$X1=="LNAPL Properties"))
LNAPL_props<-as_tibble(out1[(idx1+2):(idx2-1),1:3])
colnames(LNAPL_props)<-c("property", "character","value")
LNAPL_props$value[2:5]<-LNAPL_props$character[2:5]
LNAPL_props$value<-as.double(LNAPL_props$value)

# Source Area Parameters
idx2<-max(which(out1$X1=="Source Area Parameters"))
SourceArea_props<-as_tibble(out1[(idx2+2):(idx1-1),1:2])
colnames(SourceArea_props)<-c("property", "value")
SourceArea_props$value <- parse_double(SourceArea_props$value)

# Groundwater Conditions
idx1<-max(which(out1$X1=="Groundwater Conditions"))
GW_conditions<-as_tibble(out1[(idx1+2):(idx2-1),1:2])
colnames(GW_conditions)<-c("property", "value")
GW_conditions$value<-parse_double(GW_conditions$value)

# Soil Properties
idx2<-max(which(out1$X1=="Soil Properties"))
Soil_props<-as_tibble(out1[(idx2+2):(idx1-1),1:3])
colnames(Soil_props)<-c("property", "character","value")
Soil_props$value[2:7]<-Soil_props$character[2:7]
Soil_props$value<-as.double(Soil_props$value)

# calculations on smear zone thickness exported from 3D model
SourceArea_props$value[1] <- mean(SZ$z) # LNAPL thickness
SourceArea_props$value[3] <- signif(quantile(SZ$x,.75)-quantile(SZ$x,.25),1) # smear zone length
SourceArea_props$value[4] <- signif(quantile(SZ$y,.75)-quantile(SZ$y,.25),1) # smear zone width
SourceArea_props$value[2] <- (mean(grnd$z) - quantile(SZ_pts$z,.95)) # average depth to top of smear zone

SourceArea_props$source <- c(rep("3D Model",4), "default")

# GW conditions calculations and sources
GW_conditions$value[1] <- mean(c(0.0023, 0.028))

GW_conditions$source <- c("Average for shallow monitoring wells in SCR Table 3-2", "calculated from hydraulic conductivity and gradient", "calculated from hydraulic conductivity, gradient, and effective porosity")

# Soil Props
Soil_props$value[6] <- 0.43
Soil_props$character[6] <- "0.43"
Soil_props$value[7] <- (1.24e-6/100*60*60*24) # unit conversion from cm/s to m/d
Soil_props$character[6] <- Soil_props$value[7]

Soil_props$source <- c("Boring Logs", rep("default",4), "Value used in MTGW evaluation, SCR page 2-4", "Value used in MTGW evaluation (for sandstone), SCR page 2-4, converted to meters/day")

# transport Props
transport_props$source <- c(rep("Contaminant Transport Modeling Report Table 1",4), rep("default",2))

LNAPL_props$source<-rep("assumed/default",nrow(LNAPL_props))
constituent_props$source<-rep("assumed/default",nrow(constituent_props))

#bring the modified values back into the CSV and export it                        
# solute transport properties
idx1<-max(which(out1$X1=="Solute Transport Properties"))
idx2<-max(which(out1$X1=="Miscellaneous Calculation Options"))
out1[(idx1+2):(idx2-1),1:ncol(transport_props)]<-transport_props
out1[(idx1+1),ncol(transport_props)]<-"Source"

# LNAPL constituents
idx2<-max(which(out1$X1=="Constituent"))
out1[(idx2+1):(idx1-1),1:ncol(constituent_props)]<-constituent_props
out1[(idx2),ncol(constituent_props)]<-"Source"

# LNAPL properties
idx1<-max(which(out1$X1=="LNAPL Properties"))
out1[(idx1+2):(idx2-1),1:ncol(LNAPL_props)]<-LNAPL_props
out1[(idx1+1),ncol(LNAPL_props)]<-"Source"

# Source Area Parameters
idx2<-max(which(out1$X1=="Source Area Parameters"))
out1[(idx2+2):(idx1-1),1:ncol(SourceArea_props)]<-SourceArea_props
out1[(idx2+1),ncol(SourceArea_props)]<-"Source"

# Groundwater Conditions
idx1<-max(which(out1$X1=="Groundwater Conditions"))
out1[(idx1+2):(idx2-1),1:ncol(GW_conditions)]<-GW_conditions
out1[(idx1+1),ncol(GW_conditions)]<-"Source"

# Soil Properties
idx2<-max(which(out1$X1=="Soil Properties"))
out1[(idx2+2):(idx1-1),1:ncol(Soil_props)]<-Soil_props
out1[(idx2+1),ncol(Soil_props)]<-"Source"

#write output
write_csv(out1, "out1.csv", col_names = FALSE)

