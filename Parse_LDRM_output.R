library(readr)
library(dplyr)
library(readxl)
library(tidyverse)
#read data
out1<-read.csv("Greybull.csv", header = FALSE, col.names = seq(1,16))
tbl<-read_excel("201312_Tables1to3_TBL.xls")
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

