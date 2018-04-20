### 19 April 2019 - by Cat ###
## Let's play around with some Tree Spotters data!

# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(randomForest)

# Set Working Directory
setwd("~/Documents/git/treespotters/Data")

## Get all that data!
## Start with phenology data and clean
bb<-read.csv("19.Apr.2018/individual_phenometrics_data.csv", header=TRUE)
bb<-bb%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=NumYs_in_Series)%>%
  rename(precip=Accum_Prcp)%>%
  rename(photo=Daylength)%>%
  rename(phase=Phenophase_Description)%>%
  rename(ID=Individual_ID)
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, precip, photo, ID)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)

### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>2)




