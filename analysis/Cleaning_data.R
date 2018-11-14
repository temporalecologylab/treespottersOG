### 19 April 2019 - by Cat ###
## Let's play around with some Tree Spotters data!
## Find Provenance Lat info and Age!
## Updated 9 May 2018 with new TS data & Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(geosphere)
library(anytime)
library(weathermetrics)
library(measurements)
library(lubridate)
library(egg)


# Set Working Directory
setwd("~/Documents/git/treespotters/analysis")

## Get all that data!
## Start with phenology data and clean
bb<-read.csv("input/individual_phenometrics_data.csv", header=TRUE)
bb<-bb%>%
  rename(lat=Latitude)%>%
  rename(long=Longitude)%>%
  rename(elev=Elevation_in_Meters)%>%
  rename(year=First_Yes_Year)%>%
  rename(month=First_Yes_Month)%>%
  rename(day=First_Yes_Day)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(numYs=Multiple_Observers)%>%
  rename(photo=Daylength) %>%
  rename(phase=Phenophase_Description)%>%
  rename(ID=Individual_ID)
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, photo, ID)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Breaking leaf buds", "budburst", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Leaves", "leafout", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Flowers or flower buds", "flowers", bb.pheno$phase)
bb.pheno$phase<-ifelse(bb.pheno$phase=="Falling leaves", "leaf drop", bb.pheno$phase)

### Now work on finding day of budburst, etc.
bb.pheno<-filter(bb.pheno, numYs>0)
stan.bb<-bb.pheno%>% 
  group_by(ID, phase, year) %>% 
  slice(which.min(doy))
stan.bb$m.doy<-ave(stan.bb$doy, stan.bb$phase, stan.bb$Genus, stan.bb$year)
stan.bb<-stan.bb[!duplicated(stan.bb),]
stan.bb$photo<-stan.bb$photo/3600
x<-paste(stan.bb$year, stan.bb$doy)
stan.bb$date<-as.Date(strptime(x, format="%Y %j"))
stan.bb$photo<-ifelse(stan.bb$year==2018, daylength(stan.bb$lat, stan.bb$date), stan.bb$photo)

### Clean observation error!
stan.bb$doy<-ifelse(stan.bb$Species=="alleghaniensis"&stan.bb$year==2016&stan.bb$doy==59, NA, stan.bb$doy)
stan.bb<-stan.bb[!is.na(stan.bb$doy),]

## Now bring in climate data...
cc<-read.csv("input/weldhill.csv", header=TRUE)
cc<-cc%>%
  rename(date.time=Eastern.daylight.time)
cc$date<-gsub("\\s* .*$", '', cc$date.time)
cc$date<- as.Date(cc$date, "%m/%d/%Y")
cc$year<-substr(cc$date, 0, 4)
cc$doy<-yday(cc$date)
cc$hour<-gsub("^.* \\s*|\\s*:.*$", '', cc$date.time)

cc<-dplyr::select(cc, Temp..F,
                  Rain.in, date, year, doy, hour)
cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
cc$tmin<-fahrenheit.to.celsius(cc$tmin)
cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
cc$tmax<-fahrenheit.to.celsius(cc$tmax)
cc$tmean<-ave(cc$Temp..F, cc$date)
cc$tmean<-fahrenheit.to.celsius(cc$tmean)
cc$tchill<-ave(cc$Temp..F, cc$date)
cc$tchill<-fahrenheit.to.celsius(cc$tchill)

cc$Rain.in<-ave(cc$Rain.in, cc$date, FUN=sum)
cc$precip<-conv_unit(cc$Rain.in, "inch", "mm")





