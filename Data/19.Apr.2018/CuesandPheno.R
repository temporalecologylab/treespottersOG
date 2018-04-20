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
library(geosphere)
library(anytime)
library(weathermetrics)
library(measurements)


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
stan.bb<-bb.pheno%>% 
  group_by(ID, phase, year) %>% 
  slice(which.min(doy))
stan.bb$m.doy<-ave(stan.bb$doy, stan.bb$phase, stan.bb$Genus, stan.bb$year)
stan.bb<-stan.bb[!duplicated(stan.bb),]
stan.bb$photo<-stan.bb$photo/3600
x<-paste(stan.bb$year, stan.bb$doy)
stan.bb$date<-as.Date(strptime(x, format="%Y %j"))
stan.bb$photo<-ifelse(stan.bb$year==2017, daylength(stan.bb$lat, stan.bb$date), stan.bb$photo)

## Now bring in climate data...
cc<-read.csv("19.Apr.2018/weldhill.csv", header=TRUE)
cc<-cc%>%
  rename(date.time=Eastern.daylight.time)
cc$date<-substr(cc$date.time, 0, 9)
cc$date<-ifelse(grepl('1$', cc$date), substr(cc$date.time, 0, 10), cc$date)
cc$date<-ifelse(grepl(' $', cc$date), substr(cc$date.time, 0, 8), cc$date)
cc$date<- as.Date(cc$date, "%m/%d/%Y")
cc$year<-substr(cc$date, 0, 4)
cc$doy<-yday(cc$date)
cc<-dplyr::select(cc, Solar.Rad.W.m.2, Wind.Speed.mph, Wind.Dir, Temp..F, Dewpt..F, Temp.Soil..F,
                  Rain.in, Pressure.inHg, date, year, doy)
cc$tmin<-ave(cc$Temp..F, cc$date, FUN=min)
cc$tmin<-fahrenheit.to.celsius(cc$tmin)
cc$tmax<-ave(cc$Temp..F, cc$date, FUN=max)
cc$tmax<-fahrenheit.to.celsius(cc$tmax)
cc$tmean<-ave(cc$Temp..F, cc$date)
cc$tmean<-fahrenheit.to.celsius(cc$tmean)

cc$tmin.soil<-ave(cc$Temp.Soil..F, cc$date, FUN=min)
cc$tmin.soil<-fahrenheit.to.celsius(cc$tmin.soil)
cc$tmax.soil<-ave(cc$Temp.Soil..F, cc$date, FUN=max)
cc$tmax.soil<-fahrenheit.to.celsius(cc$tmax.soil)
cc$tmean.soil<-ave(cc$Temp.Soil..F, cc$date)
cc$tmean.soil<-fahrenheit.to.celsius(cc$tmean.soil)

cc$dewpt<-fahrenheit.to.celsius(ave(cc$Dewpt..F, cc$date))
cc$Rain.in<-ave(cc$Rain.in, cc$date, FUN=sum)
cc$precip<-conv_unit(cc$Rain.in, "inch", "mm")

sm.cc<-cc%>%
  rename(solr=Solar.Rad.W.m.2)%>%
  rename(windsp=Wind.Speed.mph)%>%
  rename(pressure=Pressure.inHg)

sm.cc$solr<-ave(sm.cc$solr, sm.cc$date)
sm.cc$windsp<-ave(sm.cc$windsp, sm.cc$date)
sm.cc$pressure<-ave(sm.cc$pressure, sm.cc$date)

sm.cc<-dplyr::select(sm.cc, -Temp..F, -Temp.Soil..F, -Dewpt..F, -Rain.in, -Wind.Dir)
stan.cc<-sm.cc[!duplicated(sm.cc),]
  

### Let's join em up!
stan.cc$year<-as.numeric(stan.cc$year)
stan.cc$doy<-as.numeric(stan.cc$doy)
stan.bb$doy<-as.numeric(stan.bb$doy)
stan.bb<-rename(stan.bb,rain=precip)

stan.tree<-inner_join(stan.bb, stan.cc)
#stan.tree$tmax.soil<-ifelse(stan.tree$tmax.soil<0, NA, stan.tree$tmax.soil)
#stan.tree$tmin.soil<-ifelse(stan.tree$tmin.soil<0, NA, stan.tree$tmin.soil)

stan.tree<-ungroup(stan.tree)
tree<-dplyr::select(stan.tree, Genus, Species, phase, lat, elev, year, doy, photo, 
                    date, solr, windsp, pressure, tmin, tmax, tmean, dewpt, precip)

tree$spp<-paste(tree$Genus, tree$Species)
tree$spp<-as.numeric(as.factor(tree$spp))
tree.bb<-subset(tree, phase=="budburst")
tree.bb<-dplyr::select(tree.bb, doy, spp, lat, elev, year, photo, solr, windsp, pressure, tmin,
                       tmax, tmean, dewpt, precip)
tree.bb<-na.omit(tree.bb)
tree.bb$lat<-as.numeric(tree.bb$lat)
tree.bb$elev<-as.numeric(tree.bb$elev)
tree.bb$photo<-as.numeric(tree.bb$photo)
tree.bb$solr<-as.numeric(tree.bb$solr)
tree.bb$windsp<-as.numeric(tree.bb$windsp)
tree.bb$pressure<-as.numeric(tree.bb$pressure)
tree.bb$tmin<-as.numeric(tree.bb$tmin)
tree.bb$tmax<-as.numeric(tree.bb$tmax)
tree.bb$tmean<-as.numeric(tree.bb$tmean)
tree.bb$dewpt<-as.numeric(tree.bb$dewpt)
tree.bb$precip<-as.numeric(tree.bb$precip)


fit<-randomForest(doy ~ spp+ lat + elev + year + photo + solr + windsp + pressure + tmin +
                  tmax + tmean + dewpt + precip, data=tree.bb)


