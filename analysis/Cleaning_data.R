### 19 April 2019 - by Cat ###
## Let's play around with some Tree Spotters data!
## Find Provenance Lat info and Age!
## Updated 19 Nov 2018 with new TS data & Climate data
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
bb.pheno<-dplyr::select(bb, Genus, Species, Common_Name, phase, lat, long, elev, year, doy, numYs, photo, ID, AGDD)
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
cc$date<-gsub("001", "201", cc$date)
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

cc$year<-as.integer(cc$year)

cc<-dplyr::select(cc, -hour)
cc<-cc[!duplicated(cc),]
#bbx<-left_join(bb, cc)

#### Now start building a small data frame with phenophase info then add in forcing, chilling and photo
colstokeep<-c("Genus", "Species", "ID","year", "phase","lat", "long", "elev", "doy")
bbx<-subset(stan.bb, select=colstokeep)
bbx<-bbx[!duplicated(bbx),]

bbx<-bbx%>%tidyr::spread(phase, doy)
bbx$budburst<-ave(bbx$budburst, bbx$ID, bbx$year, FUN=first)
bbx$leafout<-ave(bbx$leafout, bbx$ID, bbx$year, FUN=first)
bbx$flowers<-ave(bbx$flowers, bbx$ID, bbx$year, FUN=first)
bbx$fruits<-ave(bbx$Fruits, bbx$ID, bbx$year, FUN=first)
bbx$col.leaves<-ave(bbx$`Colored leaves`, bbx$ID, bbx$year, FUN=first)
bbx$leafdrop<-ave(bbx$`leaf drop`, bbx$ID, bbx$year, FUN=first)
bbx$last.obs<-ave(bbx$`leaf drop`, bbx$ID, bbx$year, FUN=first)


bbx$gdd.start<-46 # 15 February for each year - arbitrary, can change
### How do I want to calculate dvr? Chilling should be the same, diff photo for leafout and forcing between bb and lo or agdd until bb and then till lo?

bb.start<-bbx[!is.na(bbx$budburst),]
bb.start<-bb.start[!is.na(bb.start$leafout),]
#bb.start<-bb.start[!is.na(bb.start$flowers),]
#bb.start<-bb.start[!is.na(bb.start$leafdrop),]
#bb.start<-bb.start[!is.na(bb.start$fruits),]

bb.start$id_year<-paste(bb.start$ID, bb.start$year)

dxx<-data_frame()
days.btw<-array()
for(i in length(bb.start$id_year)){
  days.btw[i] <- Map(seq, bb.start$gdd.start[i], bb.start$budburst[i], by = 1)
  dxx <- data.frame(ID=bb.start$ID, year=bb.start$year, Genus=bb.start$Genus, Species=bb.start$Species, 
                    lat=bb.start$lat, long=bb.start$long, elev=bb.start$elev,
                    id_year = rep.int(bb.start$id_year, vapply(days.btw[i], length, 1L)), 
                    doy = do.call(c, days.btw[i]))
}

dxx<-dxx[!duplicated(dxx),]
dxx<-dplyr::select(dxx, -id_year)
dxx$budburst<-ave(dxx$doy, dxx$ID, dxx$year, FUN=max)

dxx$gdd.start<-ave(dxx$doy, dxx$ID, dxx$year, FUN=min)

cc<-dplyr::select(cc, year, doy, tmean, precip)

dxx<-inner_join(dxx, cc)
dxx<-dxx[!duplicated(dxx),]

dxx$force<-NA
dxx$force<-ifelse(dxx$doy>=dxx$gdd.start & dxx$doy<=dxx$budburst, ave(dxx$tmean, dxx$ID, dxx$year), dxx$force)
dxx$gdd<-ifelse(dxx$tmean>0, dxx$tmean, 0)
dxx$gdd<-ave(dxx$gdd, dxx$ID, dxx$year, FUN=sum)

pb<-txtProgressBar(min=1, max=nrow(bb.start), style=3)
bb.start$agdd<-NA
for(i in c(1:nrow(bb.start))){
  for(j in c(1:nrow(cc))) {
  vector<-ifelse(bb.start$year[i]==cc$year[j] & cc$doy[j]<=bb.start$budburst[i] & cc$doy[j]>=bb.start$gdd.start[i], cc$tmean[j], 0)
  bb.start$agdd[i]<-sum(vector)
  setTxtProgressBar(pb, i)
  }
}

bb.start$force<-NA
for(i in c(1:nrow(bb.start))){
  for(j in c(1:nrow(cc))) {
    bb.start$force[i]<-ifelse(bb.start$year[i]==cc$year[j] & cc$doy[j]<=bb.start$budburst[i] & cc$doy[j]>=bb.start$gdd.start[i], ave(cc$tmean), bb.start$force)
    setTxtProgressBar(pb, i)
  }
}

bb.start$achill<-NA
for(i in c(1:nrow(bb.start))){
  for(j in c(1:nrow(cc))) {
    v<-ifelse(bb.start$year[i]==cc$year[j]+1 & cc$doy[j]>=bb.start$last.obs[i] & ccx$tchill[j]>=0 &ccx$tchill[j]<=5, cc$tchill[j], 0)
    v2<-ifelse(bb.start$year[i]==cc$year[j] & cc$doy[j]<=bb.start$gdd.start[i] & ccx$tchill[j]>=0 &ccx$tchill[j]<=5, cc$tchill[j], 0)
    vector<-v+v2
    bb.start$achill[i]<-sum(vector)
  }
}

bb.start$chill<-NA
for(i in c(1:nrow(bb.start))){
  for(j in c(1:nrow(cc))) {
    v<-ifelse(bb.start$year[i]==cc$year[j]+1 & cc$doy[j]>=bb.start$last.obs[i], ave(cc$tchill[j]), bb.start$chill)
    v2<-ifelse(bb.start$year[i]==cc$year[j] & cc$doy[j]<=bb.start$gdd.start[i], ave(cc$tchill[j]), bb.start$chill)
    bb.start$chill[i]<- (v + v2)/2
  }
}




