### 19 April 2019 - by Cat ###
## Let's play around with some Tree Spotters data!
## Find Provenance Lat info and Age!
## Updated 19 Nov 2018 with new TS data & Climate data
### Weather data downloaded from... http://labs.arboretum.harvard.edu/weather/

## NOTE! 20 November 2018 - need to make sure the data is cleaning

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
#stan.bb$photo<-stan.bb$photo/3600
#x<-paste(stan.bb$year, stan.bb$doy)
#stan.bb$date<-as.Date(strptime(x, format="%Y %j"))
stan.bb$photo<-as.numeric(geosphere::daylength(stan.bb$lat, stan.bb$doy))

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
bbx$last.obs<-ave(bbx$`leaf drop`, bbx$ID, bbx$year, FUN=last)
bbx$last.obs<-ifelse(is.na(bbx$last.obs), ave(bbx$col.leaves, bbx$ID, bbx$year, FUN=last), bbx$last.obs)


bbx$gdd.start<-46 # 15 February for each year - arbitrary, can change
### How do I want to calculate dvr? Chilling should be the same, diff photo for leafout and forcing between bb and lo or agdd until bb and then till lo?

dxx<-bbx[!is.na(bbx$budburst),]
dxx<-dxx[!is.na(dxx$leafout),]
dxx<-dxx[!is.na(dxx$last.obs),]
#bb.start<-bb.start[!is.na(bb.start$flowers),]
#bb.start<-bb.start[!is.na(bb.start$leafdrop),]
#bb.start<-bb.start[!is.na(bb.start$fruits),]

dxx$id_year<-paste(dxx$ID, dxx$year)

force<-data_frame()
days.btw<-array()

days.btw <- Map(seq, dxx$gdd.start, dxx$leafout, by = 1)

force <- data.frame(id_year = rep.int(dxx$id_year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))

force$ID<-as.integer(substr(force$id_year, 0, 6))
force$year<-as.integer(substr(force$id_year, 7,11))

force<-force[!duplicated(force),]
force<-dplyr::select(force, -id_year)
force$leafout<-ave(force$doy, force$ID, force$year, FUN=max)

force$gdd.start<-ave(force$doy, force$ID, force$year, FUN=min)


cc<-dplyr::select(cc, year, doy, tmean)

force<-inner_join(force, cc)
force<-force[!duplicated(force),]

foober<-dxx%>%dplyr::select(Genus, Species, ID, year, elev, lat, long,budburst)
force<-full_join(force, foober)

force$tmeanbb<-ifelse(force$doy>=force$gdd.start & force$doy<=force$budburst, force$tmean, force$force)
force$force<-ave(force$tmeanbb, force$ID, force$year)
force$tmeanlo<-ifelse(force$doy>=force$gdd.start & force$doy<=force$leafout, force$tmean, force$force)
force$force.lo<-ave(force$tmeanlo, force$ID, force$year)
force$gdd.bb<-ifelse(force$doy>=force$gdd.start & force$doy<=force$budburst & force$tmean>0, force$tmean, 0)
force$gdd.bb<-ave(force$gdd.bb, force$ID, force$year, FUN=sum)
force$gdd.lo<-ifelse(force$doy>=force$gdd.start & force$doy<=force$leafout & force$tmean>0, force$tmean, 0)
force$gdd.lo<-ave(force$gdd.lo, force$ID, force$year, FUN=sum)

dxx$yrend<-ifelse(dxx$year==2016, 412, 411)

days.btw <- Map(seq, dxx$last.obs, dxx$yrend, by = 1)
chilldays <- data.frame(id_year = rep.int(dxx$id_year, vapply(days.btw, length, 1L)), 
                      doy2 = do.call(c, days.btw))

chilldays$ID<-as.integer(substr(chilldays$id_year, 0, 6))
chilldays$year<-as.integer(substr(chilldays$id_year, 7,11))

chilldays<-full_join(chilldays, foober)

chilldays$year2<-chilldays$year
chilldays$doy<-ifelse(chilldays$year==2016 & chilldays$doy2>366, chilldays$doy2-366, chilldays$doy2)
chilldays$year<-ifelse(chilldays$year==2016 & chilldays$doy2>366, 2017, chilldays$year)
chilldays$doy<-ifelse(chilldays$year!=2016 & chilldays$doy2>365, chilldays$doy2-365, chilldays$doy2)
chilldays$year<-ifelse(chilldays$year2!=2016 & chilldays$doy2>365, chilldays$year2 + 1, chilldays$year)

chilldays<-chilldays[!duplicated(chilldays),]
chilldays<-dplyr::select(chilldays, -id_year)

chilldays$chill.start<-ave(chilldays$doy2, chilldays$ID, chilldays$year, FUN=min)
chilldays$chill.end<-ave(chilldays$doy2, chilldays$ID, chilldays$year, FUN=max)

cc<-dplyr::select(cc, year, doy, tmean)

chilldays<-inner_join(chilldays, cc)
chilldays<-chilldays[!duplicated(chilldays),]


chilldays$chill<-NA
chilldays$chill<-ifelse(chilldays$doy2>=chilldays$chill.start, ave(chilldays$tmean, chilldays$ID, chilldays$year), chilldays$chill)


chilldays$achill<-ifelse(chilldays$doy2>=chilldays$chill.start & chilldays$doy2<=chilldays$chill.end & chilldays$tmean>=0 & chilldays$tmean<=5, chilldays$tmean, 0)
chilldays$achill<-ave(chilldays$achill, chilldays$ID, chilldays$year, FUN=sum)

force<-dplyr::select(force, -tmean, -doy, -gdd.start)
chilldays<-dplyr::select(chilldays, -doy2, -year2, -tmean, -doy, -chill.start, -chill.end)
tree<-full_join(force, chilldays)

tree<-tree[!duplicated(tree),]
#tree<-na.omit(tree)

photos<-data_frame()
days.btw<-array()

days.btw <- Map(seq, dxx$gdd.start, dxx$leafout, by = 1)

photos <- data.frame(id_year = rep.int(dxx$id_year, vapply(days.btw, length, 1L)), 
                    doy = do.call(c, days.btw))

photos$ID<-as.integer(substr(photos$id_year, 0, 6))
photos$year<-as.integer(substr(photos$id_year, 7,11))

photos<-full_join(photos, foober)

photos<-photos[!duplicated(photos),]
photos<-dplyr::select(photos, -id_year)
photos$leafout<-ave(photos$doy, photos$ID, photos$year, FUN=max)

photos$pho.start<-ave(photos$doy, photos$ID, photos$year, FUN=min)
goo<-stan.bb%>% ungroup(phase, ID, year) %>%
  dplyr::select(ID, year, doy, photo)
photos<-full_join(photos, goo)

photos$m.photo.bb<-ifelse(photos$doy>=photos$pho.start & photos$doy<=photos$budburst, ave(photos$photo, photos$ID, photos$year), photos$photo)
photos$m.photo.lo<-ifelse(photos$doy>=photos$pho.start & photos$doy<=photos$budburst, ave(photos$photo, photos$ID, photos$year), photos$photo)
photos$photo.bb<-ifelse(photos$doy>=photos$pho.start & photos$doy<=photos$budburst, ave(photos$photo, photos$ID, photos$year, FUN=sum), photos$photo)
photos$photo.lo<-ifelse(photos$doy>=photos$pho.start & photos$doy<=photos$leafout, ave(photos$photo, photos$ID, photos$year, FUN=sum), photos$photo)


photos<-photos%>%dplyr::select(-doy, -pho.start)
tree<-full_join(tree, photos)
tree<-tree[!duplicated(tree),]

