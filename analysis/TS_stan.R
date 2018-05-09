## Cat - 24 April 2018 ##
## Now some stan models looking at Tree Spotters data

# ## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstanarm)
library(brms)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis")

## Data!
d<-read.csv("output/tree_rf_data.csv", header=TRUE)

## Budburst
bb<-subset(d, phase=="budburst")
bb<-dplyr::select(bb, doy, spp, photo, gdd, ID, year, aprecip, solr, chill, frz, tdiff, tmean, tmax, tmin, frost)

bb$cgdd<-bb$gdd/20
bb$cprecip<-bb$aprecip/20
bb$cchill<-bb$chill/50
bb$cfrz<-bb$frz/20
bb$csolr<-bb$solr/20
bb$cyear<-as.numeric(substr(bb$year, 3,4))

bb.stan<-stan_glmer(doy~photo+cgdd+cprecip+cchill+
                    (1|spp), data=bb)

#bb.stan<-stan_glmer(doy~solr+tmean+tmin+tmax+year+(1|spp), data=bb)

#bb.combo<-stan_glmer(doy~photo+cgdd+cprecip+csolr+tmean+tdiff+(1|spp), data=bb)

bb.inter<-stan_glmer(doy~photo+cgdd+cchill+photo:cgdd+photo:cchill+cgdd:cchill+cprecip+
                       (1|spp), data=bb)

bb.brm<-brm(doy~cgdd+photo+cprecip+csolr+(1|spp) + (cgdd-1|spp) +
              (photo-1|spp) + (cprecip-1|spp) + (csolr-1|spp), data=bb)

## Leafout
lo<-subset(d, phase=="leafout")
lo<-dplyr::select(lo, doy, spp, photo, gdd, ID, year, aprecip, solr, chill, frz, tdiff, tmean, tmax, tmin, frost)

lo<-lo[!duplicated(lo),]

lo$cgdd<-lo$gdd/20
lo$cprecip<-lo$aprecip/20
lo$cchill<-lo$chill/50
lo$cfrz<-lo$frz/20
lo$csolr<-lo$solr/20
lo$cyear<-as.numeric(substr(lo$year, 3,4))

lo$doy<-ave(lo$doy, lo$ID, lo$year, FUN=first)
#lo$resp<-ave(lo$doy, lo$spp, lo$year)

#lo<-dplyr::select(lo, -ID)
#lo<-lo[!duplicated(lo),]

lo.stan<-stan_glmer(resp~photo+cgdd+cprecip+cchill+
                      (1|spp), data=lo)

lo.stan<-stan_glmer(doy~cgdd+frost+cchill+cgdd:cchill+(1|spp), data=lo)

lo.brm<-brm(resp~photo+cgdd+cprecip+cchill+(1|spp), data=lo)

