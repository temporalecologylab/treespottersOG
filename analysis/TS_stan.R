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
bb<-dplyr::select(bb, doy, spp, photo, gdd, ID, year, aprecip, solr)
bb$gdd<-ave(bb$gdd, bb$ID, bb$year, FUN=max)

bb<-bb[!duplicated(bb),]

bb$cgdd<-bb$gdd/20
bb$cprecip<-bb$aprecip/10
bb.stan<-stan_glmer(doy~photo+cgdd+cprecip+photo:cgdd+photo:cprecip+cgdd:cprecip+
                    (1|spp), data=bb, family=poisson)




