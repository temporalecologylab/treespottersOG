# Tree Spotters Attempt at Multilevel Regression
# 24 June 2016
# data was downloaded initially from NPN Data Downloader for the 
# TreeSpotters group. Use Tree Spotters Data.R script for reference

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory

graphics.off()

# Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "plyr", "dplyr", "tidyr", "tidytext",
              "purrr", "Hmisc", "lme4")
ipak(packages)


## First Attempt at Mixed Models
d<-data.frame(a = 1,0,-1) # to make two level rather than three
options("contrasts")
model.matrix(~ a, d)

nobs = 54
ntree = 55
nphase = 11
nspp = 11
nday = 172
nresp = 3

rep = 999

ntot = nobs*ntree*nphase*nspp*nday*nresp

tree = gl(ntree, rep, length = ntot)
spp = gl(nspp, rep*ntree, length = ntot)

resp = gl(nresp, rep*ntree*nspp, length = ntot)
obvs = gl(nobs, rep*ntree*nspp*nresp, length = ntot)
phase = gl(nphase, rep*ntree*nspp*nresp*nobs, length = ntot)

d <- data.frame(tree, spp, resp, obvs, phase)

setwd("~/Documents/Temporal Ecology/TreeSpotters")
observers<-read.csv("obs.npn.csv",header=TRUE,sep=",")
attach(observers)

dd<-factor(observers$Phenophase_Status) # to make two level rather than three
is.factor(dd) 
resp<-contrasts(dd)

date.t <- as.Date(
  observers$Observation_Date, "%m/%d/%y")
obs1<-observers%>%
  filter(date.t >= "2016/01/01")

test<-lmer(Phenophase_Status~Individual_ID + (1|Phenophase_ID) +
             (1|Day_of_Year), data = obs1)
test1<-glm(Phenophase_Status~Individual_ID + ObservedBy_Person_ID
           + Phenophase_ID + Day_of_Year, data = obs1)
summary(test)
summary(test1)
test2<-glm(Phenophase_Status~Common_Name*ObservedBy_Person_ID
           + Phenophase_ID + Day_of_Year, data = obs1)
summary(test2)

test3<-lmer(Day_of_Year~Common_Name + ObservedBy_Person_ID*
              Phenophase_Status*Phenophase_ID + 
              (ObservedBy_Person_ID|Individual_ID) + 
              (Phenophase_Status|Individual_ID) + 
              (Phenophase_ID|Individual_ID), data = obs1)
summary(test3)
qplot(resp, Common_Name, data = obs1, 
      geom = "boxplot", color=Common_Name)
