## Cat - 27 March 2017
# Look at the retention rate for Tree Spotters
# Data downloaded from: http://data.usanpn.org/observations
# Parameters: Observerations made in 2016, multiple days

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)

# Set Working Directory
setwd("~/Documents/git/treespotters/Data")
d<- read.csv("ts.observers.2016.csv", header=TRUE)

# Subset dataset down to observers and DOY
obs<- d %>%
  dplyr::select(ObservedBy_Person_ID, First_Yes_DOY) %>%
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)
obs[] <- lapply(obs, gsub, pattern="'", replacement="")

total<-as.data.frame(table(obs$ObservedBy_Person_ID))
total<- total %>%
  rename(Num=Freq)%>%
  rename(obs=Var1) 
total$Num<-ifelse(total$Num>1, total$Num, NA)
total<-na.omit(total)
