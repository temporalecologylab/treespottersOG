## Tree Spotters update
# Cat - 21 November 2017

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/treespotters")
d<-read.csv("treespotters/individual_phenometrics_data.csv",header=TRUE)

d<-d%>%
  dplyr::select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_Year, First_Yes_DOY)
d<-d[!duplicated(d),]
df<-d%>%
  group_by(Individual_ID, First_Yes_Year, Phenophase_Description)%>%
  filter(row_number()==1)%>%
  rename(year=First_Yes_Year)%>%
  rename(doy=First_Yes_DOY)%>%
  rename(pheno=Phenophase_Description)%>%
  rename(id=Individual_ID)

df$gen<-substr(df$Genus, 1, 3)
df$sp<-substr(df$Species,1, 3)
df$species<-paste(df$gen, df$sp, sep="")

df$mean<-ave(df$doy, df$pheno, df$year, df$species)

df<-ungroup(df)
dx<-df%>%
  dplyr::select(species, pheno, year, mean)
dx<-dx[!duplicated(dx),]

phases<-c("Breaking leaf buds", "Leaves", "Flowers or flower buds")
dx<-filter(dx, pheno%in%phases)

dx<-filter(dx, year>=2016)
dvr<-dx
dvr$pheno<-ifelse(dvr$pheno=="Breaking leaf buds", "bb", dvr$pheno)
dvr$pheno<-ifelse(dvr$pheno=="Leaves", "lo", dvr$pheno)
dvr$pheno<-ifelse(dvr$pheno=="Flowers or flower buds", "flo", dvr$pheno)
dvr<-spread(dvr, pheno, mean)
dvr$risk<-dvr$lo-dvr$bb
dvr$hys<-ifelse(dvr$lo<=dvr$flo, "pro", "hys")

ggplot(dx, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~species)

fit1<-stan_glm(risk~bb+as.factor(year), data=dvr)
fit1

ggplot(dvr, aes(x=species, y=risk)) + geom_point(aes(color=as.factor(year))) + ylab("Frost Risk") +
  xlab("Species")

dx$mean<-ifelse(dx$pheno=="Leaves" & dx$species=="Popdel" & dx$year==2017, 110, dx$mean)

dx.r<-filter(dx, pheno!="Flowers or flower buds")
ggplot(dx.r, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~as.factor(year)) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species")

dx.h<-filter(dx, pheno!="Breaking leaf buds")
ggplot(dx.h, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~as.factor(year)) + 
  coord_flip() + ylab("Day of Year") + xlab("Species")



