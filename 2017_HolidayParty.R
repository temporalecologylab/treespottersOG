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
library(RColorBrewer)
library(egg)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis/input")
b<-read.csv("individual_phenometrics_data.csv",header=TRUE)

### Cheap cleaning method to remove probable errors...
#d<-b[(b$First_Yes_DOY>=60),]
#d[] <- lapply(d, gsub, pattern="'", replacement="")
#d<-d[!(d$ObservedBy_Person_ID=='26122' & d$First_Yes_Year==2017 & d$First_Yes_DOY==85),]
#d<-d[!(d$Species=="alba" & d$First_Yes_DOY == 66 & d$Phenophase_Description=="Leaves"),]
d<-b[(b$Multiple_Observers>0),]

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

df$doy<-as.numeric(df$doy)
df$mean<-ave(df$doy, df$pheno, df$year, df$species)
#df$mean<-ifelse(df$pheno=="Leaves" & df$species=="Popdel" & df$year==2017, 110, df$mean)

df<-ungroup(df)
dx<-df%>%
  dplyr::select(species, pheno, year, mean)
dx<-dx[!duplicated(dx),]

phases<-c("Breaking leaf buds", "Leaves", "Flowers or flower buds")
dx<-filter(dx, pheno%in%phases)

dx<-filter(dx, year>=2016)

dx$species<-ifelse(dx$species=="Acesac", "Acer saccharum", dx$species)
dx$species<-ifelse(dx$species=="Aesfla", "Aesculus flava", dx$species)
dx$species<-ifelse(dx$species=="Betall", "Betula alleghaniensis", dx$species)
dx$species<-ifelse(dx$species=="Betnig", "Betula nigra", dx$species)
dx$species<-ifelse(dx$species=="Cargla", "Carya glabra", dx$species)
dx$species<-ifelse(dx$species=="Carova", "Carya ovata", dx$species)
dx$species<-ifelse(dx$species=="Faggra", "Fagus grandifolia", dx$species)
dx$species<-ifelse(dx$species=="Popdel", "Populus deltoides", dx$species)
dx$species<-ifelse(dx$species=="Quealb", "Quercus alba", dx$species)
dx$species<-ifelse(dx$species=="Querub", "Quercus rubra", dx$species)
dx$species<-ifelse(dx$species=="Tilame", "Tilia americana", dx$species)

dvr<-dx
dvr$pheno<-ifelse(dvr$pheno=="Breaking leaf buds", "bb", dvr$pheno)
dvr$pheno<-ifelse(dvr$pheno=="Leaves", "lo", dvr$pheno)
dvr$pheno<-ifelse(dvr$pheno=="Flowers or flower buds", "flo", dvr$pheno)
dvr<-spread(dvr, pheno, mean)
dvr$lo<-as.numeric(dvr$lo)
dvr$bb<-as.numeric(dvr$bb)
dvr$risk<-dvr$lo-dvr$bb
dvr$hys<-ifelse(dvr$lo<=dvr$flo, "pro", "hys")


#ggplot(dx, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~species)

#fit1<-stan_glm(risk~bb+as.factor(year), data=dvr)
#fit1

cols <- colorRampPalette(brewer.pal(8,"Spectral"))(3)

#dvr<-dvr[!(dvr$species=="Fagus grandifolia" & dvr$year==2018)]
dvr$species<-reorder(dvr$species, dvr$risk)

frost<- ggplot(dvr, aes(x=species, y=risk)) + geom_point(aes(color=as.factor(year))) + ylab("Frost Risk") +
  xlab("Species") + geom_line(aes(x=species, y=risk, color=as.factor(year), group=as.factor(year))) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        axis.text=element_text(size=10), legend.key = element_rect(fill = "transparent"),
        legend.box.background = element_rect(), legend.position = c(0.15, 0.85)) + scale_color_manual(values=cols, 
                                                              labels=c("2016","2017","2018"),
                                                              name="Year") + scale_y_continuous(limits=c(0, 30), expand=c(0,0))
  


dx.r<-filter(dx, pheno!="Flowers or flower buds")
xx<-subset(dx.r, pheno=="Breaking leaf buds")
xx$bb.yr<-ave(xx$mean, xx$year)
xx<-dplyr::select(xx, bb.yr, year)
xx<-xx[!duplicated(xx),]
#dx.r<-inner_join(dx.r, xx)

####### Stop here Jun 19, 2018 - issues with mapping budburst date #########

ggplot(dx.r, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~year) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=10), legend.key = element_rect(fill = "transparent"),
                                                                            legend.box.background = element_rect(),
                                                                            panel.spacing = unit(2, "lines")) + labs(col="Phenophase") + geom_vline(aes(xintercept = bb.yr), xx, color="green")


sixteen<-subset(dx.r, dx.r$year==2016)
six<-ggplot(sixteen, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=10), legend.position = "none")
seventeen<-subset(dx.r, dx.r$year==2017)
seven<-ggplot(seventeen, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=10), legend.position="none")
eighteen<-subset(dx.r, dx.r$year==2018)
eight<-ggplot(eighteen, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=10), legend.key = element_rect(fill = "transparent"),
                                                                            legend.box.background = element_rect()) + labs(col="Phenophase")
ggarrange(six, seven, eight, ncol=3, nrow=1)


dx.h<-filter(dx, pheno!="Breaking leaf buds")
ggplot(dx.h, aes(x=species, y=mean)) + geom_point(aes(color=pheno)) + facet_wrap(~as.factor(year)) + 
  coord_flip() + ylab("Day of Year") + xlab("Species")

df<-dplyr::select(d, ObservedBy_Person_ID, First_Yes_Year)
df<-filter(df, First_Yes_Year>2016)
dx<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)


dx<-dx[!duplicated(dx),]
length(unique(dx$ObservedBy_Person_ID))
                 #[1] 73

