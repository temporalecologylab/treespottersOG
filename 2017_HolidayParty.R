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

cols <- colorRampPalette(brewer.pal(3,"Set1"))(3)

#dvr<-dvr[!(dvr$species=="Fagus grandifolia" & dvr$year==2018)]
dvr$code<-reorder(dvr$species, dvr$risk)

quartz()
frost<- ggplot(dvr, aes(x=code, y=risk)) + geom_point(aes(color=as.factor(year))) + ylab("Frost Risk") +
  xlab("Species") + geom_line(aes(x=code, y=risk, color=as.factor(year), group=as.factor(year))) +
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
dx.r<-inner_join(dx.r, xx)

####### Stop here Jun 19, 2018 - issues with mapping budburst date #########
dx.r$code<-reorder(dx.r$species, dx.r$mean)
dvr2<-ggplot(dx.r, aes(x=code, y=mean)) + geom_point(aes(color=pheno)) +
  geom_line() + coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                            legend.box.background = element_rect(),
                                                                            panel.spacing = unit(2, "lines"),
                                                                            strip.background = element_rect(fill="transparent"),
                                                                            strip.text = element_text(size=14)) + labs(col="Phenophase") + 
  facet_wrap(~year) + geom_hline(aes(yintercept=bb.yr), xx, col="forestgreen", linetype="dashed")


dx.h<-filter(dx, pheno!="Breaking leaf buds")
hys<-dplyr::select(dvr, species, year, hys)
dx.h<-inner_join(dx.h, hys)
dx.h<-dx.h[!(dx.h$species=="Fagus grandifolia" & dx.h$year==2018),]

colors<-colorRampPalette(brewer.pal(8,"Spectral"))(2)
ggplot(dx.h, aes(x=species, y=mean)) + geom_point(aes(col=hys)) + geom_line(aes(col=hys)) +
  coord_flip() + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                              axis.text.y = element_text(face = "italic"),
                                                              axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                              legend.box.background = element_rect(),
                                                              panel.spacing = unit(2, "lines"),
                                                              strip.background = element_rect(fill="transparent"),
                                                              strip.text = element_text(size=14)) + labs(col="Phenophase") + 
  facet_wrap(~year) + geom_hline(aes(yintercept=bb.yr), xx, col="forestgreen", linetype="dashed") + scale_color_manual(values=colors, 
                                                                                                                     labels=c(hys="Hysteranthy",
                                                                                                                              pro="Proteranthy"),
                                                                                                                     name="")

df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2016)
dsix<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)


dsix<-dsix[!duplicated(dsix),]
length(unique(dsix$ObservedBy_Person_ID))
                    # 62 = 2016

df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2017)
dseven<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)


dseven<-dseven[!duplicated(dseven),]
length(unique(dseven$ObservedBy_Person_ID))
                  # 74 = 2017


df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2018)
deight<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)


deight<-deight[!duplicated(deight),]
length(unique(deight$ObservedBy_Person_ID))
                # 58 = 2016


obs<-full_join(dsix, dseven)
obs<-full_join(obs, deight)
obs<-obs[!duplicated(obs),]

tt<-as.data.frame(table(obs$First_Yes_Year, obs$ObservedBy_Person_ID))
tt$years<-ave(
  tt$Freq, tt$Var2, 
  FUN=function(x) cumsum(c(1, head(x, -1)))
)

t18<-subset(tt, Var1==2018)
t18<-t18[(t18$Freq>0),]
table(t18$years)









