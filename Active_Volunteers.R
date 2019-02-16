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

newspp <- c("Acerub", "Hamvir", "Vaccor", "Vibnud")
dx <- dx[!(dx$species %in% newspp),]

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
cols <- colorRampPalette(brewer.pal(3, "Dark2"))(3)
dx.r$colz <-ifelse(dx.r$pheno=="Breaking leaf buds", "salmon3", "royalblue3")
dvr2016<-ggplot(dx.r[(dx.r$year==2016),], aes(x=code, y=mean)) + geom_point(aes(color=colz,shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                            axis.text.y = element_text(face = "italic"),
                                                                            axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                            legend.box.background = element_rect(),
                                                                            panel.spacing = unit(2, "lines"),
                                                                            plot.title = element_text(color="#1B9E77"),
                                                                            legend.position = "none",
                                                                  axis.title.y = element_blank()) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), xx[(xx$year==2016),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2016") +
  scale_y_continuous(breaks=seq(min(100), max(140), by=10)) + coord_flip(ylim=c(100,140)) 

dvr2017<-ggplot(dx.r[(dx.r$year==2017),], aes(x=code, y=mean)) + geom_point(aes(color=colz,shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + ylab("Day of Year") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                                                   axis.text.y = element_blank(),
                                                                                    axis.title.y = element_blank(),
                                                                                    axis.ticks.y = element_blank(),
                                                                                                   axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                                                   legend.box.background = element_rect(),
                                                                                                   plot.title = element_text(color="#D95F02"),
                                                                                                   legend.position="none") + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), xx[(xx$year==2017),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2017")+
  scale_y_continuous(breaks=seq(min(100), max(140), by=10)) + coord_flip(ylim=c(100,140)) 

dvr2018<-ggplot(dx.r[(dx.r$year==2018),], aes(x=code, y=mean)) + geom_point(aes(color=colz,shape=pheno)) +
  geom_line(col="green4", alpha=0.3) + coord_flip(ylim=c(100,140)) + ylab("Day of Year") + xlab("Species") +theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                                                                                  axis.text.y = element_blank(),
                                                                                                                  axis.title.y = element_blank(),
                                                                                                                  axis.ticks.y = element_blank(),
                                                                                                   axis.text=element_text(size=9), legend.key = element_rect(fill = "transparent"),
                                                                                                   legend.box.background = element_rect(),
                                                                                                   plot.title = element_text(color="#7570B3"),
                                                                                                   legend.text = element_text(size=7),
                                                                                                   legend.title = element_text(size=8)) + labs(col="Phenophase") + 
  geom_hline(aes(yintercept=bb.yr), xx[(xx$year==2018),], col="black", linetype="dashed") +
  scale_shape_manual(name="Phenophase", values=c(16, 17), labels=c("Budburst", "Leafout")) +
  scale_color_manual(name="Phenophase", values=c("salmon3", "royalblue3"), labels=c("Budburst", "Leafout")) + ggtitle("2018") +
  scale_y_continuous(breaks=seq(min(100), max(140), by=10))


allyrs <- ggarrange(dvr2016, dvr2017, dvr2018, ncol=3)


### Let's add in Climate data now
clim <- read.csv("weldclimate.csv", header=TRUE)
clim$hour <- NULL
clim <- clim[(clim$year>2015),]
clim <- clim[!duplicated(clim),]

spring <- clim[(clim$doy>=1 & clim$doy<=140),]
#spring$year <- as.integer(spring$year)
spring$Temp..F <- ave(spring$Temp..F, spring$date)
spring <- spring[!duplicated(spring),]
spring$tmean <- (spring$Temp..F - 32) * (5/9)

climate <- ggplot(spring, aes(x=doy, y=tmean, col=as.factor(year))) + #geom_point(aes(col=as.factor(year)), alpha=0.1) +
  geom_smooth(aes(col=as.factor(year), fill=as.factor(year)), stat="smooth", method="loess", se=TRUE, span=0.9) + 
  scale_color_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018")) +
   scale_fill_manual(name = "Year", values=cols, labels = c("2016", "2017", "2018")) +
  theme_classic() + xlab("Day of Year") + ylab("Mean \n Temperature (°C)") +
  coord_cartesian(ylim=c(-8, 18), expand=0) + scale_x_continuous(breaks = seq(min(0), max(140), by=30)) +
  scale_y_continuous(breaks=seq(min(-8), max(18), by=4)) + theme(panel.spacing = unit(c(0,0,5,5),"cm"),
                                                                 legend.text = element_text(size=7),
                                                                 legend.title = element_text(size=8),
                                                                 legend.key.size = unit(0.8,"line"))


grid.arrange(allyrs, climate, nrow=3, heights = c(3, 0.5, 1.3), layout_matrix=rbind(c(1, 1, 1),
                                                                             c(NA),
                                                                             c(NA, 2, 2, 2, NA)))


dx.h<-filter(dx, pheno!="Breaking leaf buds")
hys<-dplyr::select(dvr, species, year, hys)
dx.h<-inner_join(dx.h, hys)
dx.h<-dx.h[!(dx.h$species=="Fagus grandifolia" & dx.h$year==2018),]

colors<-colorRampPalette(brewer.pal(8,"Spectral"))(2)

hys <- ggplot(dx.h, aes(x=species, y=mean)) + geom_point(aes(col=hys)) + geom_line(aes(col=hys)) +
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

df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year, First_Yes_DOY)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2016)
dsix<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)

total.numobs.six <- as.data.frame(table(sort(dsix$ObservedBy_Person_ID)))
total.numobs.six <- data.frame(observer=total.numobs.six$Var1, n=total.numobs.six$Freq)
numobs.six <- subset(total.numobs.six, total.numobs.six$n>40) ## 35 Observers

numobs.six$observer <- as.character(numobs.six$observer)
numobs.six$n <- as.numeric(numobs.six$n)

totobservs <- unique(numobs.six$observer)

numvisits.six <- dsix %>%
  filter(ObservedBy_Person_ID %in% totobservs) %>%
  rename(doy = First_Yes_DOY) %>%
  rename(observer = ObservedBy_Person_ID) %>%
  rename(year = First_Yes_Year)

numvisits.six <- numvisits.six[!duplicated(numvisits.six),]

numvisits.six$numdays <- as.numeric(ave(numvisits.six$doy, numvisits.six$observer, FUN=length))

numvisits.six <- subset(numvisits.six, numvisits.six$numdays>3)
numvisits.six$doy <- as.numeric(numvisits.six$doy)
numvisits.six$first <- ave(numvisits.six$doy, numvisits.six$observer, FUN=min)
numvisits.six$last <- ave(numvisits.six$doy, numvisits.six$observer, FUN=max)

numvisits.six$daysbtw <- numvisits.six$last - numvisits.six$first


numvisits.six$doy <- NULL
numvisits.six <- numvisits.six[!duplicated(numvisits.six),]

observers.16 <- full_join(numobs.six, numvisits.six)

if(FALSE){
total.numobs.six$observer <- as.character(total.numobs.six$observer)
roster$observer <- as.character(roster$observer)

total.numobs.six <- full_join(total.numobs.six, roster)
total.numobs.six <- total.numobs.six[order(-total.numobs.six$n),]
total.numobs.six <- total.numobs.six[!is.na(total.numobs.six$n),]

#write.csv(total.numobs.six, file="observers2016.csv", row.names = FALSE)


dsix<-dsix[!duplicated(dsix),]
length(unique(dsix$ObservedBy_Person_ID))
                    # 62 = 2016
}


df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year, First_Yes_DOY)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2017)
dseven<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)

total.numobs.seven <- as.data.frame(table(sort(dseven$ObservedBy_Person_ID)))
total.numobs.seven <- data.frame(observer=total.numobs.seven$Var1, n=total.numobs.seven$Freq)
numobs.seven <- subset(total.numobs.seven, total.numobs.seven$n>40) ## 35 Observers

numobs.seven$observer <- as.character(numobs.seven$observer)
numobs.seven$n <- as.numeric(numobs.seven$n)

totobservs <- unique(numobs.seven$observer)

numvisits.seven <- dseven %>%
  filter(ObservedBy_Person_ID %in% totobservs) %>%
  rename(doy = First_Yes_DOY) %>%
  rename(observer = ObservedBy_Person_ID) %>%
  rename(year = First_Yes_Year)

numvisits.seven <- numvisits.seven[!duplicated(numvisits.seven),]

numvisits.seven$numdays <- as.numeric(ave(numvisits.seven$doy, numvisits.seven$observer, FUN=length))

numvisits.seven <- subset(numvisits.seven, numvisits.seven$numdays>3)
numvisits.seven$doy <- as.numeric(numvisits.seven$doy)
numvisits.seven$first <- ave(numvisits.seven$doy, numvisits.seven$observer, FUN=min)
numvisits.seven$last <- ave(numvisits.seven$doy, numvisits.seven$observer, FUN=max)

numvisits.seven$daysbtw <- numvisits.seven$last - numvisits.seven$first


numvisits.seven$doy <- NULL
numvisits.seven <- numvisits.seven[!duplicated(numvisits.seven),]

observers.17 <- full_join(numobs.seven, numvisits.seven)

if(FALSE){
total.numobs.seven$observer <- as.character(total.numobs.seven$observer)
roster$observer <- as.character(roster$observer)

total.numobs.seven <- full_join(total.numobs.seven, roster)
total.numobs.seven <- total.numobs.seven[order(-total.numobs.seven$n),]
total.numobs.seven <- total.numobs.seven[!is.na(total.numobs.seven$n),]

#write.csv(total.numobs.seven, file="observers2017.csv", row.names = FALSE)

dseven<-dseven[!duplicated(dseven),]
length(unique(dseven$ObservedBy_Person_ID))
                  # 74 = 2017
}

df<-dplyr::select(b, ObservedBy_Person_ID, First_Yes_Year, First_Yes_DOY)
df[] <- lapply(df, gsub, pattern="'", replacement="")
df<-filter(df, First_Yes_Year==2018)
deight<-df %>% 
  mutate(ObservedBy_Person_ID = strsplit(as.character(ObservedBy_Person_ID), ",")) %>% 
  unnest(ObservedBy_Person_ID)

total.numobs.eight <- as.data.frame(table(sort(deight$ObservedBy_Person_ID)))
total.numobs.eight <- data.frame(observer=total.numobs.eight$Var1, n=total.numobs.eight$Freq)
numobs.eight <- subset(total.numobs.eight, total.numobs.eight$n>40) ## 35 Observers

numobs.eight$observer <- as.character(numobs.eight$observer)
numobs.eight$n <- as.numeric(numobs.eight$n)

totobservs <- unique(numobs.eight$observer)

numvisits.eight <- deight %>%
  filter(ObservedBy_Person_ID %in% totobservs) %>%
  rename(doy = First_Yes_DOY) %>%
  rename(observer = ObservedBy_Person_ID) %>%
  rename(year = First_Yes_Year)

numvisits.eight <- numvisits.eight[!duplicated(numvisits.eight),]

numvisits.eight$numdays <- as.numeric(ave(numvisits.eight$doy, numvisits.eight$observer, FUN=length))

numvisits.eight <- subset(numvisits.eight, numvisits.eight$numdays>3)
numvisits.eight$doy <- as.numeric(numvisits.eight$doy)
numvisits.eight$first <- ave(numvisits.eight$doy, numvisits.eight$observer, FUN=min)
numvisits.eight$last <- ave(numvisits.eight$doy, numvisits.eight$observer, FUN=max)

numvisits.eight$daysbtw <- numvisits.eight$last - numvisits.eight$first


numvisits.eight$doy <- NULL
numvisits.eight <- numvisits.eight[!duplicated(numvisits.eight),]

observers.18 <- full_join(numobs.eight, numvisits.eight)

roster <- read.csv("grouproster.csv", header=TRUE)
roster <- roster %>% dplyr::select(Person_ID, User_Name, Name) %>%
  rename(observer = Person_ID)


allobsers <- full_join(observers.16, observers.17)
allobsers <- full_join(allobsers, observers.18)
roster$observer <- as.character(roster$observer)
allobsers <- full_join(allobsers, roster)

allobsers$totalobs <- ave(allobsers$n, allobsers$observer, FUN=sum)
allobsers$year <- as.numeric(allobsers$year)
allobsers$totyears <- ave(allobsers$year, allobsers$observer, FUN=length)

allobsers <- allobsers[!is.na(allobsers$n),]



if(FALSE){
total.numobs.eight$observer <- as.character(total.numobs.eight$observer)
roster$observer <- as.character(roster$observer)

total.numobs.eight <- full_join(total.numobs.eight, roster)
total.numobs.eight <- total.numobs.eight[order(-total.numobs.eight$n),]
total.numobs.eight <- total.numobs.eight[!is.na(total.numobs.eight$n),]

#write.csv(total.numobs.eight, file="observers2018.csv", row.names = FALSE)

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
}



cols <- colorRampPalette(c("blue", "red"))
visits<-ggplot(allobsers, aes(x=numdays)) + geom_histogram(binwidth = 5, size=0.3, fill=cols(18)) +
  xlab("Number of Visits") + ylab("Number of Observers") + coord_cartesian(expand =c(0, 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(0.05,0.85), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10)) +
  scale_x_continuous(breaks = seq(min(0), max(100), by=10))

cols <- colorRampPalette(brewer.pal(3, "Set1"))(3)
obsersvations<-ggplot(allobsers, aes(x=as.factor(year), y=n)) + geom_boxplot(aes(fill=as.factor(year), group=as.factor(year), y=n)) +
  scale_fill_manual(values=cols, labels = c("2016", "2017", "2018")) +
  xlab("Year") + ylab("Number of Observerations") + #coord_cartesian(expand =c(0, 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = "none", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10)) 
                         

box.visits<-ggplot(allobsers, aes(x=as.factor(year), y=numdays)) + geom_boxplot(aes(fill=as.factor(year), group=as.factor(year), y=numdays)) +
  scale_fill_manual(values=cols, labels = c("2016", "2017", "2018")) +
  xlab("Year") + ylab("Number of Visits") + #coord_cartesian(expand =c(0, 0)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = "none", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10)) 



cols <- colorRampPalette(c("blue", "red"))
hist2016<-ggplot(total.numobs.six, aes(x=n)) + geom_histogram(binwidth = 11, size=0.3, fill=cols(33)) +
  xlab("Number of Observations") + ylab("Number of Observers") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(0.05,0.85), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10))

hist2017<-ggplot(total.numobs.seven, aes(x=n)) + geom_histogram(binwidth = 11, size=0.3, fill=cols(52)) +
  xlab("Number of Observations") + ylab("Number of Observers") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(0.05,0.85), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10))

hist2018<-ggplot(total.numobs.eight, aes(x=n)) + geom_histogram(binwidth = 11, size=0.3, fill=cols(33)) +
  xlab("Number of Observations") + ylab("Number of Observers") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(0.05,0.85), legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=12), legend.title = element_text(size=8), axis.text=element_text(size=10))

quartz()
ggarrange(hist2016, hist2017, hist2018, ncol=3)


