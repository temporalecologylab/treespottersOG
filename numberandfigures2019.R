### Time to visualize the numbers
# 29 July 2019 - Cat

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
library(viridis)

# Set Working Directory
setwd("~/Documents/git/treespotters/analysis/input")
b<-read.csv("individual_phenometrics_data.csv",header=TRUE)


###### Breakdown of observations per species
b$spp <- paste(b$Genus, b$Species)

numobsperspp <- b %>% count(spp)

#colz <- colorRampPalette(brewer.pal(15, "Dark2"))(15)
colz <- viridis_pal(option="D")(15)
colourCount = length(unique(numobsperspp$spp))
getPalette = colorRampPalette(brewer.pal(8, "Spectral"))

sppnum <- ggplot(numobsperspp, aes(x=spp, y=n, fill=spp)) + geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=getPalette(colourCount), 
                    labels=numobsperspp$spp) + 
  labs(title="Total number of observed phenophases \n(e.g., leaves or flowers) for each species (2015-2019).") +
  scale_x_discrete(labels=c("Acer rubrum" = "Red Maple",
                            "Acer saccharum" = "Sugar Maple",
                            "Aesculus flava" = "Yellow Buckeye",
                            "Betula alleghaniensis" = "Yellow Birch",
                            "Betula nigra" = "River Birch",
                            "Carya glabra" = "Pignut Hickory",
                            "Carya ovata" = "Shagbarck Hickory",
                            "Fagus grandifolia" = "American Beech",
                            "Hamamelis virginiana" = "Witch hazel",
                            "Populus deltoides" = "Eastern Cottonwood",
                            "Quercus alba" = "White Oak",
                            "Quercus rubra" = "Red Oak",
                            "Tilia americana" = "American Linden",
                            "Vaccinium corymbosum" = "Highbush Blueberry",
                            "Viburnum nudum" = "Possumhaw")) +
  theme_classic() + theme(legend.position="none", 
                          plot.margin=unit(c(1,3,1,1), "lines"),
                          plot.title = element_text(face="bold")) +
  ylab("Number of observed phenophases") + xlab("") +
  coord_flip(expand=c(0,0), clip="off") + 
  geom_text(aes(label=n), hjust=-0.2, col=getPalette(colourCount))

quartz()
sppnum

### Now let's make a plot with time-series data
df <- data.frame(year=c(2015:2019), obsperyr=c(12524, 53502, 66218, 60971,46495))
df$obssum <- c(12524, 12524+53502, 12524+53502+66218, 12524+53502+66218+60971, 12524+53502+66218+60971+46495)


cols<-colorRampPalette(brewer.pal(8, "Dark2"))(5)
colpal <- colorRampPalette(c("blue3", "red3"))
colvir <- viridis_pal(option="C")(6)
quartz()
ggplot(df, aes(x=year, y=obssum)) + geom_line(aes(col=as.factor(obssum)), col=colpal(5)) + geom_point(aes(col=as.factor(year))) + 
  theme_classic() + scale_color_manual(values=colpal(5), labels=df$year) + 
  theme(legend.position = "none",
        plot.margin = unit(c(1,4.5,1,1), "lines"),
        plot.title=element_text(face="bold"),
        axis.text.x=element_text(color=colpal(5))) + ggtitle("Total number of observations over time") +
  ylab("Total Number of Observations") +
  geom_text(aes(label=obssum, col=as.factor(year), size=as.factor(year)),hjust=-0.2, vjust=0.5) + xlab("") +
  coord_cartesian(clip = 'off')
  


### Pie Chart
routes <- data.frame(route=c("Linden and North Woods", "Maple", "Shrub", "Birch", "Oak",
                             "Hickory", "Beech", "Peters Hill"), 
                     n=c(23027, 22861, 11135, 57517, 22293, 36661, 12838, 53795))



piechart <- ggplot(routes, aes(x="", y=n, fill=route))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + theme_classic() +
  scale_fill_brewer(palette="Dark2", name="Route") + ggtitle("Percentage of total observations by route") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(face="bold")) +
  geom_text(aes(label = paste0(round((n/239710)*100), "%")), position = position_stack(vjust = 0.5))
  
quartz()
piechart

