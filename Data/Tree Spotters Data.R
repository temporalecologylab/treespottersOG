# Tree Spotters Data
# Organize total recordings by volunteers and determine where gaps are
# June 22 2016

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)


# Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)


# Integrate Phenology Data and compare methodologies
# Set Working Directory
setwd("~/Documents/git/treespotters/Data")
record<-read.csv("treespotters.Dec2016.csv",header=TRUE,sep=",")

# Blend
blend<-record %>% 
  select(Route, first, second, third) %>%
  gather(Year, Recordings, -Route) %>%
  arrange(Route)

# Bar Plot
ggplot(blend,aes(x=Route,y=Recordings, fill=factor(Year))) +
  geom_bar(stat = "identity" , position="dodge") +
  xlab("Route")+ylab("Number of Recordings") +
  scale_fill_discrete(labels=c("2014", "2015", "2016"),
                      name="Number of Recordings")

# Volunteer Data and Retention Rate
observers<-read.csv("potluck.presentation.csv",header=TRUE,sep=",")
attach(observers)

retention<- observers%>%
  select(ObservedBy_Person_ID,Observation_Date) %>%
  group_by(ObservedBy_Person_ID) %>%
  summarize_each(funs(first))

date <- as.Date(
  retention$Observation_Date, "%m/%d/%y")

rate<- retention %>%
  filter(date >= '2016-01-01')

# Demographics
setwd("~/Documents/Temporal Ecology/TreeSpotters")
demo<-read.csv("volunteer.csv",header=TRUE,sep=",")
attach(demo)

date2 <- as.Date(
  demo$Last_Observation, "%m/%d/%y")

area<-demo %>%
  filter(date2 >= "2016/01/01") %>%
  select(Area, Last_Observation)
count<- count(area,Area)
  
qplot(Area,n, data = count, geom = "boxplot", color=Area)

# Attempt at visualizing observer error
setwd("~/Documents/Temporal Ecology/TreeSpotters")
observers<-read.csv("treespotupdate.csv",header=TRUE,sep=",")
attach(observers)

birch<- observers%>%
  select(Species_ID,Phenophase_Status,
         Phenophase_Description,ObservedBy_Person_ID) %>%
  filter(Species_ID == 97)

all<- observers%>%
  select(Species_ID,Phenophase_Status,Day_of_Year,
          Phenophase_Description,ObservedBy_Person_ID) %>%
  filter(Phenophase_Status == 1)

test<- observers %>%
  select (Observation_Date, Phenophase_Status, Phenophase_Description,
          Individual_ID, Day_of_Year, ObservedBy_Person_ID) %>%
  filter(Individual_ID == 85762) %>%
  filter(Phenophase_Description == "Breaking leaf buds")
date.t <- as.Date(
  test$Observation_Date, "%m/%d/%y")
test1<-test%>%
  filter(date.t >= "2016/01/01")

t.count<-count(test,Day_of_Year)

ggplot(test1,aes(x=Day_of_Year,y=Phenophase_Status,
                color=factor(ObservedBy_Person_ID))) +
  geom_point(stat = "identity" , position="dodge",
             size=4)+
  xlab("Day_of_Year")+ylab("Y/N/?")

counts<-count(all, Phenophase_Status)
count<- count(birch, Phenophase_Status)

# Budbreak per Species
bb.yb<- observers %>%
  select (Observation_Date, Phenophase_Status, 
          Phenophase_Description,Individual_ID, Day_of_Year, 
          Species_ID, ObservedBy_Person_ID) %>%
  filter(Species_ID == 97) %>%
  filter(Phenophase_Description == "Breaking leaf buds") %>%
  filter(Phenophase_Status == 1)

date.bb <- as.Date(
  bb.yb$Observation_Date, "%m/%d/%y")

bb.yb1 <- bb.yb %>%
  filter(date.bb >= "2016/01/01") %>%
  group_by(Individual_ID) %>%
  arrange(Day_of_Year) %>%
  filter(row_number()==1)

ggplot(bb.yb1,aes(x=Day_of_Year,y=Phenophase_Status,
                  fill = factor(Individual_ID))) +
  geom_bar(stat = "identity" , position="stack", width = 0.5) +
  xlab("Day_of_Year")+ylab("Y/N/?") +
  scale_x_continuous(limits = c(95,105),breaks=seq(95,105,1))

qplot(Common_Name,Day_of_Year, data = bb.all1, 
      geom = "boxplot", color=Common_Name)
# First Yes BB
bb.all <- observers %>%
  select (Observation_Date, Phenophase_Status, 
          Phenophase_Description,Individual_ID, 
          Common_Name,
          ObservedBy_Person_ID, Day_of_Year) %>%
  filter(Phenophase_Description == "Breaking leaf buds") %>%
  filter(Phenophase_Status == 1)
date.bb1 <- as.Date(
  bb.all$Observation_Date, "%m/%d/%y")
bb.all1 <- bb.all %>%
  filter(date.bb1 >= "2016/01/01") %>%
  group_by(Individual_ID) %>%
  arrange(Day_of_Year) %>%
  filter(row_number()==1)
ggplot(bb.all1,aes(x=Day_of_Year,y=Phenophase_Status,
                  fill = factor(Common_Name))) +
  geom_bar(stat = "identity" , position="stack", width = 0.5) +
  xlab("Julian Date")+ylab("First Yes") +
  scale_x_continuous(limits = c(80,135),breaks=seq(80,135,2)) +
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,1))

# First Yes Leaves
leaf.all <- observers %>%
  select (Observation_Date, Phenophase_Status, 
          Phenophase_Description,Individual_ID, 
          Common_Name,
          ObservedBy_Person_ID, Day_of_Year) %>%
  filter(Phenophase_Description == "Leaves") %>%
  filter(Phenophase_Status == 1)
date.leaf1 <- as.Date(
  leaf.all$Observation_Date, "%m/%d/%y")
leaf.all1 <- leaf.all %>%
  filter(date.leaf1 >= "2016/01/01") %>%
  group_by(Individual_ID) %>%
  arrange(Day_of_Year) %>%
  filter(row_number()==1)
ggplot(leaf.all1,aes(x=Day_of_Year,y=Phenophase_Status,
                   fill = factor(Common_Name))) +
  geom_bar(stat = "identity" , position="stack", width = 0.5) +
  xlab("Julian Date")+ylab("First Yes") +
  scale_x_continuous(limits = c(65,145),breaks=seq(65,145,2)) +
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,1))

# First Yes Flowers of Flower Buds
flw.all <- observers %>%
  select (Observation_Date, Phenophase_Status, 
          Phenophase_Description,Individual_ID, 
          Common_Name,
          ObservedBy_Person_ID, Day_of_Year) %>%
  filter(Phenophase_Description == "Flowers or flower buds") %>%
  filter(Phenophase_Status == 1)
date.flw1 <- as.Date(
  flw.all$Observation_Date, "%m/%d/%y")
flw.all1 <- flw.all %>%
  filter(date.flw1 >= "2016/01/01") %>%
  group_by(Individual_ID) %>%
  arrange(Day_of_Year) %>%
  filter(row_number()==1)
ggplot(flw.all1,aes(x=Day_of_Year,y=Phenophase_Status,
                     fill = factor(Common_Name))) +
  geom_bar(stat = "identity" , position="stack", width = 0.5) +
  xlab("Julian Date")+ylab("First Yes") +
  scale_x_continuous(limits = c(75,145),breaks=seq(75,145,2)) +
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,1))

# First Yes Fruits
fruit.all <- observers %>%
  select (Observation_Date, Phenophase_Status, 
          Phenophase_Description,Individual_ID, 
          Common_Name,
          ObservedBy_Person_ID, Day_of_Year) %>%
  filter(Phenophase_Description == "Fruits") %>%
  filter(Phenophase_Status == 1)
date.fruit1 <- as.Date(
  fruit.all$Observation_Date, "%m/%d/%y")
fruit.all1 <- fruit.all %>%
  filter(date.fruit1 >= "2016/01/01") %>%
  group_by(Individual_ID) %>%
  arrange(Day_of_Year) %>%
  filter(row_number()==1)
ggplot(fruit.all1,aes(x=Day_of_Year,y=Phenophase_Status,
                    fill = factor(Common_Name))) +
  geom_bar(stat = "identity" , position="stack", width = 0.5) +
  xlab("Julian Date")+ylab("First Yes") +
  scale_x_continuous(limits = c(55,190),breaks=seq(55,190,3)) +
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,1))


# How many observers per individual and species
setwd("~/Documents/Temporal Ecology/TreeSpotters")
update<-read.csv("treespotupdate.csv",header=TRUE,sep=",")
attach(observers)

obs.all<- update %>%
  select (Observation_Date, Phenophase_Status, Common_Name,
          Phenophase_Description,Plant_Nickname, Day_of_Year, 
          Species_ID, ObservedBy_Person_ID) 
date.obs <- as.Date(
  obs.all$Observation_Date, "%m/%d/%y")
obs.all1 <- obs.all %>%
  filter(date.obs >= "2016/01/01") %>%
  group_by(Plant_Nickname,ObservedBy_Person_ID)%>%
  select(Day_of_Year,Plant_Nickname,ObservedBy_Person_ID,Common_Name)%>%
  filter(row_number()==1)
obs.count<-count(obs.all1,Plant_Nickname)
obs<-full_join(obs.count,obs.all1, by = "Plant_Nickname")%>%
  group_by(Plant_Nickname)%>%
  filter(row_number()==1) %>%
  select(-ObservedBy_Person_ID)

qplot(Common_Name,n, data = obs, 
      geom = "boxplot", color=Common_Name) + 
  xlab("Species")+ylab("# of Observers")

# Observers per Tree
total<- update %>%
  select (Observation_Date, Common_Name, Plant_Nickname, Day_of_Year,
          Observation_ID) 
date.tot <- as.Date(
  total$Observation_Date, "%m/%d/%y")
total1 <- total %>%
  filter(date.tot >= "2016/01/01") %>%
  group_by(Plant_Nickname,Observation_ID)%>%
  select(Day_of_Year,Plant_Nickname,Observation_ID, Common_Name)
total.count<-count(total1,Plant_Nickname)
tot<-full_join(total.count,total1, by = "Plant_Nickname")%>%
  group_by(Plant_Nickname)%>%
  filter(row_number()==1) %>%
  select(-Observation_ID)

table<-left_join(tot,obs, by="Plant_Nickname")%>%
  select(-Day_of_Year.x, -Day_of_Year.y, -Common_Name.x)

ggplot(table,aes(x=n.x,y=n.y)) +
  geom_point(aes(col=Common_Name.y)) + geom_smooth(method="lm") +
  xlab("Total Observations")+ylab("Number of Observers")

