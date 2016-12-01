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
record<-read.csv("December2016.csv",header=TRUE,sep=",")

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
observers<-read.csv("December2016.csv",header=TRUE,sep=",")
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
#setwd("~/Documents/Temporal Ecology/TreeSpotters")
#demo<-read.csv("volunteer.csv",header=TRUE,sep=",")

#date2 <- as.Date(
  #demo$Last_Observation, "%m/%d/%y")

#area<-demo %>%
  #filter(date2 >= "2016/01/01") %>%
  #select(Area, Last_Observation)
#count<- count(area,Area)
  
#qplot(Area,n, data = count, geom = "boxplot", color=Area)

# How many observers per individual and species
obs.all<- record %>%
  select(Observation_Date, Phenophase_Status, Common_Name,
          Phenophase_Description,Plant_Nickname, Day_of_Year, 
          Species_ID, ObservedBy_Person_ID) 
date.obs <- as.Date(
  obs.all$Observation_Date, "%m/%d/%y")
obs.all1 <- obs.all %>%
  filter(date.obs >= "2016/01/01") %>%
  group_by(Plant_Nickname,ObservedBy_Person_ID)%>%
  arrange(Plant_Nickname)%>%
  filter(row_number()==1)
obs.count<-as.data.frame(table(obs.all1$Plant_Nickname)) %>%
  rename(Plant_Nickname = Var1) %>%
  rename(n = Freq)
obs<-full_join(obs.count,obs.all1, by="Plant_Nickname")

qplot(Common_Name, n, data = obs, 
      geom = "boxplot", color=Common_Name) + 
  xlab("Species")+ylab("# of Observers")

# Observers per Tree
total<- record %>%
  select (Observation_Date, Common_Name, Plant_Nickname, Day_of_Year,
          Observation_ID) 
date.tot <- as.Date(
  total$Observation_Date, "%m/%d/%y")
total1 <- total %>%
  filter(date.tot >= "2016/01/01") %>%
  group_by(Plant_Nickname,Observation_ID)%>%
  select(Day_of_Year,Plant_Nickname,Observation_ID, Common_Name)
total.count<-as.data.frame(table(total1,Plant_Nickname))
tot<-full_join(total.count,total1, by = "Plant_Nickname")%>%
  group_by(Plant_Nickname)%>%
  filter(row_number()==1) %>%
  select(-Observation_ID)

table<-left_join(tot,obs, by="Plant_Nickname")%>%
  select(-Day_of_Year.x, -Day_of_Year.y, -Common_Name.x)

ggplot(table,aes(x=n.x,y=n.y)) +
  geom_point(aes(col=Common_Name.y)) + geom_smooth(method="lm") +
  xlab("Total Observations")+ylab("Number of Observers")
