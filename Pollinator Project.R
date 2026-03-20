setwd("C:/Users/gr00k/Downloads/Data Analysis/Pollinator Project")

#load in relevant packages
library(tidyverse)#for data wrangling and restructuring
library(statmod)
library(lme4)#for modeling linear mixed effect models
library(emmeans)#post-hoc analysis
library(car)#for regression analysis
library(performance)#this is new
library(see)#this is new
library(lmerTest)
library(patchwork)
library(ggpubr)
library(rstatix)
library(conflicted)
library(magrittr)
#library(rsthemes)
library(ggplot2)
library(dplyr)

#Conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
rlang::last_trace()



#Create the 2 data sets in r
community <- read.csv("plantsurvey_Raw.csv")
network <- read.csv("network_castilleja.csv")


#getting rid of variable "who" from data set
network <- subset(network, select = -(who))
network <- subset(network, select = -(round))
#get rid of variable "notes" from data set
community <- subset(community, select = -(notes))


#coding for castileja
  #select data set
network %<>% 
  group_by(transect,segment) %>% 
  #create new variable within selected data set
  mutate(cas_yn = str_detect(plant,"Castilleja"),
         #create new variable cast_presence which is if any of the plants within a segment is castilleja every observation from that segment will be labelled with 1
         cast_presence = ifelse(any(cas_yn==TRUE),1,0))

community %<>% 
  group_by(transect,segment) %>% 
  mutate(cas_yn = str_detect(plant,"Castilleja"),
         cast_presence = ifelse(any(cas_yn==TRUE),1,0))


#Create new data only with segments with castilleja
network_yes <- filter(network, cast_presence == "1")
network_no <- filter(network, cast_presence == "0")
community_yes <- filter(community, cast_presence == "1")
community_no <- filter(community, cast_presence == "0")


#for the community data sets create a data set that is each segment and the number of plants in each segment
community_yes_plants <- community_yes %>% 
  #Sum up the number of plants in each segment because each plant is individually assigned cast_presence
  group_by(transect,segment) %>% 
  count(cast_presence)

community_no_plants <- community_no %>%
  group_by(transect,segment) %>% 
  count(cast_presence)

#-----------Looking at mean amount of plants according to the presence of castilleja---------#
#find mean and standard deviation amongst the segments in each data set
comm_yes_plant_mean <- community_yes_plants %>% 
  group_by(transect) %>% 
  summarise(mean = mean(n),
            se = sd(n)/sqrt(n()))

comm_no_plant_mean <- community_no_plants %>% 
  group_by(transect) %>% 
  summarise(mean = mean(n),
            se = sd(n)/sqrt(n()))

comm_no_plant_mean <- community_no_plants %>% 
  group_by(transect, segment, cast_presence) %>% 
  summarise()

community_sums <- community %>% 
  group


#Graph mean amount of plants in each transect according to the presence of castilleja
#Graph for transects with segments with castilleja
ggplot(data = comm_yes_plant_mean, aes(x = transect, y =mean, fill = transect)) + #sets the data and the axes for what we would like in our plot
  geom_col(position = position_dodge(0.7), width = .6, linewidth = 0.75, alpha = 0.9, size = 0.1) + #position_dodge keeps bars from being on top of each other, alpha is opacity, line width is width of line around bars, width is width of bars
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), #Sets error bars as going from mean-se to mean+se
                position = position_dodge(width = 0.7), width = 0.15) + #Corrects and places error bars correctly
  labs(x = "Transect", y = "Number of Plants") + #labels the axes
  scale_fill_manual(values=c("#4b3b40","#b6ad90", "brown", "grey", "tan"))+ #sets the colors of the bars
  theme_pubr() +
  ylim(0,300) #sets y axis limit

#Graph for transects with segments without castilleja
ggplot(data = comm_no_plant_mean, aes(x = transect, y =mean, fill = transect)) + #sets the data and the axes for what we would like in our plot
  geom_col(position = position_dodge(0.7), width = .6, linewidth = 0.75, alpha = 0.9, size = 0.1) + #position_dodge keeps bars from being on top of each other, alpha is opacity, line width is width of line around bars, width is width of bars
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), #Sets error bars as going from mean-se to mean+se
                position = position_dodge(width = 0.7), width = 0.15) + #Corrects and places error bars correctly
  labs(x = "Transect", y = "Number of Plants") + #labels the axes
  scale_fill_manual(values=c("#4b3b40","#b6ad90", "purple"))+ #sets the colors of the bars
  theme_pubr() +
  ylim(0,50) #sets y axis limit

#-------Looking at total amount of flowers according to presence of castilleja------#
comm_yes_flowers <- community_yes %>%
  group_by(transect,segment) %>%
  sum("total_flowers", na.rm = FALSE)



#Now I want to look at the network data, from our graphs we can see the entire transect that has segments with no castilleja has a lower amount of plants on average
#Create a new dataset that shows the count of insects of each order for each segment
network_yes_insects <- network_yes %>% 
  group_by(transect, segment) %>% 
  count(insect_order)

#Make a new variable that shows the total amount of insects per segment, for some reason it doesn't put it into the environment dataset byt prints it in the console
network_yes_insects %>% 
  group_by(transect, segment) %>% 
  mutate(insect_total = sum(n))


network_no_insects <- network_no %>% 
  group_by(transect, segment) %>% 
  count(insect_order)

network_no_insects %>% 
  group_by(transect, segment) %>% 
  mutate(insect_total = sum(n))
         

