setwd("~/GitHub/My Castilleja Work")
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
conflicts_prefer(tidyr::extract)
conflicts_prefer(purrr::setnames)



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
#network_yes <- filter(network, cast_presence == "1")
#network_no <- filter(network, cast_presence == "0")
#community_yes <- filter(community, cast_presence == "1")
#community_no <- filter(community, cast_presence == "0")


community_sums <- community %>%  
  group_by(transect, segment, cast_presence) %>% 
  count(cast_presence)

community_sums$segment <- as.factor(community_sums$segment)

facet_wrap(~transect)

ggplot(data = community_sums, aes(x = transect, y = n, fill = as.factor(segment))) + #sets the data and the axes for what we would like in our plot
  geom_col(position = position_dodge(0.7), width = .6, linewidth = 0.75, alpha = 0.9, size = 0.1) + #position_dodge keeps bars from being on top of each other, alpha is opacity, line width is width of line around bars, width is width of bars
  labs(x = "Segment", y = "Number of Plants") + #labels the axes
  scale_fill_manual(values=c("#4b3b40","#b6ad90", "purple", "gray", "#5e819d"))+ #sets the colors of the bars
  theme_pubr() +
  ylim(0,400) #sets y axis limit
















         

