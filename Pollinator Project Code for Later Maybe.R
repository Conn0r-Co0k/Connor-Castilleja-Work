



#for the community data sets create a data set that is each segment and the number of plants in each segment
community_yes_plants <- community_yes %>% 
  #Sum up the number of plants in each segment because each plant is individually assigned cast_presence
  group_by(transect,segment) %>% 
  count(cast_presence)

community_no_plants <- community_no %>%
  group_by(transect,segment) %>% 
  count(cast_presence)


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


#-----------Looking at mean amount of plants according to the presence of castilleja---------#
#find mean and standard deviation amongst the segments in each data set
#comm_yes_plant_mean <- community_yes_plants %>% 
#group_by(transect) %>% 
#summarise(mean = mean(n),
#se = sd(n)/sqrt(n()))

#comm_no_plant_mean <- community_no_plants %>% 
#group_by(transect) %>% 
#summarise(mean = mean(n),
#se = sd(n)/sqrt(n()))

#comm_no_plant_mean <- community_no_plants %>% 
#group_by(transect, segment, cast_presence) %>% 
#count(cast_presence)

#community_sums <- community %>%  
#group_by(transect, segment, cast_presence) %>% 
#count(cast_presence)


#-------Looking at total amount of flowers according to presence of castilleja------#
#comm_yes_flowers <- community_yes %>%
#group_by(transect,segment) %>%
#sum("total_flowers", na.rm = FALSE)



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