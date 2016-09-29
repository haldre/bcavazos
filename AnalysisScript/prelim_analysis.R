##########################################################################
#### Brittany Cavazos
#### September 25, 2016
#### Preliminary analysis of clay catipillar results and butterfly survey
#### Last edited September 25, 2016
##########################################################################
##########################################################################

###Butterfly Survey prelim analysis###

# first we can make another column for total butterflies per site

unique(butterflydata$site)
tot_butterfly <- aggregate(butterflydata$number_indiv, by = list(butterflydata$site), FUN="sum")
colnames(tot_butterfly)<-c("site","total_butterfly")
butterflydata<-left_join(butterflydata, tot_butterfly, by=NULL)

# change to factors so we can graph
str(butterflydata)
butterflydata$island<-as.factor(butterflydata$island)
butterflydata$site<-as.factor(butterflydata$site)
butterflydata$type<-as.factor(butterflydata$type)

boxplot(total_butterfly~island, data=butterflydata)
boxplot(total_butterfly~site, data=butterflydata)



hist(butterflydata$total_butterfly)

coplot(total_butterfly~ site | type, data=butterflydata)


plot(butterflydata$type, butterflydata$total_butterfly) 
plot(butterflydata$island, butterflydata$total_butterfly)


library(ggplot2)
#histogram - use only a single continuous x value
ggplot(butterflydata, aes(total_butterfly))+
  geom_histogram()

#boxplot
ggplot(butterflydata, aes(type, total_butterfly))+
  geom_boxplot() #plot response and predictors, continuous data

#create different boxplots for each island
a<-(ggplot(butterflydata, aes(island, total_butterfly, color=type))+
      geom_boxplot()+
      theme_classic()+
      ggtitle("title"))
a + theme_bw

#add facet_grid to show other variables
#ggplot(transplant2, aes(websize, duration))+
  #geom_point()+
  #facet_grid(netting~island)+
  #theme_minimal()+
  #ggtitle("title")

###Caterpillar analysis###

# let's make proportion predated column 
# if you turn anything predated into a 1 and anything unpredated into a 0, you can just take the mean
unique(caterpillardata$result)
caterpillardata$result<-gsub("unpredated", "0", caterpillardata$result)
caterpillardata$result<-gsub("predated", "1", caterpillardata$result)
caterpillardata$result<-as.numeric(caterpillardata$result)
prop_pred <- aggregate(caterpillardata$result, by = list(caterpillardata$site), FUN="mean")
colnames(prop_pred)<-c("site","prop_pred")
caterpillardata<-left_join(caterpillardata, prop_pred, by=NULL)

caterpillardata$prop_pred<-as.numeric(caterpillardata$prop_pred)
caterpillardata$site<-as.factor(caterpillardata$site)
hist(caterpillardata$prop_pred)
plot(prop_pred~island, data=caterpillardata)
plot(prop_pred~habitat, data=caterpillardata)


names(caterpillardata)


