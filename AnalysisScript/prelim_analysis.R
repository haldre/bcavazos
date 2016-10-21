##########################################################################
#### Brittany Cavazos
#### Preliminary analysis of clay catipillar results and butterfly survey
#### Last edited October 20, 2016
##########################################################################
##########################################################################

### Big questions ###
# What are the effects on herbivorous arthropod populations, specifically butterflies, in the absence of birds?
#    a. Is there an increased abundance of butterflies on Guam, which has no insectivorous forest birds, compared to Saipan and Rota, which do have insectivorous forest birds?
#    b. Are rates of herbivorous arthropod predation higher on Saipan and Rota than on Guam?

library(dplyr); library(ggplot2); library(bbmle); library(tidyr); library(lme4); library(vegan); library(reshape2)

###Butterfly Survey preliminary analysis###

# read in the cleaned up data files
butterflydata <- read.csv("C:\\Users\\brittanycavazos\\Documents\\Fall 2016\\EEB_590\\ClayCaterpillarProject\\bcavazos\\Data\\Butterflies\\WorkingData\\butterflydata.csv")
caterpillardata <- read.csv("C:\\Users\\brittanycavazos\\Documents\\Fall 2016\\EEB_590\\ClayCaterpillarProject\\bcavazos\\Data\\Caterpillars\\WorkingData\\caterpillardata.csv")

# first we can make another column for total butterflies per site

unique(butterflydata$site)
tot_butterfly <- aggregate(butterflydata$number_indiv, by = list(butterflydata$site), FUN="sum")
colnames(tot_butterfly)<-c("site","total_butterfly")
butterflydata<-left_join(butterflydata, tot_butterfly, by=NULL)

# make bird column (guam = no, saipan & rota = yes)
butterflydata$bird<-ifelse(butterflydata$island=="guam","no","yes")
# although after talking to H, we may want to look at islands instead of birds because there is a bit of a gradients between islands

meanperisland <- aggregate(butterflydata$number_indiv, by = list(butterflydata$island), FUN="mean")

# change to factors so we can graph
str(butterflydata)
butterflydata$island<-as.factor(butterflydata$island)
butterflydata$site<-as.factor(butterflydata$site)
butterflydata$type<-as.factor(butterflydata$type)

# total butterfly is not normally distributed- use poisson distribution in glmer
hist((butterflydata$total_butterfly))
hist(log(butterflydata$total_butterfly))
ggplot(butterflydata, aes(total_butterfly, fill=island)) +
  geom_histogram()
ggplot(butterflydata, aes(total_butterfly, fill=type)) +
  geom_histogram()


butterflydata$duration<-as.integer(butterflydata$duration)

butterfly.model1 <- glmer(total_butterfly ~ island+(1|site), data=butterflydata, family="poisson") # best model
butterfly.model4 <- glmer(total_butterfly ~ type*island+(1|site), data=butterflydata, family="poisson") # runs with a warning
butterfly.model5 <- glmer(total_butterfly ~ type+island+(1|site), data=butterflydata, family="poisson") # second best model
butterfly.model8 <- glmer(total_butterfly ~ island*duration+(1|site), data=butterflydata, family="poisson")
butterfly.model9 <- glmer(total_butterfly ~ island+duration+(1|site), data=butterflydata, family="poisson")
butterfly.model11 <- glmer(total_butterfly ~ island+type*duration+(1|site), data=butterflydata, family="poisson")

AICtab(butterfly.model1, butterfly.model4, butterfly.model5, butterfly.model8, butterfly.model9, butterfly.model11, weights=T) # Likelihood ratio test, best model is...a tie between model 1 and 5  (maybe 4 too)- consider model averaging


# models I took out 
# butterfly.model2 <- glmer(total_butterfly ~ type+(1|site), data=butterflydata, family="poisson")
# butterfly.model3 <- glmer(total_butterfly ~ duration+(1|site), data=butterflydata, family="poisson") # butterfly.model6 <- glmer(total_butterfly ~ type*duration+(1|site), data=butterflydata, family="poisson") # bad model
# butterfly.model7 <- glmer(total_butterfly ~ type+duration+(1|site), data=butterflydata, family="poisson") # bad model
#butterfly.model10 <- glmer(total_butterfly ~ island*type+duration+(1|site), data=butterflydata, family="poisson") #runs with a warning
#butterfly.model12 <- glmer(total_butterfly ~ island*type*duration+(1|site), data=butterflydata, family="poisson") # runs with a warning

###
# number of sites are different - consider converting to avg number butterflies per site
###


###graphing butterflies
ggplot(butterflydata, aes(island,total_butterfly, fill=type))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Total number of butterflies by forest type and island")

ggplot(butterflydata, aes(type, total_butterfly))+
  geom_boxplot()+
  facet_grid(.~species_name, "free")


# trying to look at comm comp.

butterflydata$comm<-butterflydata$number_indiv/butterflydata$total_butterfly
test<-dcast(butterflydata, island + type + site~species_name)
ord<- metaMDS()
# Use ddply to sum cact.count and reproductive status

#######################################################3333333
##############################################################


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

caterpillardata$bird<-ifelse(caterpillardata$island=="guam","no","yes")


caterpillardata$prop_pred<-as.numeric(caterpillardata$prop_pred)
caterpillardata$site<-as.factor(caterpillardata$site)
caterpillardata$bird<-as.factor(caterpillardata$bird)

summary(caterpillardata$prop_pred)
hist(caterpillardata$prop_pred)
plot(prop_pred~bird, data=caterpillardata)
plot(prop_pred~habitat, data=caterpillardata)


# assign biological significance to predation score
index<-c("*","??","A","B","C","D","E","F","G","H","I","J","L","M","N","NP","O","P")
values<-c("unknown","unknown","arthropod","arthropod","lizard","lizard","small mammal","arthropod","bird","arthropod","arthropod","arthropod","bird","small mammal","unknown","unknown","arthropod","small mammal")

caterpillardata$predator<- values[match(caterpillardata$type, index)]

caterpillar.model1 <- glmer(result ~ island +(1|site), data=caterpillardata, family="binomial")
caterpillar.model2 <- glmer(result ~ habitat +(1|site), data=caterpillardata, family="binomial")
caterpillar.model3 <- glmer(result ~ island + habitat +(1|site), data=caterpillardata, family="binomial")
caterpillar.model4 <- glmer(result ~ island * habitat +(1|site), data=caterpillardata, family="binomial")
AICtab(caterpillar.model1, caterpillar.model2, caterpillar.model3, caterpillar.model4, weights=T) 

###graphing caterpillars
ggplot(caterpillardata, aes(island,prop_pred))+
  geom_point(aes(color=habitat))+
  theme_bw()+
  ggtitle("Proportion caterpillars predated by forest type and island")
# only one trial with a saipan disturbed site :( 

boxplot(caterpillardata$prop_pred~caterpillardata$predator)
caterpillardata[caterpillardata$result=="1",]
ggplot(caterpillardata[caterpillardata$result=="1",], aes(island,prop_pred, fill=predator))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Proportion predated by island and predator type")

