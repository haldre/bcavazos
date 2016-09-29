require(graphics)

#Set Working Directory
setwd("C:/Users/Dunham/Desktop/Rogers/research/Rogers/REU 2013") #Amy's computer
setwd("~/Documents/Dropbox/EBL Project/SCIENCE/Food web/REU 2013 Butterflies and caterpillars/R scripts/R scripts for REU") #Haldre's laptop

### General Approach to Analysis ########
#1) Is there a difference in butterfly abundance between islands with and without birds?  We need to take into account differences in forest type, length of transect
#2) Is there a difference in the diversity/community of butterflies present on islands with and without birds? Note- 8 sites out of 21 had zero butterflies. 

#######################
#Attach Butterfly Data
butterfly<-read.csv("REUmetadata.csv", header=T)

library(sciplot) 
library(lme4)
library(plyr)
library(ggplot2)

###################
#Butterfly Surveys- Look at data
names(butterfly)
summary(butterfly)
str(butterfly)

####Manipulate data to get into right format ######

#Adds bird or no bird column 
butterfly$bird<-ifelse(butterfly$island=="guam","no","yes")
butterfly$bird<-as.factor(butterfly$bird)

#make dataframe with sum of number of butterflies per transect
sumbutterfly<-ddply(butterfly, .(site), summarize, total=sum(number))
sumbutterfly<-join(sumbutterfly, butterfly, by=c("site"), type="left", match="first")
sumbutterfly<-sumbutterfly[,-c(7,8)]

#Just to Check Data
summary(butterfly, na.rm=T) #Summary of raw data
summary(sumbutterfly)

with(sumbutterfly, table(total,site)) #tells us how many sites had each number of butterflies (e.g. 8 sites had 0, 1 site had 1 etc.)

###### Graphs ####
#Island Count 
bargraph.CI(type,number, group=island, data=butterfly, xlab="Island", ylab="count", legend=T)
#Island Count by Species
bargraph.CI(species,number, group = island, data =butterfly, xlab="Island", ylab="count", legend =T)
#Island Count by Type
bargraph.CI(island,number, group = type, data =butterfly, xlab="Island", ylab="count", legend =T)
##Bird presence vs number butterflies
bargraph.CI(bird,number, group=type, data=butterfly, xlab="Birds Present", ylab="Number of Butterflies", legend=T)

### Statistical models ######
#comparison of butterfly abundance between islands
m1 <- glm(total ~ bird*type+length, data=sumbutterfly, family="poisson")
summary(m1)
m2 <- glm(total  ~ bird+type+length, data=sumbutterfly, family="poisson")
m3<- glm(total  ~ type+length, data=sumbutterfly, family="poisson")
AIC(m1,m2) # Likelihood ratio test, best model is model 1 
AIC(m2,m3)

#two things to check- Haldre
#1)why can't do Likelihood Ratio Test? 
#2)overdispersion- need to correct for it - either use a quasi-poisson? or add 
#observation-level random effect, or check zero-inflated poisson

### Is there a difference in species diversity between islands with and without birds? ###
#use vegan package for ordination? 
#calculate shannon-weiner index of diversity? - best to do in vegan

###graphing butterflies
ggplot(sumbutterfly, aes(type, total, fill=bird))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("Total number of butterflies by forest type and bird presence")

ggplot(butterfly, aes(type, number))+
  geom_boxplot()+
  facet_grid(.~species, "free")

