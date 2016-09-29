require(graphics)

#Set Working Directory as Dunham Folder
setwd("C:/Users/Dunham/Desktop/Rogers/research/Rogers/REU 2013")

#######################
#Attach Butterfly Data
butterfly<-read.csv("REUmetadata.csv", header=T)
d<-butterfly

library(sciplot)
#Island Count
bargraph.CI(type,number, group=island, data=d, xlab="Island", ylab="count", legend=T)
#Island Count by Species
bargraph.CI(species,number, group = island, data =d, xlab="Island", ylab="count", legend =T)
#Island Count by Type
bargraph.CI(island,number, group = type, data =d, xlab="Island", ylab="count", legend =T)

library(lme4)
library(plyr)

###################
#Butterfly Survey#

#Adds bird or no bird column 
names(butterfly)
summary(butterfly)
butterfly$bird<-ifelse(butterfly$island=="guam","no","yes")
butterfly$bird<-as.factor(butterfly$bird)

#make dataframe with sum of number of butterflies per transect
sumbutterfly<-ddply(butterfly, .(site), summarize, total=sum(number))
sumbutterfly<-join(sumbutterfly, butterfly, by=c("site"), type="left", match="first")
sumbutterfly<-sumbutterfly[,-c(7,8)]

#Just to Check Data
butterfly
sumbutterfly

####Summary Statistics
summary(butterfly, na.rm=T) #Summary of raw data

bargraph.CI(bird,number, group=type, data=butterfly, xlab="Birds Present", ylab="Number of Butterflies", legend=T)

#comparison of butterfly abundance between islands
m1 <- glm(total ~ bird*type+length, data=sumbutterfly, family="poisson")
summary(m1)

m2 <- glm(total  ~ bird+type+length, data=sumbutterfly, family="poisson")
m3<- glm(total  ~ type+length, data=sumbutterfly, family="poisson")
AIC(m1,m2) # Likelihood ratio test, best model is model 1 
AIC(m2,m3)

##Mixed Effects Trial Two##
library(lme4) 
model1<-glm(total~island*type+length, family="poisson", data=sumbutterfly)
summary(model1)
model2<-lm(number~island+type+(length)+(site), data=butterfly)
summary(model2)
model3<-lm(number~island+(length)+(site), data=butterfly)
summary(model3)
AIC(model1, model2, model3)
aov(model1)

