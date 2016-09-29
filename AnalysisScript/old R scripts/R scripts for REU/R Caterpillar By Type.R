####For Clay Caterpillar By Predation Type
#Set Working Directory
setwd("C:/Users/Dunham/Desktop/Rogers/research/Rogers/REU 2013")
data <-read.csv("caterpillarmetadata.csv", header=T)
data

#Questions
#1)

library(lme4) 

##Model One - Most Accuarate Version##
m1 <- glm(predation~bird*type+site, data = data, family=binomial)
summary(m1)

#to do- figure out how to analyze the predation marks

#BarPlots
library(sciplot)
bargraph.CI(island,result, group=type, data=data, xlab="Island", ylab="Proportion predated", legend=T)
bargraph.CI(island,site, group=type, data=data, xlab="Island", ylab="Site Predation", legend=T)
bargraph.CI(site,type, group=type, data=data, xlab="Site", ylab="Predation By Type", legend=T)
