require(graphics)
setwd("~/Dropbox/MEAL/SCIENCE/REU 2013")

#######################
#For Clay Caterpillars#
setwd("~/Documents/Dropbox/EBL Project/SCIENCE/REU 2013/R scripts")
caterpillars <- read.table("C:\\Users\\Anybody\\Desktop\\EBL REU 2013\\butterfly", header=T, sep="\t")
caterpillars<-read.csv("othercaterpillars.csv", header=T)
d<-caterpillars

library(sciplot)
bargraph.CI(island,result, group=type, data=d, xlab="Island", ylab="proportion predated", legend=T)
bargraph.CI(site,result, data=subset(d, d$type=="native"), xlab="Island", ylab="proportion predated", main="Native")
bargraph.CI(site,result, data=subset(d, d$type=="disturbed"), xlab="Island", ylab="proportion predated", main="Disturbed")

library(lme4)

#creating 2 columns of predated and unpredated
d$pred<-ifelse(d$result=="1", 1, 0)
d$unpred<-ifelse(d$result=="0", 1, 0)

#binding those together into one vector
d$new<-cbind(d$pred, d$unpred)

#The model
m1 <- lmer(new ~ island*type + (1|site), family=binomial, data=d,REML=F)
summary(m1)

#To get the means for each of your treatment combinations 
d$island_type <- factor(paste(d$island, d$type))  #creates a new column that 
                                                  #combines each combination of island and type

#Writing a model to getthemeans foreachof thosecombinations,taking your random effects into account
m1m<- lmer(new ~ island_type - 1 + (1|site), family=binomial, data=d,REML=F)

#Because it's binomial the results are on thelogit scale so you need to back transform them
unlogit <- function(x) exp(x) / (1 + exp(x))
unlogit(fixef(m1m))

#This createsthe confidence intervals
#Extract them
r<-sqrt(diag(vcov(m1m)))
#Confidence limits - do sums then transform
ucl <- unlogit(fixef(m1m) + 2*r)
lcl <- unlogit(fixef(m1m) - 2*r)


###################
#Butterfly Survey#


#Possible Mixed Model for Butterfly Survey
butterflies=read.table("otherbutterflies", header=TRUE)
attach(butterflies)
m1 <- lmer(total ~ island*type + (1|site),data=butterflies,REML=F)
summary(m1)

#Possible ANOVA for Butterfly Data

require(graphics)
setwd("~/Dropbox/MEAL/SCIENCE/REU 2013")

butterfly <- read.csv("butterlyraw.csv", header=TRUE)
attach(butterfly)
names(butterfly)
summary(butterfly)

####Summary Statistics
summary(total, na.rm=T) #Summary of raw data
table(island, total, type) #shows count of results per island
data = table(island, total)
table(total)/type(total) #shows frequency of result per caterpillars
barplot(table(total))

?table


###Anova###
data.ex2=read.csv("butterlyraw.csv",header=TRUE)
data.ex2
aov.ex2 = aov(total~island*type,data=data.ex2)#do the analysis of variance
summary(aov.ex2) 

head(butterfly)
summary(butterfly)

##Mixed Effects##
library(lme4)
m1 <- lmer(total ~ island*type + (1|site) + (1|distance), REML=FALSE, data=data.ex2)
summary(m1)
anova(m1)

##Mixed Effects Trial Two##
library(lme4) 
model1<-lm(total~island*type+(distance)+(site), data=data.ex2)
summary(model1)
model2<-lm(total~island+type+(distance)+(site), data=data.ex2)
summary(model2)
model3<-lm(total~island+(distance)+(site), data=data.ex2)
summary(model3)
AIC(model1, model2, model3)
aov(model1)

##Zero-Inflated Model for Butterfly Data##
require(ggplot2)
require(pscl)
require(boot)
library(ggplot2)
library(pscl)
library(boot)
summary(butterfly)

#Model1
summary(m1 <-zeroinfl(total~island+type | distance, data = butterfly))
#Model2
summary(m2 <-zeroinfl(total~island+type | site))