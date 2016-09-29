require(graphics)
setwd("~/Dropbox/MEAL/SCIENCE/REU 2013")
setwd("~/Documents/Dropbox/EBL Project/SCIENCE/REU 2013/R scripts")
setwd("~/Dropbox/EBL Project/SCIENCE/Food web/REU 2013 Butterflies and caterpillars/R scripts") #haldre's desktop

#######################
#For Clay Caterpillars#
caterpillars <- read.table("C:\\Users\\Anybody\\Desktop\\EBL REU 2013\\butterfly", header=T, sep="\t")
caterpillars<-read.csv("othercaterpillars.csv", header=T)
d<-caterpillars

library(sciplot)
bargraph.CI(island,result, group=type, data=d, xlab="Island", ylab="proportion predated", legend=T)
bargraph.CI(site,result, data=subset(d, d$type=="native"), xlab="Island", ylab="proportion predated", main="Native")
bargraph.CI(site,result, data=subset(d, d$type=="disturbed"), xlab="Island", ylab="proportion predated", main="Disturbed")
bargraph.CI(bird, result, data=subset(d, d$type=="disturbed"), xlab="bird", ylab="proportion predated", main="Disturbed")

library(lme4)

#creating 2 columns of predated and unpredated
d$pred<-ifelse(d$result=="1", 1, 0)
d$unpred<-ifelse(d$result=="0", 1, 0)

d$bird<-ifelse(d$island=="guam","no","yes")
d$bird<-as.factor(d$bird)

#binding those together into one vector
d$new<-cbind(d$pred, d$unpred)

#The model
m1 <- lmer(result ~ bird*type + (1|site), family=binomial, data=d,REML=F)
summary(m1)

m2<-glmer(result~bird+type+(1|site), family=binomial, data=d, REML=F)
summary(m2)

m3<-glmer(result~type+(1|site), family=binomial, data=d, REML=F)
summary(m3)
anova(m2,m3) ##

#############
#To get the means for each of your treatment combinations 
d$bird_type <- factor(paste(d$bird, d$type))  #creates a new column that combines each combination of island and type

#Writing a model to getthemeans foreachof thosecombinations,taking your random effects into account
m1m<- lmer(result ~ bird_type - 1 + (1|site), family=binomial, data=d,REML=F)
summary(m1m)

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
butterfly <- read.csv("butterlyraw.csv", header=TRUE)
names(butterfly)
summary(butterfly)
butterfly$bird<-ifelse(butterfly$island=="guam","no","yes")
butterfly$bird<-as.factor(butterfly$bird)

####Summary Statistics
summary(total, na.rm=T) #Summary of raw data
table(island, total, type) #shows count of results per island
data = table(island, total)
table(total)/type(total) #shows frequency of result per caterpillars
barplot(table(total))

bargraph.CI(bird,total, group=type, data=butterfly, xlab="Bird", ylab="number of butterflies", legend=T)

m1 <- glm(total ~ bird*type+distance, data=butterfly, family="poisson")
summary(m1)

m2 <- glm(total ~ bird+type+distance, data=butterfly, family="poisson")
m3<- glm(total ~ type+distance, data=butterfly, family="poisson")
anova(m1,m2) # best model is model 1
anova(m2,m3)

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