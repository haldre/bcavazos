#Clay Caterpillar Analysis
#based on caterpillars.csv file created in REU EBL file on desktop

require(graphics)
setwd("C:\\Users\\Anybody\\Desktop\\EBL REU 2013")

caterpillars <- read.csv("caterpillars.csv", header=TRUE)
attach(caterpillars)
names(caterpillars)
summary(caterpillars)


d$bird<-ifelse(d$island=="guam","no","yes")
d$bird<-as.factor(d$bird)
d
####Summary Statistics
summary(result, na.rm=T) #Summary of raw data
table(result, island, type) #shows count of results per island
data = table(island, result)
table(result)/length(result) #shows frequency of result per caterpillars
barplot(table(result, bird))
?table


###Anova###
data.ex2=read.csv("caterpillars.csv",header=TRUE)
data.ex2
aov.ex2 = aov(result~island,data=data.ex2)#do the analysis of variance
caterpillars
summary(aov.ex2) 

head(caterpillars)
summary(caterpillars)

##Mixed Effects Trial One##
#Does predation differ between island and treatment?
library(lme4)
model1<-lm(result~island*type, data=d)
summary(model1)
model2<-lm(result~island+type, data=d)
model3<-lm(result~island, data=d)
AIC(model1, model2, model3)
model3glht<-glht(model3, linfct=mcp(island="guam"))
summary(model3glht)

##Mixed Effects Trial Two##
lmm.data.ex2 <- read.csv("othercaterpillars", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
summary(lmm.data.ex2) 
library(lme4)

lmm.2 <- lmer(formula = extro ~ open + agree + social + class + (1|school/class), data 
= lmm.data, family = gaussian, REML = TRUE, verbose = FALSE) 



                            

