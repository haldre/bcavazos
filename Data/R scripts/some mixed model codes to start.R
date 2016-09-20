caterpillars <- read.table ("C:\\Users\\Elizabeth\\Dropbox\\Postdoc\\MEAL\\SCIENCE\\REU 2013\\R scripts\\othercaterpillars.txt", header=T, sep="\t")
d<-caterpillars

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

#You will then need to put these figures into a table 
#(copy and paste into excel if necessary to drawgraphs with them)

