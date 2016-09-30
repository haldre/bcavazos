####Brittany Cavazos
####Caterpillar Data from Clay-Caterpillar Expt.
####Last edited:Sept. 29, 2016
####Goal:to "clean" data for easier analysis
##################################################
##################################################
library(readxl)
library(tidyr)
library(dplyr)
# read in caterpillar data

caterpillar<-read_excel("C:\\Users\\brittanycavazos\\Documents\\EEB_590\\ClayCaterpillarProject\\bcavazos\\Data\\Caterpillars\\RawData\\Caterpillar Predation data.xlsx", sheet="caterpillars")
predation<-read_excel("C:\\Users\\brittanycavazos\\Documents\\EEB_590\\ClayCaterpillarProject\\bcavazos\\Data\\Caterpillars\\RawData\\Caterpillar Predation data.xlsx", sheet="predationID", col_types=c("text", "text", "text", "text", "text", "text", "text", "text"))
# ^ had a problem w/ predation data with read_excel - had to add the col_types w/ 8 "text"s to have it read the number column as non-numeric # it worked! even though they are all characters now...


str(caterpillar)
unique(caterpillar$result)
# there are "missing (n=117)" and "question(n=11)" ones we may want to remove bc unsure if predated or not, 1650 total so losing about 8% there if taken out
summary(as.factor(caterpillar$result))

colnames(caterpillar)[4]<-"habitat"

# it was being weird about subsetting so i'm doing it piecewise which i know is not the most efficient way
caterpillar<-caterpillar[!caterpillar$result=="question",]
caterpillar<-caterpillar[!caterpillar$result=="missing",]

# we dont care about the analysis for Quality check and notes so we can take out those two columns
caterpillar<-caterpillar[,-9]
caterpillar<-caterpillar[,-8]

# so now that we took out the missing and question we can move on to fixing predation and merging them
names(caterpillar)

names(predation)
# get rid of two empty columns
predation<-predation[,1:6]
# first let's take out the ones that were mislabeled (these were ones that appeared to be assigned predation categories, but were marked as unpredated in caterpillars spreadsheet - that spreadsheet has been datachecked so these must be mislabeled)
predation<-predation[is.na(predation$Notes==T),]
# now we can take out the notes column because it's irrelevant 
predation<-predation[,1:5]

names(predation)<-tolower(names(predation))
str(predation)
predation$island<-as.factor(tolower(predation$island))
predation$habitat<-as.factor(tolower(predation$habitat))
predation$site<-as.factor(tolower(predation$site))

levels(predation$habitat)
# leucana = disturbed
predation$habitat <- gsub("leucana", "disturbed",predation$habitat)
# we also need to change the names around in site so they match each other
# in caterpillars - change anao to anao_n, ladtdn to ladt_n, marbo to marbo_d
# in predation - anaon to anao_n, blas to southblas, ladtn to ladt_n, marbod to marbo_d, tlp to twolovers, tweks to tweksberry
caterpillar$site <- gsub("anao", "anao_n",caterpillar$site)
caterpillar$site <- gsub("ladtdn", "ladt_n",caterpillar$site)
caterpillar$site <- gsub("marbo", "marbo_d",caterpillar$site)

predation$site <- gsub("anaon", "anao_n",predation$site)
predation$site <- gsub("blas", "southblas",predation$site)
predation$site <- gsub("ladtn", "ladt_n",predation$site)
predation$site <- gsub("marbod", "marbo_d",predation$site)
predation$site <- gsub("tlp", "twolovers",predation$site)
predation$site <- gsub("tweks", "tweksberry",predation$site)

unique(caterpillar$site)
unique(predation$site)

#now merging them should work better -- before it was just assigning NAs to predation type even if event had occured

summary(as.factor(predation$type))
# again, here there are some weird things - 41 occurances of a * (unrecognizable predation marking) and 9 occurances of "??" (predation in question); there are also 6 "NP"s, which are no predations - so I'll be taking those out for sure
# need to change the ones that =NP to switch result to unpredated

str(caterpillar)
caterpillar$island<-as.factor(caterpillar$island)
caterpillar$habitat<-as.factor(caterpillar$habitat)
caterpillar$site<-as.factor(caterpillar$site)

str(predation)
predation$island<-as.factor(predation$island)
predation$habitat<-as.factor(predation$habitat)
predation$site<-as.factor(predation$site)
predation$number<-as.character((predation$number))

predation$uniqueID<-paste(predation$site, predation$habitat, predation$number, sep = "-")
caterpillar$uniqueID<-paste(caterpillar$site, caterpillar$habitat, caterpillar$number, sep = "-")


#str of variables should match or it gives a warning message (predation habitat does not match caterpillar habitat because of the weird leucana variable found earlier. as of right now, i can't find where it is a problem.. and im not sure why but I'll investigate it later)
# so at this point, if we were to merge, we would want 1522 observations and 8 variables

caterpillardata<-left_join(caterpillar, predation, by = NULL)
# this worked

write.csv(caterpillardata, "C:\\Users\\brittanycavazos\\Documents\\EEB_590\\ClayCaterpillarProject\\bcavazos\\Data\\Caterpillars\\WorkingData\\caterpillardata.csv", row.names = F)
