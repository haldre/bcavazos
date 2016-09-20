####Brittany Cavazos
####Caterpillar Data from Clay-Caterpillar Expt.
####Last edited:Sept. 18, 2016
####Goal:to "clean" up data for easier analysis
##################################################
##################################################

# read in caterpillar data

caterpillar<-read_excel("Caterpillar Predation data.xlsx", sheet="caterpillars")
predation<-read_excel("Caterpillar Predation data.xlsx", sheet="predationID")

str(caterpillar)
unique(caterpillar$result)
# there are "missing (n=117)" and "question(n=11)" ones we may want to remove bc unsure if predated or not, 1650 total so losing about 8% there if taken out
summary(as.factor(caterpillar$result))

colnames(caterpillar)[4]<-"habitat"
# it was being weird about subsetting so i'm doing it piecewise which i know is not the most efficient way
caterpillar2<-caterpillar[!caterpillar$result=="question",]
caterpillar3<-caterpillar2[!caterpillar2$result=="missing",]

# HA I WIN

# so now that we took out the missing and question we can move on to fixing predation and merging them
names(caterpillar3)

names(predation)
predation<-predation[,1:5]
names(predation)<-tolower(names(predation))
str(predation)
predation$island<-as.factor(tolower(predation$island))
predation$habitat<-as.factor(tolower(predation$habitat))
predation$site<-as.factor(tolower(predation$site))

summary(predation$habitat)
# why is there a leucana as a habitat? was this done intentionally? I'm 90% sure this is supposed to be disturbed because there is only once occurence...and its in marbod, which I believe is marbo_disturbed
# because this messes up with merging the info, I'm switching it back to disturbed
predation$habitat <- gsub("leucana", "disturbed",predation$habitat)
# we also need to change the names around in site so they match each other
# in caterpillars - change anao to anao_n, ladtdn to ladt_n, marbo to marbo_d
# in predation - anaon to anao_n, blas to southblas, ladtn to ladt_n, marbod to marbo_d, tlp to twolovers, tweks to tweksberry
caterpillar3$site <- gsub("anao", "anao_n",caterpillar3$site)
caterpillar3$site <- gsub("ladtdn", "ladt_n",caterpillar3$site)
caterpillar3$site <- gsub("marbo", "marbo_d",caterpillar3$site)

predation$site <- gsub("anaon", "anao_n",predation$site)
predation$site <- gsub("blas", "southblas",predation$site)
predation$site <- gsub("ladtn", "ladt_n",predation$site)
predation$site <- gsub("marbod", "marbo_d",predation$site)
predation$site <- gsub("tlp", "twolovers",predation$site)
predation$site <- gsub("tweks", "tweksberry",predation$site)

unique(caterpillar3$site)
unique(predation$site)

#now merging them should work better -- before it was just assigning NAs to predation type even if event had occured

summary(predation$number)
# it seemed to have turned the weird ones (seemingly two pieces of the same individual that were predated on into NAs - there were two seperate occurances of this, the two pieces were labeled #a and #b) 
# I should decide whether to just leave them out altogether or assign them as one predation event
# for now I'll take them out
predation<-predation[!is.na(predation$number),]

summary(as.factor(predation$type))
# again, here there are some weird things - 41 occurances of a * (unrecognizable predation marking) and 9 occurances of "??" (predation in question); there are also 6 "NP"s, which are no predations - so I'll be taking those out for sure
# I will keep the 41 occurances of * for total predation-type analyses but get rid of not sure about ?? 
# need to change the ones that =NP to switch result to unpredated

str(caterpillar3)
caterpillar3$island<-as.factor(caterpillar3$island)
caterpillar3$habitat<-as.factor(caterpillar3$habitat)
caterpillar3$site<-as.factor(caterpillar3$site)

str(predation)
predation$island<-as.factor(predation$island)
predation$habitat<-as.factor(predation$habitat)
predation$site<-as.factor(predation$site)

#str of variables should match or it gives a warning message (predation habitat does not match caterpillar habitat because of the weird leucana variable found earlier. as of right now, i can't find where it is a problem.. and im not sure why but I'll investigate it later)
# so at this point, if we were to merge, we would want 1522 observations and 8 variables

test<-left_join(caterpillar3, predation, by = NULL)
# this supposedly looks correct
