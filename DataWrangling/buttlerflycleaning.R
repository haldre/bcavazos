####Brittany Cavazos
####Butterfly Data from Clay-Caterpillar Expt.
####Last edited:Sept. 18, 2016
####Goal:to "clean" up data for easier analysis
##################################################
##################################################
library(readxl)
library(tidyr)
library(dplyr)
#set working directory (I'll try to pull this directly out of box later -- technical difficulties w/ BoxSync)
setwd("~/EEB_590")

#Read in butterfly data
rawbutterfly<-read_excel("ButterflySurveyData.xlsx", sheet = "butterflyraw")
sitelevel<-read_excel("ButterflySurveyData.xlsx", sheet="SiteInfo")

# Goals: transform data to have one observation per row, assign site level information to observation by merging site level with rawbutterfly, check for anything weird goin on

# some decision making: guam-marbod-disturbed site had conflicting data between rawbutterly and site level, for now, I will use information from rawbutterfly as the true data, but will probably remove this site at the end.. .have to think about it though because it changes the data a bunch
names(rawbutterfly)
#get rid of last column w/ comment in it -- comment is recorded in script above (line 18)
rawbutterfly<-rawbutterfly[,1:10]

# now we can try to use gather to melt data to single observations per row
rawbutterfly2<-gather(rawbutterfly, "species_name", "number_indiv", 5:10, factor_key=T)
# that worked, but now we want to remove when there is a spp recorded w/ 0 observations
# there is a way to do this in that one line I think, but I can also subset to everything but number_indiv=0
rawbutterfly3<-rawbutterfly2[!rawbutterfly2$number_indiv=="0",]
# that worked

# quick check to see if anything is weird with variables --
unique(rawbutterfly3$island)
unique(rawbutterfly3$site) # there are 13 sites; no obvious misspellings
unique(rawbutterfly3$type)
# good to go

# now with sitelevel data, there are a couple things that come to mind -- site should probably be unique, but in come cases it is not - depending on if the survey was disturbed or native 
# we can remove notes and butterflies columns because they don't mean anything to us
# we have to do a coupld of things first to make the variables 'match' when we try to merge the data sets

# first, get rid of last row with no information
sitelevel<-sitelevel[1:21,]
# we can also get rid of butterfly and notes column
sitelevel$Butterflies<-NULL
sitelevel$notes<-NULL

# make everything lower case
names(sitelevel)<-tolower(names(sitelevel))

# change native/disturbed to type to match name in rawbutterfly (also to match sites in caterpillar data)
colnames(sitelevel)[3]<-"type"
# change transect length (m) to distance to match name in rawbutterfly
colnames(sitelevel)[9]<-"distance"

# back to making everything important lowercase
sitelevel$island<-tolower(sitelevel$island)
sitelevel$site<-tolower(sitelevel$site)
sitelevel$type<-tolower(sitelevel$type)

# now we need to fix the names of the sites
unique(sitelevel$site)
unique(rawbutterfly3$site)

# within raw butterflies - make ladtd just ladt, marbod to marbo and marbon to marbo, anaon to anao
# within sitelevel - make two lovers point - twolovers, south blas to sblas, marbo disturbed to marbo, and marbo native to marbo, gcc limestone to gcc, anao native to anao, bird island lane to birdisland, naftan point to naftan

rawbutterfly3$site <- gsub("marbod", "marbo_d",rawbutterfly3$site)
rawbutterfly3$site <- gsub("marbon", "marbo_n",rawbutterfly3$site)
rawbutterfly3$site <- gsub("anaon", "anao_n",rawbutterfly3$site)

sitelevel$site <- gsub("two lovers point", "twolovers",sitelevel$site)
sitelevel$site <- gsub("south blas", "sblas",sitelevel$site)
sitelevel$site <- gsub("marbo disturbed", "marbo_d",sitelevel$site)
sitelevel$site <- gsub("marbo native", "marbo_n",sitelevel$site)
sitelevel$site <- gsub("gcc limestone", "gcc",sitelevel$site)
sitelevel$site <- gsub("anao native", "anao_n",sitelevel$site)
sitelevel$site <- gsub("bird island lane", "birdisland",sitelevel$site)
sitelevel$site <- gsub("naftan point", "naftan",sitelevel$site)

# now we can merge these two together without it freaking out about names
# so we want island, site type and distance to merge to add  should all match - five more columns should be added to make 29x11 final datasheet

test1<-left_join(rawbutterfly3, sitelevel, by = NULL) # leaves out sites surveyed with no butterflies
test2<-right_join(rawbutterfly3, sitelevel, by = NULL) # adds in sites with no butterflies, so some NAs
test2$number_indiv[is.na(test2$number_indiv)]<-0
# butterfly totals would be useful - but in order to do that I need to make each survey a unique occurance, currently I can only do that with the use of multiple rows. I can create a new row that combines them together to serve as a unique identifier 

###################################################################################
###################################################################################

total_butterfly<-aggregate(test2$number_indiv, by=list(test2$site), FUN=sum)
colnames(total_butterfly)<-c("site","tot_butterfly")
total_butterfly$tot_butterfly[is.na(total_butterfly$tot_butterfly)]<-0

test3<-left_join(test2, total_butterfly, by=NULL)

# was having trouble getting anything to plot -- gotta change from chr to factors
str(test3)
test3$site<-as.factor(test3$site)
test3$island<-as.factor(test3$island)
test3$type<-as.factor(test3$type)


# depending on how we want this put together this part is nearly ready for actual analysis
# then we can write.csv and archive the raw data
# write.csv(datasetname, "datasetname.csv", row.names=F)

plot(test3$site, test3$tot_butterfly)
plot(test3$type, test3$tot_butterfly)
plot(test3$island, test3$tot_butterfly)
