####Brittany Cavazos
####Butterfly Data from Clay-Caterpillar Expt.
####Last edited:Sept. 18, 2016
####Goal:to "clean" up data for easier analysis
##################################################
##################################################
library(readxl)
library(tidyr)
library(dplyr)
# set working directory 
# setwd("~/EEB_590/ClayCaterpillarProject")

#Read in butterfly data
rawbutterfly<-read_excel("C:\\Users\\brittanycavazos\\Documents\\EEB_590\\ClayCaterpillarProject\\Data\\Butterflies\\RawData\\ButterflySurveyData.xlsx", sheet = "butterflyraw")
sitelevel<-read_excel("C:\\Users\\brittanycavazos\\Documents\\EEB_590\\ClayCaterpillarProject\\Data\\Butterflies\\RawData\\ButterflySurveyData.xlsx", sheet="SiteInfo")

# Goals: transform data to have one observation per row, assign site level information to observation by merging site level with rawbutterfly, check for anything weird goin on

# some decision making: guam-marbod-disturbed site had conflicting data between rawbutterly and site level, for now, I will use information from rawbutterfly as the true data, but will probably remove this site at the end.. .have to think about it though because it changes the data a bunch
names(rawbutterfly)
#get rid of last column w/ comment in it -- comment is recorded in script above (line 18)
rawbutterfly<-rawbutterfly[,1:10]

# now we can try to use gather to melt data to single observations per row
rawbutterfly<-gather(rawbutterfly, "species_name", "number_indiv", 5:10, factor_key=T)
# that worked, but now we want to remove when there is a spp recorded w/ 0 observations
# there is a way to do this in that one line I think, but I can also subset to everything but number_indiv=0
rawbutterfly<-rawbutterfly[!rawbutterfly$number_indiv=="0",]
# that worked

# quick check to see if anything is weird with variables --
unique(rawbutterfly$island)
unique(rawbutterfly$site) # there are 13 sites; no obvious misspellings
unique(rawbutterfly$type)
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
unique(rawbutterfly$site)

# within raw butterflies - make ladtd just ladt, marbod to marbo and marbon to marbo, anaon to anao
# within sitelevel - make two lovers point - twolovers, south blas to sblas, marbo disturbed to marbo, and marbo native to marbo, gcc limestone to gcc, anao native to anao, bird island lane to birdisland, naftan point to naftan

rawbutterfly$site <- gsub("marbod", "marbo_d",rawbutterfly$site)
rawbutterfly$site <- gsub("marbon", "marbo_n",rawbutterfly$site)
rawbutterfly$site <- gsub("anaon", "anao_n",rawbutterfly$site)
rawbutterfly$site <- gsub("ladtd", "ladt_d",rawbutterfly$site)


sitelevel$site <- gsub("two lovers point", "twolovers",sitelevel$site)
sitelevel$site <- gsub("south blas", "sblas",sitelevel$site)
sitelevel$site <- gsub("marbo disturbed", "marbo_d",sitelevel$site)
sitelevel$site <- gsub("marbo native", "marbo_n",sitelevel$site)
sitelevel$site <- gsub("gcc limestone", "gcc",sitelevel$site)
sitelevel$site <- gsub("anao native", "anao_n",sitelevel$site)
sitelevel$site <- gsub("bird island lane", "birdisland",sitelevel$site)
sitelevel$site <- gsub("naftan point", "naftan",sitelevel$site)
sitelevel$site[sitelevel$site=="ladt" & sitelevel$type=="native"] <- "latd_n"
sitelevel$site[sitelevel$site=="ladt" & sitelevel$type=="disturbed"] <- "ladt_d"

# now we can merge these two together without it freaking out about names
# so we want island, site type and distance to merge to add  should all match - five more columns should be added to make 29x11 final datasheet

butterflydata<-left_join(rawbutterfly, sitelevel, by = NULL) # leaves out sites surveyed with no butterflies
butterflydata<-right_join(rawbutterfly, sitelevel, by = NULL) # adds in sites with no butterflies, so some NAs
butterflydata$number_indiv[is.na(butterflydata$number_indiv)]<-0
# butterfly totals would be useful - but in order to do that I need to make each survey a unique occurance, currently I can only do that with the use of multiple rows. I can create a new row that combines them together to serve as a unique identifier 

# write.csv(butterflydata, "butterflydata.csv", row.names = F)
