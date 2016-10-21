setwd("Data/Butterflies/WorkingData")
butterflydata <- read.csv("butterflydata.csv")


with(butterflydata, table(island,type)) #only 7 sites on Rota, 11 on Saipan, 19 on Guam


