#The objective of this script is to preprocess some data for calssification in WEKA. Since there is not much I want to do to the data, and because I have been working extenisively in R recently, I assumed that it would be faster to do the preprocessing in R. 

#If you want WEKA to take a variable and use it as numeric, it cannot contain any NA values. The apical diameter values in our dataset have some NAs, so we need to remove those. 

hc <- read.csv("~/Desktop/newhc-dvrc.csv")

weka <- subset(hc, select=c(APCLD, KINOB, nn.dists, dv.real, rc.real))
weka <- na.omit(weka)

write.csv(weka, "~/Desktop/wekahc.csv", row.names=F)

