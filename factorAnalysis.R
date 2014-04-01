hc <- read.csv("~/Desktop/Haircells/haircells.csv")
hc.f <- subset(hc, select=c(SCNUM, APCLD, KINOB, KINOT, DIST))

hc.f <- na.omit(hc.f)

library(psych)

fa.parallel(hc.f)
