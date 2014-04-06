
hc <- read.csv("~/Current/DataHaircell/haircells.csv")

xy <- subset(hc, select=c(XLOC, YLOC, CLASS, DIST, APCLD))
xy <- na.omit(xy)

c0 <- xy$CLASS == 0 #No Kinociliary Bulb, Kinocilium not taller
c1 <- xy$CLASS == 1 #No kinociliary Bulb, Kinocilium is taller
c2 <- xy$CLASS == 2 #Kinociliary bulb, kinocilium is not taller
c3 <- xy$CLASS == 3 #Kinociliary bulb, kinocilium is taller

plot(xy$DIST, xy$APCLD, type='n', xlab="Distance", ylab="Apical Diameter", main="DIST, APCLD versus KC Class", ylim=c(min(xy$APCLD)-10, max(xy$APCLD)+10), xlim=c(-10, max(xy$DIST)+10) )
points(xy$DIST[c0], xy$APCLD[c0], pch=16, cex=0.7, col="red") #Class 0
points(xy$DIST[c1], xy$APCLD[c1], pch=16, cex=0.7, col="yellow") #Class 1
points(xy$DIST[c2], xy$APCLD[c2], pch=16, cex=0.7, col="green") #Class 2
points(xy$DIST[c3], xy$APCLD[c3], pch=16, cex=0.7, col="blue") #Class3

