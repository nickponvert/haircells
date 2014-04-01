hc <- read.csv("~/Desktop/Haircells/haircells.csv")

#############################################################################
#############Class Plot#####################################################
xy <- subset(hc, select=c(XLOC, YLOC, CLASS))

c0 <- xy$CLASS == 0 #No Kinociliary Bulb, Kinocilium not taller
c1 <- xy$CLASS == 1 #No kinociliary Bulb, Kinocilium is taller
c2 <- xy$CLASS == 2 #Kinociliary bulb, kinocilium is not taller
c3 <- xy$CLASS == 3 #Kinociliary bulb, kinocilium is taller

plot(xy$XLOC, xy$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Kinocilium Class")
points(xy$XLOC[c0], xy$YLOC[c0], pch=16, cex=0.7, col="red") #Class 0
points(xy$XLOC[c1], xy$YLOC[c1], pch=16, cex=0.7, col="yellow") #Class 1
points(xy$XLOC[c2], xy$YLOC[c2], pch=16, cex=0.7, col="green") #Class 2
points(xy$XLOC[c3], xy$YLOC[c3], pch=16, cex=0.7, col="blue") #Class3

text(2500, 0, "No Bulb, Not Taller", col='red')
text(10000, 0, "No Bulb, Taller", col='yellow')
text(2500, 16000, "Bulb, Not Taller", col='green')
text(10000, 16000, "Bulb, Taller", col='blue')

#############################################################################

#Custom jet color ramp palette
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#custom function to plot the value of a point as a color in the color ramp
range01 <- function(x)(x-min(x))/diff(range(x))
cRamp <- function(x){
  cols <- colorRamp(jet.colors(length(x)))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  

#function to plot a color bar in a subplot
color.bar <- function(lut, minimum, maximum, nticks=11, ticks=seq(minimum, maximum, len=nticks), title='') {
        scale = (length(lut)-1)/(maximum-minimum)
        par(mar=c(5,0,3.5,4))
        plot(c(0,10), c(minimum,maximum), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
        axis(4, ticks, las=1)
            for (i in 1:(length(lut)-1)) {
                     y = (i-1)/scale + minimum
             rect(0,y,10,y+1/scale, col=lut[i], border=NA)
                 }
}
#############################################################################
#########Apical Diameter Plot###############################################
dia <- subset(hc, select=c(XLOC, YLOC, APCLD))
dia <- na.omit(dia)
z=dia$APCLD


layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(dia$XLOC, dia$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main = "Apical Diameter")
points(dia$XLOC, dia$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))

#############################################################################
#########Bundle Width Plot####################################################
bndl <- subset(hc, select=c(XLOC, YLOC, BNDLW))
bndl <- na.omit(bndl)

z=bndl$BNDLW

layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(bndl$XLOC, bndl$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Bundle Width")
points(bndl$XLOC, bndl$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))


#############################################################################
#########Stereocilia Number Plot#############################################
scn <- subset(hc, select=c(SCNUM, XLOC, YLOC))
scn <- na.omit(scn)
z=scn$SCNUM
layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(scn$XLOC, scn$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Stereociliary Number")
points(scn$XLOC, scn$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))

#############################################################################
layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,3))

