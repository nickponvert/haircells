#04.03.14
#Computing Haircell Density

#OBJECTIVE: Compute the haircell density for the saccule dataset. Before we can accomplish this, we need to repair the distance to perimeter data after we reclassifed a previously misclassified cell (was misclassified as perimeter cell and threw off our calculations.)


################################################################################################################    
#########################TO rerun this file, get all variables to point to the same csv###########################
hc <- read.csv("~/Desktop/newhc-dvrc.csv")
newhc <- read.csv("~/Desktop/newhc-dvrc.csv")
newhc.2 <- read.csv("~/Desktop/newhc-dvrc.csv")
################################################################################################################    
################################################################################################################    


#The current haircell data file
hc <- read.csv("~/Current/DataHaircell/hc-NEW-CORRECTED.csv")

#Plot the perimeter points
plot(hc$XLOC, hc$YLOC, type='n')
peri <- hc$PERI ==1
nonperi <- hc$PERI ==0
points(hc$XLOC[peri], hc$YLOC[peri], col="red")
points(hc$XLOC[nonperi], hc$YLOC[nonperi], col="black")
#We are going to use a nearest-neighbor algorithm to determine the distance between each haircell and the closest perimeter haircell. To visualize the areas that correspond to each perimeter haircell, we can construct a Voroni diagram for our perimeter haircell points. 
library(deldir)
x <- hc$XLOC
y <- hc$YLOC
xp <- x[peri]
yp <- y[peri]

vt <- deldir(xp, yp)
par(mar=rep(1,4))
plot(xp, yp, axes=F, pch=16)
plot(vt, wlines="tess", lty="solid", add=T)
box()

#TO actually calculate the distance between the points and the perimeter cells, we will use an adaptation of the ANN nearest-neighbor search algorithm in the package RANN. 
library(RANN)
#The query dataset  
xy=subset(hc, select=c(XLOC, YLOC))
#The dataset against which the query points will be searched
PXLOC<-xy$XLOC[peri]
PYLOC<-xy$YLOC[peri]

xyp<-data.frame(PXLOC, PYLOC)
#Condust the NN search 
nn<-nn2(data=xyp, query=xy, k=1)

#Concatenate the NN results onto the end of the haircell dataframe and save it as a new cs
nndf<-data.frame(hc, nn)
write.csv(nndf, "~/Desktop/newhc.csv", row.names=F)

#INTERMEDIATE STEP
#At this point, I opened the newhc.csv file in LIbreoffice Calc, sorted the file by OUT cells, and manually made each OUT distance value negative. This seemed easier than figuring out how to code for it because there are so few out cells. 

#The new, corrected data file. 
newhc<-read.csv("~/Desktop/newhc.csv")

##############################################################################################################
################################XY plotting routeines#########################################################
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

############################################################################################################

#Plot the nn.dists against the XY location of the points to confirm that the problems with the distances have been fixed. 

newhc <- read.csv("~/Desktop/newhc.csv")
z=newhc$nn.dists     
layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(newhc$XLOC, newhc$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Distance to Perimeter")
points(newhc$XLOC, newhc$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))

##The results of the aboce graph show that we have successfully repaired the distance to perimeter dataset.
##The second objective of this analysis is to correct the dorsal-ventral measurement (DV), which is rotated 90 degrees at this point and actually points rostrally. 
#We can also compute the distance rostrally, which would potentially be a good piece of information to have for a classifier. 

xmin <- min(newhc$XLOC)
ymin <- min(newhc$YLOC)

dv.real <- newhc$XLOC - xmin #Compute distance DV
rc.real <- newhc$YLOC - ymin #Compute RC

newhc.2 <- data.frame(newhc, dv.real, rc.real)

write.csv(newhc.2, "~/Desktop/newhc-dvrc.csv", row.names=F)

#Confirm that our calculations produce acceptable results

z=newhc.2$dv.real
layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(newhc.2$XLOC, newhc.2$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Dorsoventral Distance")
points(newhc.2$XLOC, newhc.2$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))


#RC
z=newhc.2$rc.real
layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(newhc.2$XLOC, newhc.2$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Rostrocaudal Distance")
points(newhc.2$XLOC, newhc.2$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))

#DONE
#Onto the next objective: compute the haircell density. 
#Before we attempt to collect the data (Probably by using a kernel-density estimator), we can plot the density as a smoothed color function that will probably look good. 

library(grDevices)
dcol<-densCols(newhc.2$XLOC, newhc.2$YLOC, colramp=jet.colors)
graphics::plot(newhc.2$XLOC, newhc.2$YLOC, col=dcol, pch=20)

library(MASS)
dens <- kde2d(newhc.2$XLOC, newhc.2$YLOC)

# create a new data frame of that 2d density grid
# (needs checking that I haven't stuffed up the order here of z?)
gr <- data.frame(with(dens, expand.grid(x,y)), as.vector(dens$z))
names(gr) <- c("xgr", "ygr", "zgr")

# Fit a model
mod <- loess(zgr~xgr*ygr, data=gr)

# Apply the model to the original data to estimate density at that point
newhc.2$pointdens <- predict(mod, newdata=data.frame(xgr=newhc.2$XLOC, ygr=newhc.2$YLOC))

# Draw plot
z=newhc.2$pointdens

layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(6,1))
par(mar=c(5,5,3.5,1.5))
plot(newhc.2$XLOC, newhc.2$YLOC,las=1, asp=1, type='n', xlab="X", ylab="Y", main="Kernel Density Estimation")
points(newhc.2$XLOC, newhc.2$YLOC, pch=16, cex=0.7, col=cRamp(z))
color.bar(jet.colors(length(z)), minimum=min(z), maximum=max(z))

#This has potential
xy <- subset(newhc.2, select=c(XLOC, YLOC))
smoothScatter(xy, colramp=jet.colors)

#Using hexbins also ha potential
library(hexbin)
hexbinplot(YLOC~ XLOC, newhc.2, aspect = 1, trans = sqrt, inv = function(x) x^2, colramp=jet.colors)

#TODO: Try to get the point values for the points that fall into each hexbin

#TODO: Use the sp and rgeos libraries to compute the nearest neighbors within a certain radius
# http://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad
library(aqfig)
scatterplot.density(hc$XLOC, hc$YLOC)

grDevices:::.smoothScatterCalcDensity(x, nbin, bandwidth)
