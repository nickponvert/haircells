data <- read.csv("~/Desktop/Haircells/haircells.csv")
data <- subset(data, select=c(SCNUM, APCLD, KINOB, KINOT, DIST,CLASS))
#Plot a PCA biplot with points colored based on whether they are peripheral, internal, or extramaculary. 
#The desired columns can be passed to this script as a dataframe called `data`
library(polycor)
data<-na.omit(data) #Remove any rows that contian NAN values

c0 <- data$CLASS == 0 #No Kinociliary Bulb, Kinocilium not taller
c1 <- data$CLASS == 1 #No kinociliary Bulb, Kinocilium is taller
c2 <- data$CLASS == 2 #Kinociliary bulb, kinocilium is not taller
c3 <- data$CLASS == 3 #Kinociliary bulb, kinocilium is taller

data <- subset(data, select=-c(CLASS))

C<-as.matrix(hetcor(data=data)) #Use heterogenous correlation to generate a correlation matrix
het.pca<-prcomp(data, retx=T, scale=T, covmat=C) #Compute the PCA



loadings<-het.pca$rotation #loading vectors for each PC
scores<-het.pca$x #The points translated into the new PC space
rownames(loadings)<-colnames(data) #Assign variable names to each loading vector

internal<-na.omit(hc)$DIST>0 #Internal haircell bundles (hcb) are all bundles with a positive distance to the perimer (This could be altered though)
peri<-na.omit(hc)$DIST==0 #Perimeter hcb are all those with a distance of 0
em<-na.omit(hc)$DIST<0 #Extramaculary hcb are those with a negative distance value

#Plot the scores, but do not add the points yet. The xlim and ylim should be adjusted as needed
plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", type="n", asp=1, las=1, xlim=c(-4,4), ylim=c(-4,4))

#Plot the points in different colors depending on whether they are internal, peripheral, or extramaculary
points(scores[c0,1], scores[c0,2], pch=16, cex=0.7, col="red")
points(scores[c1,1], scores[c1,2], pch=16, cex=0.7, col="yellow")
points(scores[c2,1], scores[c2,2], pch=16, cex=0.7, col="green")
points(scores[c3,1], scores[c3,2], pch=16, cex=0.7, col="blue")

text(-3, 3, "No Bulb, Not Taller", col='red')
text(-3, -3, "No Bulb, Taller", col='yellow')
text(3, 3, "Bulb, Not Taller", col='green')
text(3, -3, "Bulb, Taller", col='blue')
#Add the vectors for the variable loadings in the PCs
scaling<-4
textNudge<-1.25
arrows(0, 0, loadings[,1]* scaling, loadings[,2]* scaling, length=0.1, angle=20, col="red")
text(loadings[,1]*scaling*textNudge, loadings[,2]*scaling*textNudge, rownames(loadings), col="red", cex=0.7)
