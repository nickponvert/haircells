hc<-read.csv("~/Desktop/newhc-dvrc.csv")

hc.data <- subset(hc, select=c(APCLD, nn.dists, dv.real, rc.real))
library(polycor)
covmat <- as.matrix(hetcor(hc.data))

hc.data <- na.omit(hc.data)
hc.pca <- prcomp(hc.data,scale=T, retx=T, covmat=covmat)
loadings<-hc.pca$rotation #loading vectors for each PC
scores<-hc.pca$x #The points translated into the new PC space
rownames(loadings)<-colnames(hc.data) #Assign variable names to each loading vector

plot(scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", asp=1, las=1)
