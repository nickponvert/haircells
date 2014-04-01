library(MASS)
data.raw <- read.csv("~/Desktop/Haircells/haircells.csv")

#Drop all rows with NA for any value
data.lda <- na.omit(data.raw)

lda.funcs  <- lda(data.lda$CLASS ~ data.lda$SCNUM + data.lda$APCLD +  data.lda$DIST)

lda.data  <- subset(data.lda, select=c(SCNUM, APCLD, DIST))

#The entire function below is useless. Use predict()
#calclda  <- function(variables, loadings){
    #as.data.frame(variables)
    #numsamples  <- nrow(variables)
    #ld  <- numeric(nummples)
    #numvariables <- length(varables)
    #for (i in 1 : numsamples) {
        #valuei  <- 0
        
        #for (j in 1 : numvariables){
            #valueij  <- variables[i,j]
            #loadingj  <- loadings[j]
            #valuei  <- valuei + (valueij * loadingj)
        #}
        #ld[i]  <- valuei
    #}

    ##Standardize the descriminant function so mean=0
    #ld <- as.data.frame(scale(ld, center=TRUE, scale=FALSE))
    #ld <- ld[[1]]
    #return(ld)
#}

#This is the way to go. 
lda.values  <- predict(lda.funcs, lda.data)
#Plot the group values for the first DF
ldahist(data=lda.values$x[,1], g=data.lda$CLASS, col='grey')

g0 <- data.lda$CLASS==0
g1 <- data.lda$CLASS==1
g2 <- data.lda$CLASS==2
g3 <- data.lda$CLASS==3

plot(lda.values$x[,1], lda.values$x[,2], las=1, asp=1, type='n', ylab='Second Descriminant Function', xlab="First Descriminant Function")
points(lda.values$x[g0,1], lda.values$x[g0,2],pch=16, cex=0.7, col="red")
points(lda.values$x[g1,1], lda.values$x[g1,2],pch=16, cex=0.7, col="yellow")
points(lda.values$x[g2,1], lda.values$x[g2,2],pch=16, cex=0.7, col="green")
points(lda.values$x[g3,1], lda.values$x[g3,2],pch=16, cex=0.7, col="blue")
