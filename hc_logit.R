#This code is adapted from tutorials on the following websites:
#http://www.ats.ucla.edu/stat/r/dae/logit.htm
#http://stackoverflow.com/questions/14069629/plotting-confidence-intervals

library(ggplot2)
library(aod)
hc <- read.csv("~/Desktop/Haircell_Data/haircells.csv")
lr <- subset(hc, select=c(APCLD, DIST, KINOB, KINOT, CLASS))
lr <- na.omit(lr)
ad <- lr$APCLD
d <- lr$DIST
kb <- lr$KINOB
kt <- lr$KINOT
plot(d, ad)
boxplot(DIST~KINOB, data=lr)
t.test(DIST~KINOB, data=lr)

boxplot(DIST~KINOT, data=lr)
t.test(DIST~KINOT, data=lr)

library(polycor)

#Look at correlations between our variations in the heterogenous correlation matrix
covmat <- as.matrix(hetcor(lr))

#Construct a general linear model using the binomial family to do the logit

bglm <- glm(KINOB ~ DIST + APCLD, data=lr, family="binomial")
summary(bglm)

#Confidence intervals for the coefficient estimates using log-likliehood profiling
confint(bglm)

#CIs using standard errors
confint.default(bglm)

#Exponentiate to get odds-ratios instead of log-odds ratios
exp(coef(bglm))

#Use cbind to bind the odds ratios and the 95% CIs for the odds ratios together
exp(cbind(OR = coef(bglm), confint(bglm)))
#Use the Apical Diameter values, and hold the distance values the same. 
newdata1 <- with(lr, data.frame(DIST=mean(DIST), APCLD=APCLD))

#Use the distance values, and hold the apical diameter the same
newdata2 <- with(lr, data.frame(DIST=DIST, APCLD=mean(APCLD)))

#We can use the predict function to get the probabilities from the model with a range of inputs (newdata). 
newdata1$PROB <- predict(bglm, newdata=newdata1, type="response")

#Make a new data frame in which the Distance varies continuously and the apical diameter is held constant. 
newdata3 <- with(lr, data.frame(DIST=seq(from=min(DIST), to=max(DIST), length.out=100), APCLD=mean(APCLD)))

#Generate predicted probabilities, but get the standard errors as well so that we can calculate the upper and lower confidence intervals for the probability. 
newdata4 <- cbind(newdata3, predict(bglm, newdata = newdata3, type = "link", se = TRUE))
newdata4 <- within(newdata4, {
                       PredictedProb <- plogis(fit)
                       LL <- plogis(fit - (1.96 * se.fit))
                       UL <- plogis(fit + (1.96 * se.fit))
})


#Plot the best estimate for the probability with a dashed line, and the upper and lower confidence intervals with dashed lines. Can also plot a grey polygon over the area of the confidence interval. 
plot(newdata4$DIST, newdata4$PredictedProb, type='n')
#polygon(c(rev(newx), newx), c(rev(newdata4$UL), newdata4$LL), col="grey80", border=NA) 
lines(newdata4$DIST, newdata4$PredictedProb, lty="solid", col="black")
lines(newdata4$DIST, newdata4$UL, lty="dashed", col="black")
lines(newdata4$DIST, newdata4$LL, lty="dashed", col="black")


##################################################################
##################################################################


#Lets do a new logit without the Apical Diameter component and see how that influences the results

newlm <- glm(KINOB ~ DIST, data=lr, family="binomial")

newdata5 <- with(lr, data.frame(DIST=seq(from=min(DIST), to=max(DIST), length.out=100)))
newdata6 <- cbind(newdata5, predict(newlm, newdata = newdata5, type = "link", se = TRUE))
newdata6 <- within(newdata6, {
                       PredictedProb <- plogis(fit)
                       LL <- plogis(fit - (1.96 * se.fit))
                       UL <- plogis(fit + (1.96 * se.fit))
})
plot(newdata6$DIST, newdata6$PredictedProb, type='n', main="logit(Kinocilium Bulb Presence ~ Distance)", xlab="Distance to Perimeter", ylab="Predicted Probability of Kinocilium Bulb")
#polygon(c(rev(newx), newx), c(rev(newdata6$UL), newdata6$LL), col="grey80", border=NA) 
lines(newdata6$DIST, newdata6$PredictedProb, lty="solid", col="black")
lines(newdata6$DIST, newdata6$UL, lty="dashed", col="black")
lines(newdata6$DIST, newdata6$LL, lty="dashed", col="black")

##################################################################
##################################################################

#Lets use Apical Diameter as the factor now


newlm <- glm(KINOB ~ APCLD, data=lr, family="binomial")

newdata5 <- with(lr, data.frame(APCLD=seq(from=min(APCLD), to=max(APCLD), length.out=100)))
newdata6 <- cbind(newdata5, predict(newlm, newdata = newdata5, type = "link", se = TRUE))
newdata6 <- within(newdata6, {
                       PredictedProb <- plogis(fit)
                       LL <- plogis(fit - (1.96 * se.fit))
                       UL <- plogis(fit + (1.96 * se.fit))
})
plot(newdata6$APCLD, newdata6$PredictedProb, type='n', main="logit(Kinocilium Bulb Presence ~ Apical Diameter)", xlab="Apical Diameter", ylab="Predicted Probability of Kinocilium Bulb")
#polygon(c(rev(newx), newx), c(rev(newdata6$UL), newdata6$LL), col="grey80", border=NA) 
lines(newdata6$APCLD, newdata6$PredictedProb, lty="solid", col="black")
lines(newdata6$APCLD, newdata6$UL, lty="dashed", col="black")
lines(newdata6$APCLD, newdata6$LL, lty="dashed", col="black")

########################################################################
########################################################################
#Lets Look at the relationship between Distance and taller kinocilia

newlm <- glm(KINOT ~ DIST, data=lr, family="binomial")

newdata5 <- with(lr, data.frame(DIST=seq(from=min(DIST), to=max(DIST), length.out=100)))
newdata6 <- cbind(newdata5, predict(newlm, newdata = newdata5, type = "link", se = TRUE))
newdata6 <- within(newdata6, {
                       PredictedProb <- plogis(fit)
                       LL <- plogis(fit - (1.96 * se.fit))
                       UL <- plogis(fit + (1.96 * se.fit))
})
plot(newdata6$DIST, newdata6$PredictedProb, type='n', main="logit(Taller Kinocilium ~ Distance)", xlab="Distance to Perimeter", ylab="Predicted Probability of Taller Kinocilium")
#polygon(c(rev(newx), newx), c(rev(newdata6$UL), newdata6$LL), col="grey80", border=NA) 
lines(newdata6$DIST, newdata6$PredictedProb, lty="solid", col="black")
lines(newdata6$DIST, newdata6$UL, lty="dashed", col="black")
lines(newdata6$DIST, newdata6$LL, lty="dashed", col="black")

######################################################################
######################################################################
#Now, the relationship between apical diameter and taller kinocilia

newlm <- glm(KINOT ~ APCLD, data=lr, family="binomial")

newdata5 <- with(lr, data.frame(APCLD=seq(from=min(APCLD), to=max(APCLD), length.out=100)))
newdata6 <- cbind(newdata5, predict(newlm, newdata = newdata5, type = "link", se = TRUE))
newdata6 <- within(newdata6, {
                       PredictedProb <- plogis(fit)
                       LL <- plogis(fit - (1.96 * se.fit))
                       UL <- plogis(fit + (1.96 * se.fit))
})
plot(newdata6$APCLD, newdata6$PredictedProb, type='n', main="logit(Taller Kinocilium ~ Apical Diameter)", xlab="Apical Diameter", ylab="Predicted Probability of Taller Kinocilium")
#polygon(c(rev(newx), newx), c(rev(newdata6$UL), newdata6$LL), col="grey80", border=NA) 
lines(newdata6$APCLD, newdata6$PredictedProb, lty="solid", col="black")
lines(newdata6$APCLD, newdata6$UL, lty="dashed", col="black")
lines(newdata6$APCLD, newdata6$LL, lty="dashed", col="black")

###############################################################################
###############################################################################
#TODO: What is the relationship between Apical Diameter and Distance? (use glm)
