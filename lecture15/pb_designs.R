library(daewr)
library(FrF2)
library(BsMD)
library(reshape2)
library(ggplot2)
pb_design<-pb( nruns = 12, randomize=FALSE)
design.info(pb_design)
y <- 1:12
model_pb<-lm(y~(.)^2, data = pb_design)




## example on the fatigue life of weld-repaired castings
## load data (contained in the BSD package provided by Lawson)
data( PB12Des, package = "BsMD" )
colnames(PB12Des) <- c("c11", "c10", "c9", "c8", 
                       "G", "F", "E","D", "C", "B", "A")
## castings factors
castf <- PB12Des[c(11,10,9,8,7,6,5,4,3,2,1)]
## observations
y <- c(4.733, 4.625, 5.899, 7.0, 5.752, 5.682, 
       6.607, 5.818,5.917, 5.863, 6.058, 4.809)
## add observations to the design and 
## organize data into workable format
castf <- cbind( castf, y )


## create linear model to estimate 
## the effects of the main factors
modpb <- lm( y ~ (.), data = castf )
cfs <- coef(modpb)[2:12]
names<-names(cfs)
halfnorm(cfs, names, alpha = .35, refline=TRUE)


### from the halfnorm plot, the effects from 
### D & F seem most important, we can try to 
### fit a regression model to the data using that 

mod_DF <- lm( y ~ D+F, data = castf )


### subset regression for model selection:
library(leaps)
## select the primary factors and the observation
castfr <- castf[ , c(1:7, 12)]
## apply best subset selection algorithm
modpbr<-regsubsets(y ~ (.)^2, data=castfr,
                   method="exhaustive",nvmax=4,nbest=4)
rs <- summary(modpbr)
# visualize subset of predictors vs model R square
plot(c(rep(1:4,each=4)), rs$adjr2, 
     xlab="No. of Parameters",ylab="Adjusted R-square")
plot(modpbr,scale="r2")



################################################################################
## plotting confounding patterns (here use (.)^2 to 
## indicate the inclusion of 2-factor interaction terms):

modpb2 <- lm( y ~ (.)^2, data = subset(castf,select =-c(c8,c9,c10,c11)) )
# get design matrix
X_pb <-as.matrix(data.frame(model.matrix(modpb2)))
# calculate covariance-variance matrix
corMat<-(t(X_pb)%*%X_pb)
## visualize variance co-variance matrix 
heatmap(corMat,Colv = NA, Rowv = NA, scale="column")

## compare this with a fractional factorial design
fr_design<-FrF2( nruns = 8,  nfactors = 7)
y <-1:8
modfr <- lm( y ~ (.)^2, data = fr_design)
X_fr <-as.matrix(data.frame(model.matrix(modfr)))
corMat_fr<-(t(X_fr)%*%X_fr)
heatmap(corMat_fr,Colv = NA, Rowv = NA, scale="column")

par(mfrow=c(1,2))
heatmap(corMat,Colv = NA, Rowv = NA, scale="column")
heatmap(corMat_fr,Colv = NA, Rowv = NA, scale="column")
