library(daewr)
library(FrF2)
library(BsMD)
library(reshape2)
library(ggplot2)
################################################################################
## plotting confounding patterns (here use (.)^2 to 
## indicate the inclusion of 2-factor interaction terms):
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
modpb2 <- lm( y ~ (.)^2, data = subset(castf,select =-c(c8,c9,c10,c11)) )
# get design matrix
X_pb <-as.matrix(data.frame(model.matrix(modpb2)))
# calculate covariance-variance matrix
corMat<-(t(X_pb)%*%X_pb)
## visualize variance co-variance matrix 
heatmap(corMat,Colv = NA, Rowv = NA, 
        scale="column",cexRow=.5)

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


## compare this with a full factorial design
full_design<-FrF2( nruns = 2^7,  nfactors = 7)
y <-1:(2^7)
modfull <- lm( y ~ (.)^2, data = full_design)
X_full <-as.matrix(data.frame(model.matrix(modfull)))
corMat_full<-(t(X_full)%*%X_full)
heatmap(corMat_full,Colv = NA, Rowv = NA, scale="column")

par(mfrow=c(1,3))
heatmap(corMat,Colv = NA, Rowv = NA, scale="column")
heatmap(corMat_fr,Colv = NA, Rowv = NA, scale="column")
heatmap(corMat_full,Colv = NA, Rowv = NA, scale="column")