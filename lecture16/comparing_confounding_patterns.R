library(daewr)
library(FrF2)
library(BsMD)
library(reshape2)
library(ggplot2)
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
