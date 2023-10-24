## crate dataframe for the Lew experiment
library(daewr)
library(lme4)
library(jtools)
library(lmerTest)
## setting the variables
t1 <- rep('Control',6)
t2 <- rep('Drug1',6)
t3 <- rep('Drug2',6)
treatment<-as.factor(c(t1,t2,t3))
## experiment block
exp_levels<-c('Exp1','Exp2','Exp3','Exp4','Exp5','Exp6')
exp<-as.factor(rep(exp_levels,3))
## response variables
resp<-c(1147,1273,1216,1046,1108,1265,
        1169,1323,1276,1249,1432,1562,
        1009,1260,1143,1099,1385,1164)

lew<-data.frame(exp = exp,trt = treatment, resp = resp)

## CRD design without blocking 
mod1 <- aov( resp ~ trt, data = lew)
summary(mod1)

## CRBD design (with blocking)
mod2 <- aov( resp ~ exp + trt, data = lew)
summary(mod2)
## problem 3

## first create the dataset according to the nested structure
trailer <-NULL
for(i in 1:10){
  trailer <-c(trailer,rep(i,3))
}
sample <- rep(c(1,1,2), 10)
msmt <- rep(c(1,2,1), 10)
value <- c(47.06,44.37,49.3,
           47.43,50.35,50.42,
           48.9,48.05,50.64,
           52.32,52.26,53.47,
           46.53,45.60,53.98,
           46.99,50.87,51.87,
           47.49,51.55,58.57,
           47.41,47.63,48.63,
           48.37,51.03,50.15,
           54.8,51.57,54.52)
raw_material = data.frame(trailer = factor(trailer), 
                          sample = factor(sample), 
                          msmt = factor(msmt), 
                          value = value)
## part b nested model design implemented using aov
mod3_aov <- aov( value ~ trailer + 
                trailer:sample, data = raw_material)

summary(mod3_aov)

var_est_3_stage<-function(msA,msB,msC){
  ## input variance mean square error estimate from ANOVA table
  ## formula taken from the inverse of Table 5.12 in book
  sig2C <-msC
  sig2B <-(msB - sig2C)*(3/4)
  sig2A <- (msA - sig2C - (5/3)*sig2B)*(1/3)
  sig2_est <- data.frame(B=sig2B,A=sig2A,Residual=sig2C)
  return(sig2_est)
}

s3 <- summary(mod3_aov)
ms_est<-s3[[1]]$`Mean Sq`
sig2_est <- var_est_3_stage(msA = ms_est[1],
                            msB = ms_est[2],
                            msC = ms_est[3])

## part c using REML:

mod3_reml <-lmer(value ~ 1 + (1|trailer) 
                 + (1|trailer:sample), data = raw_material)


### part d

## create an array to represent all observations at the same level, 
## each row stands for a trailer

y<-array(raw_material$value,c(3,10))
## pooled sd at sample level
sd1 <- sqrt((y[2,]-y[1,])**2/2)
sd2 <- sqrt(2/3*(y[3,]-(y[2,]+y[1,])/2)**2)
pooled_data<-data.frame(y1=y[1,],
                        y2=y[2,],
                        y3=y[3,],
                        sd1=sd1,sd2=sd2)

## half normal plot of sd1 (sample(trailer)) and sd2 (measurement(sample))

osd1 <- sort(sd1)
r <- c( 1: length(sd1))
zscore <- qnorm( ( ( r - .5 ) / length(sd1) +1 )/ 2)
plot( zscore, osd1, 
      main = "Half-normal plot of sample(trailer) standard deviations", 
      xlab = "Half Normal Score", 
      ylab ="std. due to sample within trailer")

osd2 <- sort(sd2)
r <- c( 1: length(sd2))
zscore <- qnorm( ( ( r - .5 ) / length(sd2) +1 )/ 2)
plot( zscore, osd2, 
      main = "Half-normal plot of measurement(sample) standard deviations", 
      xlab = "Half Normal Score", 
      ylab ="std. due to measurement within sample")

## observation 5 or 9 could be problematic based on the half normal plot, what happens if we remove it?

mod3_reml_adj <-lmer(value ~ 1 + (1|trailer) 
                 + (1|trailer:sample), 
                 data = raw_material[raw_material$trailer!=c(5,9),])
summary(mod3_reml_adj)
mod3_aov_adj <- aov( value ~ trailer + 
                   trailer:sample, 
                 data = raw_material[raw_material$trailer!=c(5,9),])

s3_adj <- summary(mod3_aov_adj)
ms_est<-s3_adj[[1]]$`Mean Sq`
sig2_est_adj <- var_est_3_stage(msA = ms_est[1],
                            msB = ms_est[2],
                            msC = ms_est[3])


## part e

## estimated BLUP can be computed using the ranef() function in R

ranef_mod3<-ranef(mod3_reml)
qqnorm( ranef_mod3$trailer[[1]], 
        main="random trailer effect", 
        ylab="EBLUP",xlab ="Normal Score" )
## problem 4
## re-run golf problem using lmer
# teeheight, golfer ID and their interaction are fixed factors

mod3 <- aov(cdistance ~ teehgt + Error(id/teehgt), data = rcb)
summary(mod3)

mod4 <- lmer(cdistance ~ 1+teehgt+(1|id)+(1|id:teehgt), 
             data = rcb)
anova(mod4)

pr2 <- profile( fm1M <- lmer(cdistance ~ 1+teehgt+(1|id)+(1|id:teehgt), 
                             data = rcb))
confint (pr2) 

library(FrF2)
y <- runif(16, 0, 1)
aliases( lm( y~ (.)^4, data = design))
