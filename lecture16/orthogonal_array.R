library("daewr")
library('FrF2')
library("DoE.base")
library('AlgDesign')
show.oas( factors = list(nlevels = c(3,2), 
                         number = c(5,2)))

## smallest available array for 5 factors with 3 levels , 
## and 2 factors with 2 levels
## min3 here indicates the requirement to return a design 
## with the least amount of confounding between 
## main effects and 2-factor interactions 
des <- oa.design(nlevels = c(3, 3, 3, 3, 3, 2, 2),
                 nruns = 36,
                 columns = "min3", 
                 randomize = TRUE, 
                 seed = 104)
y<-1:36
mod_test<-lm(y~(.),data =des)
## check the confounding patterns:
X <-as.matrix(data.frame(model.matrix(mod_test)))
XtX <-round(t(X)%*%X,2)
heatmap(XtX,Colv = NA, Rowv = NA, scale="column")


cand <- oa.design( nlevels = c(3, 3, 3, 3, 3, 2, 2),
                   nruns = 36, columns = "min3", seed = 104)
optim <- optFederov( ~ A + B + C + D + E + F + G, 
                     cand,nRepeats = 10, nTrials = 18, criterion = "D")
y <-1:18
mod_test<-lm(y~A + B + C + D + E + F + G,data =optim$design)
## check the confounding patterns:
X <-as.matrix(data.frame(model.matrix(mod_test)))
XtX <-round(t(X)%*%X,0)

## product research example

cand_chair <- oa.design(nlevels = c(4, 4, 3, 2),
                        randomize = FALSE, 
                        columns = 'min3',seed = 2013)

optim_chair <- optFederov( ~ A + B + C + D, cand_chair,
                           nRepeats = 10, nTrials = 12, 
                           criterion = "D", aug = FALSE)

## analyze the outcome of the experiment
data(hardwood)
modh <- lm(Rating ~ as.factor(Price) + Density + Guarantee + Design,
           data = hardwood)
anova(modh)


modh2 <- lm(Rating ~ Price + Density + Guarantee + Design,
           data = hardwood)
summary(modh2)

