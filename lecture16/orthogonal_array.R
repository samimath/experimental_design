library("daewr")
library('FrF2')
library("DoE.base")
show.oas( factors = list(nlevels = c(3,2), 
                         number = c(5,2)))

des <- oa.design(nlevels = c(3, 3, 3, 3, 3, 2, 2),
                 nruns = 36,
                 columns = "min3", 
                 randomize = TRUE, 
                 seed = 104)
mod_test<-lm(y~(.)^2,data =des)
X <-as.matrix(data.frame(model.matrix(mod_test)))
XtX <-round(t(X)%*%X,2)