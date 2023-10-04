## fractional design generator
library(FrF2)
## define a 5-factor half factorial design in standard order:
design <- FrF2(nruns = 16, nfactors = 5, 
               generators = 'ABCD',randomize = FALSE)
print(design)
## generate alias / confounding patterns
## place holder response vector of the same dimension as the runs 
## (it can be anything)
y<-1:16
aliases( lm (y~(.)^4, 
             data = design))

