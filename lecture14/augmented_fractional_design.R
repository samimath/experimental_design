## augmentation by optimal design

library(FrF2)
library(AlgDesign)
des2 <- FrF2( 8, 7, generators = c("AB", "AC", "BC", "ABC" ),randomize=FALSE)
augm <- fold.design(des2)
## converting factors into numeric values
A <- (as.numeric( augm$A) - 1.5 ) / .5
B <- (as.numeric( augm$B) - 1.5 ) / .5
C <- (as.numeric( augm$C) - 1.5 ) / .5
D <- (as.numeric( augm$D) - 1.5 ) / .5
E <- (as.numeric( augm$E) - 1.5 ) / .5
F <- (as.numeric( augm$F) - 1.5 ) / .5
G <- (as.numeric( augm$G) - 1.5 ) / .5
## Block - tells us which sequence of the runs belong to the original design vs mirror design
Block <- augm$fold
augmn <- data.frame(A, B ,C, D, E, F, G, Block)


cand <- gen.factorial( levels = 2, nVar = 7, varNames = c("A","B", "C", "D", "E", "F", "G"))
Block <- rep('cand', 128)
cand <- data.frame( A=cand$A, B=cand$B, C=cand$C, D=cand$D,
                      + E=cand$E, F=cand$F, G=cand$G, Block)
all <- rbind( augmn, cand)

fr<-1:16
optim <- optFederov( ~ A + B + F + I(A*D) + I(C*F), data = all,
                       + nTrials = 24, criterion = "D", nRepeats = 10, augment = TRUE,
                       + rows=fr)
newruns <- optim$design[ 17:24, ]
newruns