library(FrF2)
library(AlgDesign)
library(daewr)
library(lme4)
## create design with 32 subplot, 8 whole-plots and 2 wholeplot factors
design <-FrF2(32, 4, WPs = 8, nfac.WP = 2, factor.names = (c("A", "B","C", "D")))

## implementing the model
rmod2 <- lmer( ys ~ A*B*C*D  +(1|Block) + (1|A:B:Block), data = sausage)
## ANOVA:
anova(rmod2)
## effect summary
summary(rmod2)

## interaction plot to understand the 3-way interaction between factors A,B and C

with(sausage[sausage$C==1,], interaction.plot(x.factor = A, 
                                 trace.factor = B, 
                                 response = ys,main = 'C=High Level'))
with(sausage[sausage$C==-1,], interaction.plot(x.factor = A, 
                                              trace.factor = B, 
                                              response = ys,main = 'C=Low Level'))