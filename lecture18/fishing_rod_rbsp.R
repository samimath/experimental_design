## example of RBSP design
library(AlgDesign)
# define subplot factors
sp <- expand.grid(PoleStiff = factor(c("light","medLight")), 
                  LureWgt = factor(c("light", "heavy")))
# define whole plot factor
wp <- data.frame(LineWgt = factor(c("6lb", "10lb","20lb")))
## replicate whole-plots 
wp <- rbind(wp, wp)
splitP <- optBlock( ~ LineWgt*(PoleStiff + LureWgt +
                                 PoleStiff:LureWgt), 
                    withinData = sp, blocksizes =rep(4, 6), 
                    wholeBlockData = wp)
fisherman <- factor( c(rep(1:2, each = 12)))
splitP$design <- cbind(fisherman, splitP$design)

