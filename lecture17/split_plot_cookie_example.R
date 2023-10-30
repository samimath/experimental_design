## example of implementing a split-plot design
library(AlgDesign)
## create subplots
sp <- expand.grid(trayT = factor( c("RoomT", "Hot")),
                  bakeT = factor( c("low", "mid", "high") ))
## create whole-plots 
wp <- data.frame(short = factor( c("100%", "80%") ))
## combine whole-plots and subplots
wp <- rbind(wp, wp)
## create design where shortening is the whole-plot factor 
## and the rest are subplot factors including interaction
## blocksizes -> num of subplots, num of whole-plots
splitP <- optBlock( ~ short * (trayT + bakeT +trayT:bakeT), 
                    withinData = sp, blocksizes = rep(6, 4),
                    wholeBlockData = wp)