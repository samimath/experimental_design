## splitplot design with two whole-plot factors
sp <- expand.grid(trayT = factor( c("RoomT", "Hot")),
                  bakeT = factor( c("low", "mid", "high") ))
## create whole-plots 
wp <- expand.grid(A = factor( c("A", "B")),
                  B = factor( c("C", "D") ))
## replicate whole-plots 
wp <- rbind(wp, wp)
## create design where there are two whole-plot factors
## and the rest are subplot factors including interaction
## blocksizes -num of subplots, num of whole-plots
splitP <- optBlock( ~ A *B* (trayT + bakeT +trayT:bakeT), 
                    withinData = sp, blocksizes = rep(6, 8),
                    wholeBlockData = wp)
