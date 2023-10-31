## example of implementing a split-plot design
library(daewr)
library(AlgDesign)
library(GAD)
library(ggplot2)
library(VCA) ## package to check whether a design is balanced or not 

## create subplots
sp <- expand.grid(trayT = factor( c("RoomT", "Hot")),
                  bakeT = factor( c("low", "mid", "high") ))
## create whole-plots 
wp <- data.frame(short = factor( c("100%", "80%") ))
## combine whole-plots and subplots
wp <- rbind(wp, wp)
## create design where shortening is the whole-plot factor 
## and the rest are subplot factors including interaction
## blocksizes -num of subplots, num of whole-plots
splitP <- optBlock( ~ short * (trayT + bakeT +trayT:bakeT), 
                    withinData = sp, blocksizes = rep(6, 4),
                    wholeBlockData = wp)



### analyzing the experiment 
## load data
data(splitPdes)
## note -- the 'batch' variable in the data is not 
## consistent with the 'batch' variable in the design for some reason, 
## I decided to follow the book example, I suspect the batch was changed to allow for a balanced design
## If we were to fix the batch label, this is a command we can use : 
## splitPdes$batch <-floor(as.numeric(rownames(splitPdes)))
## visualize the data

ggplot(splitPdes) +geom_boxplot(aes(x=as.factor(short), y =y, 
                                    group = batch, col = as.factor(batch)))+
  facet_grid(bakeT~trayT) + theme_bw()
## assign fixed and random factors for modeling 
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
TrayT <- as.fixed(splitPdes$trayT)

## define model - method 1 using aov
model <- aov(y ~ Short + Short%in%Batch + ## whole-plot factor
               BakeT +TrayT + ## subplot factors
               Short*BakeT + Short*TrayT + ## interaction terms
               BakeT*TrayT +Short*BakeT*TrayT, 
             data = splitPdes)

## anova 
anova(model)
## gad
gad(model)

## define model - method 2 using lmer
rmodel <- lmer(y ~ 1 + short + ## overall effect + whole-plot factor
                 bakeT + trayT + ## subplot factors
                 short:bakeT +short:trayT +   ## interaction terms
                 bakeT:trayT + short:bakeT:trayT +
                 (1|short:batch), ## random effect from whole-plot
               data = splitPdes)
## inspect effects:
anova(rmodel)

## returning variance component

## effects due to shortening (A), baking temp (B) and tray temp (C), 
## as well as shortening + tray temp interaction all appear as significant
## interaction plot 
with(splitPdes, interaction.plot(x.factor = short, 
                            trace.factor = trayT, 
                            response = y))


## additional analysis: what if we were to analyze this as if it's a CRD 
## with only factors trayT and bakeT considered?

model1<-aov(y ~BakeT +TrayT + ## subplot factors
              BakeT*TrayT , ## interaction terms
            data = splitPdes)
anova(model1)

model2<-aov(y ~short,  ## whole-plot factors
            data = splitPdes)
anova(model2)


