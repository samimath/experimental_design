## Analysis of coffee experiment
library(daewr)
library(lme4)
library(effects)

## This is a simple 2^2 design
data<-read.csv('Coffee_results.csv',header = TRUE)
## convert the variables into factor

data$Temp<-as.factor(data$Temp)
data$Grind<-as.factor(data$Grind)

## orthogonal encoding for the factor levels

#data$Temp <-ifelse(data$Temp == 80, -1, 1)
#data$Grind <-ifelse(data$Grind == 4, -1, 1)

mod <- lm( PH ~ Temp*Grind, data = data,x = TRUE )
summary(mod)

## anova(mod)

interaction.plot(data$Temp, data$Grind, data$PH, type = "l",
                 legend=FALSE, ylim = c(4,6), main = "Grind setting = 6",
                 xlab = "Water Temp", ylab = "pH of coffee")

## temperature doesn't have much of an effect here, 
## however grind size does seem to have an influence 

