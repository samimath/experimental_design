library(lme4)
library(ggplot2)
library(dplyr)
data<-read.csv('lecture12/math420_inclass_exercise.csv')

## some basic data cleaning:
data$plane.type <- as.factor(toupper(data$plane.type))
data$group <- as.factor(data$group)
data$plane.maker <- as.factor(data$plane.maker)
data$flight.tester <- as.factor(data$flight.tester)
data$trial <- as.factor(data$trial)


## Let's look at some EDA

hist(data$distance.y, breaks = 10, main = 'how far did they fly?')
hist(data$distance.x, breaks = 10,  main = 'how straight did they fly?')

## do they look different based on the group?

ggplot(data)+geom_boxplot(aes(x=group,y=distance.y, fill= group))+ggtitle('flight distance across groups')
ggplot(data)+geom_boxplot(aes(x=group,y=distance.x, fill= group))+ggtitle('flight distance across groups (x)')

## are there maker bias?
ggplot(data)+geom_boxplot(aes(x=plane.maker,y=distance.y, fill= plane.maker))+
  ggtitle('flight distance across makers')
ggplot(data)+geom_boxplot(aes(x=plane.maker,y=distance.x, fill= plane.maker))+
  ggtitle('flight distance (x) across makers')
## run the model with only flight tester (nested) as the random effect, and plane type as fixed effect 

mod1 <- lmer(distance.y~1+plane.type+(1|group:flight.tester) , 
             data = data )
summary(mod1)

## run the model with plane maker as a blocking effect 
mod2 <- lmer(distance.y~1+plane.type+(1|group)+
               (1|group:flight.tester)+
               (1|group:flight.tester:trial) , 
             data = data )
summary(mod2)



mod2b <- lmer(distance.x~1+plane.type+(1|group)+
               (1|group:flight.tester)+
               (1|group:flight.tester:trial) , 
             data = data )
summary(mod2b)


mod3 <- lmer(distance.y~1+plane.type+(1|plane.maker)+(1|group), 
             data = data )
summary(mod3)

mod4 <- lmer(distance.y~1+plane.type+(1|group)+(1|group:flight.tester), 
             data = data )