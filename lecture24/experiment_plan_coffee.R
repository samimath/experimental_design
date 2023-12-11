## 1. creating a 2-factor design plan
# creating grid for levels of different factors:

D <- expand.grid( Temp = c(80,100), Grind = c(4,6) )
D

## two ways to create replicates:

## method 1: create 2 reps per combination of level
D1<-rbind(D,D,D)

set.seed(2023)
D2<-D1[order(sample(1:nrow(D1))),]

## 3. save the plan to your current directory
setwd(dir = './lecture24/')
CoffeeDes <-D2[c('Temp','Grind')]
#write.csv(CoffeeDes,'Coffee.csv',row.names = TRUE)


