## nested design example 

## two ways to get estimates of variance components

### 1. through ANOVA
mod2 <- aov( elasticity ~ supplier + supplier:batch +
               + supplier:batch:sample, data = rubber)
s2<- summary(mod2)
### 2. using REML estimates
mod3 <- lmer( elasticity ~ 1+ (1|supplier) +(1|supplier:batch) + 
                 (1|supplier:batch:sample), data = rubber)
s3<- summary(mod3)

## summing up the variance component to get total variance
sigmaT<- s3$varcor$`supplier:batch:sample`[1]+ 
s3$varcor$`supplier:batch`[1]+
s3$varcor$supplier[1]+
(s3$sigma)^2




