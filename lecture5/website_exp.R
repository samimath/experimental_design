# example of multi-factor experiment
library(daewr)
library(effects)
##
modb <- glm( cbind(signup, visitors-signup) ~ A * B * C * D,data = web, family = binomial)
mod_effects1<-anova(update(modb, .~ A+B + A:B + C + 
               A:C + B:C + A:B:C + 
               D + A:D+B:D +C:D+ A:B:C:D ),test = "Chisq")

## 3-way interaction plots
modbA_1 <- glm( cbind(signup, visitors-signup) ~ C*D ,data = subset(webp, A == 1), family = binomial)
plot(effects::allEffects(modbA_1), main = 'background =1', xlab = 'text color', ylab = 'P(sign-up)')

modbA_2 <- glm( cbind(signup, visitors-signup) ~ C*D ,data = subset(webp, A == 2), family = binomial)
plot(effects::allEffects(modbA_2), main = 'background =1', xlab = 'text color', ylab = 'P(sign-up)')


modbA_3 <- glm( cbind(signup, visitors-signup) ~ C*D ,data = subset(webp, A == 3), family = binomial)
plot(effects::allEffects(modbA_3), main = 'background =1', xlab = 'text color', ylab = 'P(sign-up)')


