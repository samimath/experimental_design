library(daewr)
mod1 <- aov( rate ~ rat + dose, data = drug )
summary(mod1)
#To interpret the differences in treatment factor levels, comparisons of means should be made. 
contrasts(drug$dose) <- contr.poly(5)
mod2 <- aov( rate ~ rat + dose, data = drug)
summary.aov(mod2,split = list(dose = list("Linear" = 1,
                                          "Quadratic" = 2,
                                          "Cubic" = 3, 
                                          "Quartic" = 4) ) )

## plot the result

R <- do.call("cbind", split(drug$rate, drug$rat))
y <- apply(R, 1, mean )
x <- as.double( levels(drug$dose) )
plot( x, y, xlab = "dose", ylab = "average lever press rate" )
xx <- seq( 0.0, 2.0, .1 )
rate.quad <- lm( y ~ poly( x, 2) )
lines(xx, predict( rate.quad, data.frame( x = xx) ))