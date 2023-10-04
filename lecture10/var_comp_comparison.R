## comparing formula for variance component estimation

## Question from class: Is the table provided by Nelson (1983) 
## generalized for all 3-6 stages staggered design?

## Let's test this by comparing REML method and method of moment on two sets of data 



library(lme4)
### function to calculate variance components based on estimated MS output from ANOVA table 
var_est_4_stage<-function(msA,msB,msC,msD){
  ## input variance mean square error estimate from ANOVA table
  ## formula taken from the inverse of Table 5.12 in book
  sig2D <-msD
  sig2C <-(msC - msD)*(3/4)
  sig2B <-(msB - sig2D - (7/6)*sig2C )*(2/3)
  sig2A <- (msA -(sig2D+(3/2)*sig2C+(5/2)*sig2B))/4
  sig2_est <- c(sig2A,sig2B,sig2C,sig2D)
  return(sig2_est)
}


# test 1: 
mod1<-aov(strength ~ lot + lot:box + lot:box:prep, 
          data = polymer)
s1 <- summary(mod1)
ms_est<-s1[[1]]$`Mean Sq`
sig2_est <- var_est_4_stage(msA = ms_est[1],
                                msB = ms_est[2],
                                msC = ms_est[3],
                                msD = ms_est[4])
modr1 <-lmer( strength ~ 1 + (1|lot) + (1|lot:box)+
                (1|lot:box:prep), 
              data = polymer)


## putting values together in a data frame

summary_reml<-summary(modr1)
vals_REML <-round(c(as.numeric(summary_reml$varcor[3]),
                  as.numeric(summary_reml$varcor[2]),
                  as.numeric(summary_reml$varcor[1]),
                  (summary_reml$sigma)^2),2)
vals_MoM <- round(sig2_est,2)

comp_matrix <-data.frame(MoM_est = vals_MoM, REML =vals_REML )

# test 2: removing one of the levels from the 'lot' factor - still a 4 stage design
polymer_new <- polymer[polymer$lot!=19,]
# test 1: 
mod2<-aov(strength ~ lot + lot:box + lot:box:prep, 
          data = polymer_new)
s2 <- summary(mod2)
ms_est<-s2[[1]]$`Mean Sq`
sig2_est_new <- var_est_4_stage(msA = ms_est[1],
                            msB = ms_est[2],
                            msC = ms_est[3],
                            msD = ms_est[4])
modr2 <-lmer( strength ~ 1 + (1|lot) + (1|lot:box)+
                (1|lot:box:prep), 
              data = polymer_new)
summary_reml_new<-summary(modr2)
vals_new_REML <-round(c(as.numeric(summary_reml_new$varcor[3]),
                  as.numeric(summary_reml_new$varcor[2]),
                  as.numeric(summary_reml_new$varcor[1]),
                  (summary_reml_new$sigma)^2),2)
vals_new_MoM <- round(sig2_est_new,2)

comp_matrix_new <-data.frame(MoM_est_new = vals_new_MoM, 
                             REML_new =vals_new_REML )
## putting the estimates together:
summary_final<-cbind(comp_matrix,comp_matrix_new)
print(summary_final)
