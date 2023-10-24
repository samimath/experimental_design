library(FrF2)
library(DoE.base)
library(AlgDesign)
## example from water filtration study

### original design
des_test <- FrF2( nruns = 8,  
                  nfactors = 7, 
                  generator = c('AB','AC','BC','ABC'))
y <- runif( 8, 0, 1 )
aliases(lm(y~(.)^3, data = des_test))
## foldover design
des_fold<- fold.design(des_test, columns = 'full')
des_fold
y<-c(69.95,58.65,56.25,53.25,
        94.40,73.45,10.00,2.11,
        16.20,52.85,9.05,31.10,
        7.40,9.90,10.85,48.75)
add.response(des_fold, y)
model_fold<-lm(y~(.)^2, data = des_fold)
## get design matrix
model_matrix_full<-data.frame(model.matrix(model_fold))
## gathering columns from the model that appear as significant (according to the book)
X<-cbind(model_matrix_full$X.Intercept,# overall effect
         model_matrix_full$fold1,# block effect
         model_matrix_full$A1,# factor A
         model_matrix_full$B1,# factor B
         model_matrix_full$F1, # factor F 
         model_matrix_full$A1.D1, # factor AD
         model_matrix_full$C1.F1) # factor CF which is confounded with AD

## test the singularity of the matrix -- should result it in error message  
solve(t(X)%*%X)


## example to implement A/D optimal designs
## 2^7-4 design from the water filtration study
augm <- fold.design(des_test)
## converting each factor from 'levels' into numeric values
A <- (as.numeric( augm$A) - 1.5 ) / .5
B <- (as.numeric( augm$B) - 1.5 ) / .5
C <- (as.numeric( augm$C) - 1.5 ) / .5
D <- (as.numeric( augm$D) - 1.5 ) / .5
E <- (as.numeric( augm$E) - 1.5 ) / .5
F <- (as.numeric( augm$F) - 1.5 ) / .5
G <- (as.numeric( augm$G) - 1.5 ) / .5
## Block - tells us which sequence of the runs belong to the original design 
## vs mirror design. 
Block <- augm$fold
## consolidate the factors (including block) into a data frame
augmn <- data.frame(A, B ,C, D, E, F, G, Block)


## generate all candidate designs --> this should be 2^7 = 128
cand <- gen.factorial( levels = 2, 
                       nVar = 7, 
                       varNames = c("A","B", "C", "D", "E", "F", "G"))
## separately design the block variable
Block <- rep('cand', 128)

## putting the candidate design and the blocking variables in one data frame
cand <- data.frame( A=cand$A, B=cand$B, C=cand$C, D=cand$D,E=cand$E, F=cand$F, G=cand$G, Block)
all <- rbind( augmn, cand)

## utilize the Federov optimization procedure
## tell it how many runs (observations) there are:
fr<-1:16
## provide input argument for the optimization function
## start with a formula 
optim <- optFederov( frml = ~ A + B + F + I(A*D) + I(C*F), ## model using variable names from data
                     data = all, ## matrix describing the variables (in this case candidate designs)
                     nTrials = 24, ## num of total trials in the experiment
                     criterion = "D", ## criterion for the design matrix in the result (e.g. D-optimal)
                     nRepeats = 10, ## num of times the optimization process is to be repeated
                     augment = TRUE,
                     rows=fr ## rows of design to be augmented
                     )
newruns <- optim$design[ 17:24, ]
newruns

