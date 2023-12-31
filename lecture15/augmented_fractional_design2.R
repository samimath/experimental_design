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

## creating a dataframe with a different format
des_fold2 <- data.frame(des_fold)[,c('fold','A','B','C','D','E','F','G')]
# add observation vector to the data frame
des_fold2[,'y']<-y
## reorganize the data frame and create a new columns called 'block' based on the values from 'fold, 
## here the function 'ifelse()' is used to create the new column, where des_fold2$fold=='original' is the logic, 
## and 1 is the value when the logic is true,  2 is the value when the logic is false
des_fold2[,'block']<-ifelse(des_fold2$fold=='original',1,2)
## we can also create a new column called 'run' to contain the run numbers 
des_fold2[,'run'] <- 1:16
## finalize the data frame to select only the needed columns
des_fold2 <- des_fold2[,c('run','block','A','B','C','D','E','F','G','y')]




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


temp_data <- make_temp_data(data= des_fold2,factors=c('A','B','C','D','E','F','G'))
comb5<-combn(x = c('A','B','C','D','E','F','G'), m = 5)
comb4<-combn(x = c('A','B','C','D','E','F','G'), m = 4)
comb3<-combn(x = c('A','B','C','D','E','F','G'), m = 3)
comb2<-combn(x = c('A','B','C','D','E','F','G'), m = 2)
for(i in 1:ncol(comb5)){
  check_vec <- apply(temp_data[,comb5[,i]],1,prod)
  if(sum(check_vec)==length(check_vec)){
    print(comb5[,i])
  }
}


comb_vec <-combn(c('A','B','C','D','E'),4)

for(i in 1:ncol(comb_vec)){
  check_vec <- apply(des3temp[,comb_vec[,i]],1,prod)
  if(sum(check_vec)==length(check_vec)){print(comb_vec[,i])}
  }