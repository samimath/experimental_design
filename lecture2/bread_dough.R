set.seed(2023)
# list of factor levels by number of reps
f <-factor(rep(c(35,40,45), each = 4))
# randomly assign bread dough to treatment levels
fac <- sample(f, 12)
#experimental units
eu <- 1:12
plan <- data.frame(loaf = eu, time = fac)
write.csv(plan, file  = './data/Plan.csv', row.names = FALSE)
