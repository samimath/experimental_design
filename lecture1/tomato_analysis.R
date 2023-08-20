library('ggplot2')
library('dplyr')
tomato <-read.csv('data/tomato_yield.csv',header = T)
# plot yield by fertilizer treatment
ggplot(data = tomato)+
  geom_point(aes(x = row,y=yield,col=treatment),size=4)+
  theme_light()
# boxplot
ggplot(data = tomato)+
  geom_boxplot(aes(x = row,y=yield,fill=treatment),size=1)+
  theme_light()

# summarize mean
tomato%>%group_by(treatment)%>%
  summarise(avg = mean(yield),std = sd(yield),count = n())
# separate data from each group
a <- tomato$yield[tomato$treatment=='A']
b <- tomato$yield[tomato$treatment=='B']
# simple two-sample t-test:
t.val<-t.test(yield ~ treatment, data = tomato,var.equal=TRUE)

# verify 
Na<- length(a)
Nb <-length(b)
# degrees of freedom
df<- Na+Nb-2
# sample standard dev
Sa <- sum((a-mean(a))^2)/(Na-1)
Sb <- sum((b-mean(b))^2)/(Nb-1)
# pooled standard dev (when we assume equal variance)
sp <- sqrt(((Na-1)*Sa + (Nb-1)*Sb)/df)
# putting it together
t.val2 <- (mean(a)-mean(b))/(sp*(sqrt(1/Na+1/Nb)))

print(t.val)
print(t.val$statistic - t.val2)

