library('ggplot2')
library('dplyr')
tomato <-read.csv('data/tomato_yield.csv',header = T)
# plot yield by fertilizer treatment
ggplot(data = tomato)+
  geom_point(aes(x = row,y=yield,col=treatment),size=4)+
  theme_light()
# summarize mean
tomato%>%group_by(treatment)%>%
  summarise(avg = mean(yield),std = sqrt(var(yield)),count = n())
