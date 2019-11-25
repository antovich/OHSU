#Growth mixture model for comorbid disorder diagnosis status Y1-Y6
#Data restructured by age rather than test year
#Uses lcmm package


#Set working drive
setwd('H:/Projects/Y1-Y6 Comorbidity GMM for Sarah')

#Load packages
library(lcmm)
library(ggplot2)

df <- read.csv('H:/Projects/Data - General/lcmmBlogData.csv')

p1 <- ggplot(df[1:300,], aes(x, y, group=id)) + 
  geom_smooth(aes(group=id), method="loess", se=FALSE) + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", title = "One line per person, all subjects")

df$id <- as.numeric(df$id)

model1 = lcmm(y~x, mixture=~x, random=~x, subject='id', ng=2, idiag=TRUE, link="linear", data=df)

df2 <- merge(df,model1$pprob)
df2$class <- as.factor(df2$class)

p1 <- ggplot(df2, aes(x, y, group = id, colour = class)) + 
  geom_line() + 
  geom_smooth(aes(group=class), method = "loess", size = 2, se = F)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x", y = "y", colour = "Latent Class", title = "Raw")

p2 <- ggplot(df2, aes(x, y, group = id, colour = class)) + 
  geom_smooth(aes(group = id, colour = class),size = 0.5, se = F) + 
  geom_smooth(aes(group = class), method = "loess", size = 2, se = T)  + 
  scale_y_continuous(limits = c(13,37)) + 
  labs(x = "x",y = "y",colour = "Latent Class", title = "Smoothed") +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol = 2, top = "2 Latent Classes")
