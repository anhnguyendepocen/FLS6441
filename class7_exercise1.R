rm(list=ls())

library(tidyverse)
#install.packages("rdrobust")
library(rdrobust)
set.seed(0205)

#1 
income <- rnorm(50000,500,50)

#2

y0 <- rnorm(50000,10,2) + income/100 + (+ income/100)^2
y1 <- y0 + 2

data <- as.data.frame(cbind(income,y0,y1))

#3
data <- data %>%
   mutate(D = ifelse(income < 500, 1,0))
 
 #4
 
 data <- data %>%
   mutate(yobs = ifelse(D == 1, y1, y0)) 
 
#5 

data %>%
  lm(yobs ~ D, data=.) %>%
  summary()

# the effect is -6.8 instead of 2, because only people with low income received treatment and the outcome is correlated with it.

#6

data %>%
  lm(yobs ~ D + income, data=.) %>%
  summary()

#now treatment effect is closer to the one we defined - it's 1.963 

#7

data %>%
  ggplot(aes(income,yobs)) +
  geom_point() + 
  theme_light()

#There's a slight break in the data in the 500 income point

#8 

data %>%
  mutate(income.bin=as.factor(ntile(income,20))) %>%
  group_by(income.bin) %>%
  summarise(yobs.avg = mean(yobs)) %>%
  ggplot(aes(income.bin,yobs.avg)) +
  geom_point() + 
  theme_light()
