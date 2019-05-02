rm(list=ls())

library(tidyverse)
#install.packages("rdrobust")
library(rdrobust)
#install.packages("rddensity")
library(rddensity)
set.seed(0205)

#1 
income <- rnorm(50000,500,50)

#2

y0 <- rnorm(50000,10,2) + income/100 + (income/100)^2
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

#9

rdplot(data$yobs,data$income,c=500)

#10

data %>%
  filter(income >= 480 & income <= 520) %>%
  t.test(yobs~D,data=.)

#the difference is statistically significant, but way lower than the effect of 2

#11
data %>%
  filter(income >= 480 & income <= 520) %>%
  lm(yobs ~ D + income, data=.) %>%
  summary()

#12

rdrobust(data$yobs,data$income,c=500,p=1)
#the optimal bandwidth is the 'BW est. (h)' row
summary(rdrobust(data$yobs,data$income,c=500,p=1))
#summary on the above gives the RD with robust s.e. 

#13
rdrobust(data$yobs,data$income,c=500,p=2)
summary(rdrobust(data$yobs,data$income,c=500,p=2))

#we get higher se.s with the quadratic term, but the point estimate is the same

#14
?rddensity

denso <- rddensity(data$income,c=500,p=2)
rdplotdensity(denso, data$income)

denso2 <- rddensity(data$income,c=500,p=1)
rdplotdensity(denso2, data$income)

#15


income <- rnorm(50000,500,50)
y0 <- rnorm(50000,10,2) + income/100 + (income/100)^2
y1 <- y0 + 2

data2 <- as.data.frame(cbind(income,y0,y1)) %>%
  mutate(D = ifelse(income < 500, 1,0)) %>%
  mutate(y1 = ifelse(income >= 490 & income <= 510, y0 + 10, y0 +3))%>%
  mutate(yobs = ifelse(D == 1, y1, y0)) 
       