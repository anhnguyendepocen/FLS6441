library(tidyverse)
set.seed(2504)

#1 and #2


income <- rnorm(10000,500,50)
y0 <- rnorm(10000,10,2) + income/100
y1 <- y0 + 2

data <- cbind(income,y0,y1)

#3

tA <- rbinom(10000,1,0.5)
data <- data%>%
  cbind(tA)

#4

data <- as.data.frame(data) %>%
  mutate(t = tA)

#5

data <- data %>%
  mutate(t = ifelse(income > 550, 0, t)) 

#6
data <- data %>%
  mutate(yobs = ifelse(t == 1, y1, y0)) 

#7
reg1 <- lm(yobs ~t, data=data)
summary(reg1)
#treatment effect is lower than 2, it's only 1,75

#8
data <- data %>%
  mutate(t = ifelse(income < 450, 1, t))  %>%
  mutate(yobs = ifelse(t == 1, y1, y0)) 

#9      

reg2 <- lm(yobs ~t, data=data)
summary(reg2)
#treatment effect now is even lower - 1,49

#10

first.stage <- lm(t ~ tA, data=data)
summary(first.stage)


#11

data <- data %>%
  mutate(fitted.1stage = first.stage[["fitted.values"]])

#12

sec.stage <- lm(yobs ~ fitted.1stage, data= data) 
summary(sec.stage)

# 13

install.packages("AER")
library(AER)

iv1 = ivreg(yobs ~ t | tA , data = data)
summary(iv1)

#the SEs are slightly smaller

#15

data <- data %>%
  mutate(t = ifelse(income < 480 & tA == 0, 1,
                    ifelse(income < 480 & tA == 1, 0,t)))  %>%
  mutate(yobs = ifelse(t == 1, y1, y0))

iv2 = ivreg(yobs ~ t | tA , data = data)
summary(iv2)
 #the treatment effect gets higher


#######

test <- lm(yobs ~ t*tA, data=data)
summary(test)

1.24838 - 0.38629 + 1.00256
