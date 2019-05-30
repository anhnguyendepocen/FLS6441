library(tidyverse)

#1 to 4 - generating fake data
data <- data.frame(age=rnorm(100,40,7)) %>%
  mutate(gender = rbinom(nrow(.),1,0.5),
         income = rnorm(nrow(.),500,50),
         education = sample(c(0,1,2,3), nrow(.), replace=TRUE, prob=c(0.25, 0.25, 0.25, 0.25)),
         y0 = rnorm(nrow(.),20,5) + (age/4) - (5*gender) + (income/50) - (3*education),
         y1 = y0 + 5,
         D = ifelse((gender*2) + (age/8) + (income/50) + (2*education) + rnorm(nrow(.),0,3) > 19, 1,0),
         yobs = ifelse(D ==1, y1, y0))

#5

data %>%
  lm(yobs ~D, data = .) %>%
  summary()
#because treatment is dependent on other variables and it's not random

#6
print(data[1,])

#only one unit had a match
match <- data %>%
  filter(D == 0 & between(age,30,45) & between(income,500,550) & gender == 1 & education %in% c(1:3)) %>%
  rbind(data[1,])

match$age[1] - match$age[2] # 8.6 y.o. difference
match$income[1] - match$income[2]# 13.11 income difference
match$education[1] - match$education[2] # 1 level of education difference 

#7 

match$yobs[1] - match$yobs[2]
#there's a difference of 10 between treated and control, twice the value we have determined. 
#this is because we usually get the average treatment effect and don't assume all units will have the same effect. 


#8 
