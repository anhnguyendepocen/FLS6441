library(tidyverse)
install.packages("estimatr")
library(estimatr)


#questions 1 to 3

data <- data.frame(age = rnorm(10000,40,7)) %>%
  mutate(gender = rbinom(nrow(.),1,0.5),
         y0 = rnorm(nrow(.),10,2) + (age/5) - 12*gender,
         y1 = y0 + 5,
         D = ifelse(((gender*20)+age) > 50,1,0),
         yobs = ifelse(D==1,y1,y0))

#5 

data %>%
  lm(yobs ~D, data=.)%>%
  summary()
#The results are the opposite of the expected: there's an effect of -4.4 instead of +5

#6 
#there are cofounders in the treatment effect

#7 (caderno)

#8 
# The back-doors are:
# D <- gender -> Y 
# D <- age -> Y
# we need to control for both gender and age

#9

data %>%
  lm(yobs ~D + gender + age, data=.)%>%
  summary()
# now the effect is within we expected, 4.97. 

# 10 

data <- data %>%
  mutate(charity = rnorm(nrow(.),0,2) + 2*yobs)

# 11 (caderno)

# 12

data %>%
  lm(charity ~ D + age + gender + yobs, data=.) %>%
  summary()

#the problem is that yobs is the only link for D, so when we controll for it we remove all the variation responsible for the treatment effect

# 13

data %>%
  lm(charity ~ D + age + gender, data=.) %>%
  summary()

#the effect of D now is correctly estimated around 10

# 14

data <- data %>%
  mutate(education = rnorm(nrow(.),10,1),
         religion = ifelse((age*2) + (education*2) + rnorm(nrow(.),3,1) >100, 1,0),
         y0 = rnorm(nrow(.),10,2) + (age/5) - (3*gender) + (5*education),
         y1 = y0 + 5,
         D = ifelse(((gender*20)+age) > 50,1,0),
         yobs = ifelse(D==1,y1,y0))

#15 back-doors:
# D <- gender -> Y
# D <- age -> religion <- education -> Y 

#16 
# gender and age

#17 

data %>%
  lm(yobs ~ D + age + gender + religion, data=.) %>%
  summary()

#religion is a collider 

#18 

data %>%
  lm(yobs ~ D + age + gender, data=.) %>%
  summary()

         
