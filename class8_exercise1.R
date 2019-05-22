library(tidyverse)
install.packages("estimatr")
library(estimatr)


#questions 1 to 5

data <- data.frame(oil = rbinom(5570,1,0.5)) %>%
  mutate(municipality = row_number()) %>%
  slice(rep(1:n(), each = 3)) %>%
  cbind(data.frame(t=c(0,1,2))) %>%
  mutate(y0 = rnorm(nrow(.),60,5) - 2*t - 3*oil,
         y1 = y0 + 5,
         D = ifelse(oil==1,1,0),
         yobs = ifelse((D==1 & t==2),y1,y0))


#6
summary(lm(yobs ~ D, data=data))
#we have ommited variable issue


#7
data %>%
  filter(t %in% c(1,2)) %>%
  lm(yobs ~ t, data=.)%>%
  summary()
#there's a time trend that affects Yobs

#8
data %>%
  filter(t %in% c(1,2)) %>%
  lm(yobs ~ D + t + D*t, data=.)%>%
  summary()

#now we get closer to our treatment assumption - there's a positive effect of 5 for treated units

#9
data %>%
  filter(t %in% c(1,2)) %>%
  lm_robust(yobs ~ D + t + D*t, data = ., clusters = municipality) %>%
  summary()

#there's smallers std. error. 

#10

data %>%
  filter(t %in% c(1,2)) %>%
  group_by(D,t)%>%
  summarise(mean = mean(yobs))

difD1 <- 58.0 - 55.0 
difD0 <- 56.1 - 58.2

difindif <- difD1-difD0
difindif

#11
data %>%
  filter(t %in% c(0,1)) %>%
  lm_robust(yobs ~ D + t + D*t, data = ., clusters = municipality) %>%
  summary()

#12
data%>%
  group_by(D,t)%>%
  summarise(meanyobs = mean(yobs))%>%
  ggplot(., aes(x=t, y=meanyobs, colour=as.factor(D))) +
  geom_line()+
  theme_minimal()


#13

data2 <- data.frame(oil = rbinom(5570,1,0.5)) %>%
  mutate(municipality = row_number()) %>%
  slice(rep(1:n(), each = 3)) %>%
  cbind(data.frame(t=c(0,1,2))) %>%
  mutate(y0 = rnorm(nrow(.),60,5) - 2*t*oil - 3*oil,
         y1 = y0 + 5,
         D = ifelse(oil==1,1,0),
         yobs = ifelse((D==1 & t==2),y1,y0))

#14
data2%>%
  group_by(D,t)%>%
  summarise(meanyobs = mean(yobs))%>%
  ggplot(., aes(x=t, y=meanyobs, colour=as.factor(D))) +
  geom_line()+
  theme_minimal()

#checking effect - around 3, not 5
data2 %>%
  filter(t %in% c(1,2)) %>%
  lm_robust(yobs ~ D + t + D*t, data = ., clusters = municipality) %>%
  summary()

#checking parallels - there're no parallels after treatment (interaction term is significant)
data2 %>%
  filter(t %in% c(0,1)) %>%
  lm_robust(yobs ~ D + t + D*t, data = ., clusters = municipality) %>%
  summary()

#means dif in dif - the effect is 3, not 5
data2 %>%
  filter(t %in% c(1,2)) %>%
  group_by(D,t)%>%
  summarise(mean = mean(yobs))

difD1.2 <- 58.1 - 55.0 
difD0.2 <- 60.0 - 59.9

difindif2 <- difD1.2-difD0.2
difindif2

