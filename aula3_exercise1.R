library(tidyverse)

set.seed(0404)

# 1

data <- as.data.frame(rbinom(1000, 1, 0.5))

# 2

data <- data %>%
  rename(gender = `rbinom(1000, 1, 0.5)`) %>%
  mutate(y0 = rnorm(1000, mean=5, sd=1))

# 3

data <- data %>%
  mutate(y0 = ifelse(gender == 1, (y0 + 1), y0))

# 4 

data <- data %>%
  mutate(c = 2,
         y1 = y0 + c)

# 5

data <- data %>%
  mutate(D = rbinom(1000, 1, 0.5))

# 6

cor.test(data$gender,data$D)

# 7

data <- data %>%
  mutate(yObs = ifelse(D == 1, y1, y0))

# 8

t.test(data$gender,data$D)

# 9 

t.test(data$y0,data$y1) #it's 2 as we created

# 10

t.test(data$yObs,data$D) #No, the difference is a lot higher

# 11

reg1 <- lm(yObs ~ D, data = data)
summary(reg1)

# 12

reg2 <- lm(yObs ~ D + gender, data = data)
summary(reg2)

# 13
# a

data <- data %>%
  mutate(bins = as.factor(ntile(y0, q=20)))

# b
clusters <- data%>%
  select(bins) %>%
  distinct() %>%
  mutate(D_cluster = rbinom(20, 1, 0.5))

data <- data %>%
  left_join(clusters)

# c

data <- data %>%
  mutate(yObs2 = ifelse(D_cluster == 1, y1, y0))

reg3 <- lm(yObs2 ~ D_cluster, data = data)
summary(reg3)

# d 

install.packages("estimatr")
library(estimatr)

?estimatr

reg3_clus <- lm_robust(yObs2 ~ D_cluster, data = data, clusters = bins)

summary(reg3)
summary(reg3_clus)

# 14 
install.packages("dotwhisker")
library(dotwhisker)
library(broom)
library(dplyr)


results<-vector("list", 100) 
datasims <- data.frame()
for (i in 1:100){
  data <- data %>%
    mutate(D = rbinom(1000, 1, 0.5),
           yObs = ifelse(D == 1, y1, y0))
  tidyy <- tidy(lm(yObs ~ D, data = data)) %>%
    mutate(model = i,
           min=estimate-1.96*std.error,
           max=estimate+1.96*std.error,
           ci_test = ifelse(max <2 | min > 2, "off ATE", "got ATE"))
  datasims <- rbind(datasims,tidyy) %>%
    arrange(desc(max))
  rm(tiddy)
  
}

datasims %>%
  filter(term == "D") %>%
  group_by(ci_test) %>%
  summarise(n = n())

secret_weapon(datasims, "D", vline = geom_vline(xintercept = 2, colour = "grey60", linetype = 2)) +
  theme_bw()


