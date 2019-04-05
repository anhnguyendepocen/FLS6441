library(tidyverse)
set.seed(2803)

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

plot(density(data$y0))
lines(density(data$y1))

# 6

data <- data %>%
  mutate(latent = 0.5*gender + runif(1000, min = 0, max = 1),
         D = ifelse(latent > 0.75, 1, 0))

# 7

chisq.test(data$gender, data$D)

# 8 

mean(data$y1 - data$y0)

# 9

data <- data %>%
  mutate(yObs = ifelse(D == 1, y1, y0))

# 10

reg1 <- lm(yObs ~ D, data = data)
summary(reg1)

# 11

data2 <- as.data.frame(rbinom(1000, 1, 0.5)) %>%
  rename(gender = `rbinom(1000, 1, 0.5)`) %>%
  mutate(y0 = rnorm(1000, mean=5, sd=1)) %>%
  mutate(y0 = ifelse(gender == 1, (y0 + 1), y0)) %>%
  mutate(c = 0,
         y1 = y0 + c) %>%
  mutate(latent = 0.5*gender + runif(1000, min = 0, max = 1),
         D = ifelse(latent > 0.75, 1, 0)) %>%
  mutate(yObs = ifelse(D == 1, y1, y0))

reg2 <- lm(yObs ~ D, data = data2)
summary(reg2)

# 12
plot(density(data$yObs[data$D == 1]))
lines(density(data$yObs[data$D == 0]))

# 13

data3 <- as.data.frame(rbinom(1000000, 1, 0.5)) 

names(data3) <- c("gender")

data3 <- data3 %>%
  mutate(y0 = rnorm(1000000, mean=5, sd=1)) %>%
  mutate(y0 = ifelse(gender == 1, (y0 + 1), y0)) %>%
  mutate(c = 0,
         y1 = y0 + c) %>%
  mutate(latent = 0.5*gender + runif(1000000, min = 0, max = 1),
         D = ifelse(latent > 0.75, 1, 0)) %>%
  mutate(yObs = ifelse(D == 1, y1, y0))

reg3 <- lm(yObs ~ D, data = data3)
summary(reg3)

# 14

reg4 <- lm(yObs ~ D + gender, data = data3)
summary(reg4)
