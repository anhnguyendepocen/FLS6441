library(data.table)
library(tidyverse)
library(effects)
library(MASS)
library(Zelig)
library(ZeligChoice)
library(estimatr)

#loading the data
data <- fread("GGL_data_prepared.csv")

#######
## 4 ##
#######
table1 <- data %>%
  group_by(hh_id,treatment) %>%
  summarise_all(funs(mean))%>%
  group_by(treatment) %>%
  mutate(n=n())%>%
  summarise_all(funs(mean))%>%
  dplyr::select(treatment,hh_size,g2002,g2000,p2004,p2002,p2000,sex,age,n)

table1


#######
## 5 ##
#######
t.test(data$age[data$treatment=="Control"], data$age[data$treatment=="Neighbors"])

#######
## 6 ##
#######
t.test(data$voted[data$treatment=="Control"], data$voted[data$treatment=="Neighbors"])

#######
## 7 ##
#######

#reordering the treatment factor so the "Control" is the reference group for estimates
data <- data%>%
  mutate(treatment=factor(treatment, levels = c("Control",
                                                "Civic Duty",
                                                "Hawthorne",
                                                "Self", 
                                                "Neighbors")))

#simple OLS
reg1 <- lm(voted ~ treatment,data = data)

stargazer(reg1)
summary(reg1)

#######
## 9 ##
#######

library(RCurl)
library(gdata) 
library(zoo)
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
library(knitr)
kable(summary(reg1,cluster = c("hh_id")))

#######
## 10 #
#######

clusmean <- data %>%
  mutate(treat.neighbor = ifelse(treatment=="Neighbors", 1,0)) %>%
  filter(treatment %in% c("Control", "Neighbors")) %>%
  group_by(cluster) %>%
  summarise(treat.mean = mean(treat.neighbor)) %>%
  ungroup()

data2 <- data %>%
  filter(treatment %in% c("Control", "Neighbors")) %>%
  mutate(treat.neighbor = ifelse(treatment=="Neighbors", 1,0)) %>%
  left_join(clusmean) %>%
  mutate(treat.neighbor2 = treat.neighbor-treat.mean)

reg2 <- lm(voted ~ treat.neighbor2 ,data = data2)
pander(summary(reg2,cluster = c("hh_id")))

#######
## 11 #
#######

reg3 <- lm(voted ~ treat.neighbor2 + g2000 + g2002 +p2000+p2002+p2004 ,data = data2)
summary(reg3,cluster = c("hh_id"))

#######
## 12 #
#######


reg4.logit <- zelig(voted ~ treatment,
                    data = data, model = "logit")

summary(reg4.logit)

exp(0.36509) #it's 1.44

summary(reg4.logit,odds_ratios = TRUE)


#######
## 13 #
#######

x.control<- setx(reg4.logit, treatment = "Control")
x.treat <- setx(reg4.logit, treatment = "Neighbors")
logit.sim <- sim(reg4.logit, x = x.control, x1 = x.treat)
summary(logit.sim)
par(mar=c(2.5,2.5,2.5,2.5))
plot(logit.sim)