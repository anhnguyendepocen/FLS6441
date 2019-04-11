getwd()
library(tidyverse)
library(data.table)
library(broom)
library(devtools)
install.packages("list")
install.packages("mvtnorm")
install_github("https://github.com/cran/mvtnorm")
library(list)
library(Zelig)
library(estimatr)
library(dotwhisker)

sdata <- fread("https://jonnyphillips.github.io/Methods_III/Classes/Class_4/Lab_Exercise/Survey_data.csv")
cdata <- fread("https://jonnyphillips.github.io/Methods_III/Classes/Class_4/Lab_Exercise/Conjoint_data.csv")

glimpse(sdata)
glimpse(cdata)

###1 

t.test(sdata$Constitution_Reform_Support[sdata$Primed==1],
       sdata$Constitution_Reform_Support[sdata$Primed==0])

#those who were primed have a higher support for constitutional reform

###2

reg1 <- lm(Constitution_Reform_Support ~ Primed, data=sdata)
summary(reg1)

# the primed people have on average 11.4% more support than those who weren't 

###3 

ATE_list <- (mean(sdata$list_response[sdata$list_treated==1],na.rm = T) - 
  mean(sdata$list_response[sdata$list_treated==0],na.rm = T))

ATE_list

#Around 20.8% of people who saw the sensitive item picked the item

###4

reg2 <- tidy(lm(list_response ~ list_treated, data=sdata))
summary(reg2)

lbound <- reg2$estimate + 1.96*reg2$std.error
hbound <- reg2$estimate - 1.96*reg2$std.error

#the 95% confidence interval is between 0.144 and 0.274

###5 

?ict.test

ict.test(sdata$list_response, sdata$list_treated)

###6

ictreg(list_response~1,
       data=survey %>% as.data.frame(),
       treat="list_treated",
       J=3,
       method="ml",
       floor=T,
       ceiling=T,
       floor.fit="bayesglm",
       ceiling.fit="bayesglm") %>%
  summary()

###7

100*sum(is.na(sdata$direct_clientelism))/nrow(sdata)
100*sum(is.na(sdata$list_response))/nrow(sdata)

#While only 0.4% of people haven't answered the indirect measure, over 8% refused.

###8

table(sdata$direct_clientelism)


###9

sdata <- sdata%>%
  mutate(gender=factor(gender, levels = c("male",
                                                "female",
                                                "other")))

reg3 <- lm(list_response ~ list_treated*gender + list_treated + gender, data=sdata)
summary(reg3)

#women were less likely to answer the sensitive item 


### 10
a<-length(levels(as.factor(cdata$Profile_Gender)))
b<-length(levels(as.factor(cdata$Profile_Caste)))
c<-length(levels(as.factor(cdata$Profile_Party)))
d<-length(levels(as.factor(cdata$Profile_PG)))
e<-length(levels(as.factor(cdata$Profile_Promise)))

combinations <- a*b*c*d*e  
combinations

#there are 120 possible combinations

### 11

reg4 <- lm(conjoint_choice ~ Profile_PG, data = cdata)
summary(reg4)

#the Public Goods profile increases 22% the respondent choice

### 12

reg4.logit <- zelig(conjoint_choice ~ Profile_PG, data = cdata, model = "logit")
summary(reg4.logit,odds_ratios = TRUE)

# The Public Goods profile increases in 145% the odds of chosing the candidate

### 13

reg4.robust <- lm_robust(conjoint_choice ~ Profile_PG, data = cdata,clusters = UID)
summary(reg4.robust)
summary(reg4)

### 14

reg5 <- lm(conjoint_choice ~ Profile_Gender +
                   Profile_Caste + Profile_Party + Profile_PG + 
                   Profile_Promise, data = cdata) %>%
dwplot(conf.level = .95,
       vline = geom_vline(xintercept = 0,colour = "grey50", linetype = 2)) +
  xlab("Coefficient")

# Positive effect: OBC Caste, SC Caste, Public Good rodas
# Negative effect: RJD Party 

### 15 

cdata <- cdata%>%
  mutate(respondent_gender=factor(respondent_gender, levels = c("male",
                                          "female")))

reg6 <- lm(conjoint_choice ~ Profile_Gender +
     Profile_Caste + Profile_Party + Profile_PG + 
     Profile_Promise + respondent_gender + Profile_Promise*respondent_gender, data = cdata)
summary(reg6)

dwplot(reg6,conf.level = .95,
       vline = geom_vline(xintercept = 0,colour = "grey50", linetype = 2))

#Women respond more positive to patronage than men   
