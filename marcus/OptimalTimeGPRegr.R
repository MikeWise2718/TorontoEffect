library(tidyverse)
library(`gp.regression`)

setwd("C:/Users/macolli/Documents/SkylightCustomer/EconometricModel/")
mydata <- read.csv("ChatsByHourOfWeek.csv")

mydata['hour_of_day'] = mydata$hour_of_week %% 24
mydata['treatment'] = (mydata$week== 10)*((mydata$hour_of_week==54)+(mydata$hour_of_week==55)+(mydata$hour_of_week== 128))+ 
  (mydata$week==11)*((mydata$hour_of_week==54)+(mydata$hour_of_week==55)+(mydata$hour_of_week==25)+(mydata$hour_of_week==26))
reg1 = glm(ChatsInBlock ~ as.factor(week) + as.factor(hour_of_day) + as.factor(treatment), data=mydata)
summary(reg1)
reg2 = glm(rate ~ as.factor(week) + as.factor(hour_of_day) + as.factor(treatment), data=mydata)
summary(reg2)

mydata <- read.csv("CallsByHourOfWeek.csv")
mydata['hour_of_day'] = mydata$hour_of_week %% 24
mydata['treatment'] = (mydata$week== 10)*((mydata$hour_of_week==54)+(mydata$hour_of_week==55)+(mydata$hour_of_week== 128))+ 
  (mydata$week==11)*((mydata$hour_of_week==54)+(mydata$hour_of_week==55))
reg3 = glm(WindowsToAgentCalls ~ as.factor(week) + as.factor(hour_of_day) + as.factor(treatment), data=mydata)
summary(reg3)
reg4 = glm(rate ~ as.factor(week) + as.factor(hour_of_day) + as.factor(treatment), data=mydata)
summary(reg4)

