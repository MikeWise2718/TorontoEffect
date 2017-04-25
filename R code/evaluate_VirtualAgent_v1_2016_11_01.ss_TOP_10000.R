#library(strucchange)
library(plyr)
library(dplyr)
library(doBy)
library(reshape2)
library(tidyr)
library(rpart)
library(rpart.plot)
#library(glmnet)
#library(MSwM)

setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto")



mydata <- read.csv("VirtualAgent_v1_2016_11_01.ss_TOP_10000.csv")
colnames(mydata)

# 112 real variables.  The rest don't are a bunch of null values.
mydata <- mydata[,1:112]

unique(mydata$PartnerId)
# smc community xbox ivr are the unique elements

as.data.frame(table(mydata$TotalLandingPageViews,mydata&ExperienceDuration))
#  Var1 Freq
#1           4136
#2 community  322
#3       ivr    2
#4       smc 5464
#5      xbox   76

colnames(mydata)

#Total seconds by total landing page views (landing page is observation 18)
aggregate(mydata[, 18], list(mydata$TotalLandingPageViews), mean)

# Summary statistics by number of clocks on "Get Started"
ddply(mydata, c("TotalGetStartedClicks"), summarise,  mean = mean(ExperienceDuration), sd = sd(ExperienceDuration))

ddply(mydata, c("TotalGetStartedClicks"), summarise,  mean = mean(TotalUserResponsesSubmitted), sd = sd(TotalUserResponsesSubmitted))
as.data.frame(table(mydata$TotalGetStartedClicks,mydata$TotalUserResponsesSubmitted))

unique(mydata$TotalUserResponsesSubmitted)

unique(mydata$TotalAnswerResponsesReceived)
ddply(mydata, c("TotalAnswerResponsesReceived"), summarise,  mean = mean(ExperienceDuration), sd = sd(ExperienceDuration))

mydata<-mydata[order(mydata$ClickedInstantAnswerFeedbackSuccess),]
