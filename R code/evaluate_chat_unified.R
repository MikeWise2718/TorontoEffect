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
library(chron)

#setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto")
setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto")

mydatachat <- read.csv("ChatSessionUnified_2016_11_20-Lmtd.csv")
chat_unified <- read.csv("phone_unified_features.csv")

mydata_short <- chat_unified[1:30000,]
reg = glm(CallDuration ~ as.factor(Hour) + as.factor(CallDOW) + as.factor(ProductFamily) +as.factor(SupportChannel)*SL1 +as.factor(SupportChannel)*SL2 +as.factor(SupportChannel)*SL3, data=mydata_short)
summary(reg)

date_parts = t(as.data.frame(strsplit(as.character(mydata_short$Date),' ')))
rdate<-as.Date(chat_unified$Date1,"%m/%d/%y")




colnames(mydata)

as.data.frame(table(mydata$CallTypeId))

#Create subset of data to work with
mydata_short <- mydata[1:10000,]

#code below not wating to work; error in usage of "strsplit"
date_parts = t(as.data.frame(strsplit(as.character(mydata_short$CallStartDateTime),' ')))
thetimes = chron(dates=date_parts[,1],times=date_parts[,2],format=c('m/d/y','h:m'))


date_parts = t(as.data.frame(strsplit(mydata_short$CallStartDateTime,' ')))
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('y-m-d','h:m:s'))

# Format date and time date to calculate call duration in seconds. 
mydata$CallStartDateTime<-as.POSIXct(mydata$CallStartDateTime)


# unique(mydata$PartnerId)
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
