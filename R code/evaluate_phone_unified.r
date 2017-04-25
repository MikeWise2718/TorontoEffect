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

setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto")
#setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto")

mydata <- read.csv("phone_unified_features.csv")
colnames(mydata)

#Shorten Dataset to be able to work quickly.  Full program will have to run over hours. 
mydata_short <- mydata[1:100000,]
#Correctly format the variable
mydata_short$Date <- as.character(mydata_short$Date)
#Create new object to operate on
dtparts <-  t(as.data.frame(strsplit(mydata_short$Date,' ')))
#Eliminate the unneeded rowname
row.names(dtparts) = NULL


####################
#Get the vectors in the proper form
dtparts[,2] <- paste(dtparts[,2], dtparts[,3], sep = " ")
#time isn't needed anymore but in the future it could be
time <- dtparts[,2]
####################

#Isolate and order by date
date <- dtparts[,1]
mydata_short$date_only <- date
date <- unique(date)
date <- strptime(date,format='%m/%d/%Y')
date <- date[order(as.Date(date))]
#date$trend <- seq(length(date))
poly <- data.frame(date)
names(poly)[1] <- "date_only"
poly$trend <- seq(length(poly$date_only))
mydata_short$date_only <- strptime(mydata_short$date_only,format='%m/%d/%Y')
complete <- merge(poly,mydata_short,by=c("date_only"))
#This merge isn't wanting to work and not sure how.


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
