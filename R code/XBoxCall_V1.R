### XBox Chat data
### Data comes from Marcus Collins' scope script from .ss structured stream called ChatSessionUnified in Cosmos

# These library permit use of heteroskedastic robust standard errors 
library(lmtest)
library(sandwich)
library(tidyverse)
library(stringr)


setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto/Data")
#setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto/Data")

phonedata <- read.csv("xbox_call_data_genesys.csv")
colnames(phonedata)

#Format as a time series
phonedata$dt <- as.POSIXct(phonedata$Date, format = "%m/%d/%Y %H:%M")
#Take relevant subset


#####################
# Daily Plot
#####################
ag3 <- aggregate(. ~ cut(ordered_US$dt, "1 day"), ordered_US[setdiff(names(ordered_US), "dt")], sum)
colnames(ag3)[1] <- "daily_time"
ag3$date_formatted <- as.Date(ag3$daily_time)

#Drop final day for which there is only a subset of hours.
ag3 <- subset(ag3,ag3$date_formatted <= "2016-12-06")


#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11", ag3$date_formatted))
sky2<-which(grepl("2016-10-18", ag3$date_formatted))
sky3<-which(grepl("2016-11-01", ag3$date_formatted))
content1<-which(grepl("2016-11-17", ag3$date_formatted))
#Plot of the raw daily data 
plot(ag3$date_formatted, ag3$ChatsInBlock,  main = "Xbox A&B Chat: Raw Data Aggregated to Daily Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(ag3$date_formatted[sky1]),col = "red")
abline(v=as.numeric(ag3$date_formatted[sky2]),col = "red")
abline(v=as.numeric(ag3$date_formatted[sky3]),col = "red")
abline(v=as.numeric(ag3$date_formatted[content1]),col = "blue")
#########################################



ordered <-phonedata[with(phonedata, order(Date, Hour)), ]


daily <- subset(phonedata,Hour == 0)
daily <- subset(daily,COUNTRY_CODE == "US")
daily <- daily[with(daily, order(dt)), ]
daily$count <- seq(length(daily$Hour))
daily$count2 <- daily$count^2
daily$count3 <- daily$count^3
daily$count4 <- daily$count^4
daily$count5 <- daily$count^5
daily$count6 <- daily$count^6
daily$count7 <- daily$count^7

colnames(daily)

####################
# Order the data since not date formatted at the hourly level
####################
vars <- c("dt","count", "count2","count3", "count4","count5", "count6","count7")
newdata <- daily[vars]
m1 <- merge(newdata, ordered, by="dt")
ordered_US <- subset(m1,COUNTRY_CODE == "US")
ordered_US <-ordered_US[with(ordered_US, order(dt, Hour)), ]
ordered_US$day <- weekdays(as.Date(ordered_US$dt))
#############################



colnames(ordered_US)
#On to the regressions
reg_poly7_full = glm(XboxABCount ~ as.factor(Hour) +as.factor(day) + count + count2 + count3 + count4 + count5 + count6 + count7  + X1 + X2 + X3 + X4 + XC1, data=ordered_US)
summary(reg_poly7_full)
coeftest(reg_poly7_full, vcov = vcovHC(reg_poly7_full, "HC1"))

#On to the regressions
reg_poly4_full = glm(XboxABCount ~ as.factor(Hour) +as.factor(day) + count + count2 + count3 + count4  + X1 + X2 + X3 + X4 + XC1, data=ordered_US)
summary(reg_poly4_full)
coeftest(reg_poly4_full, vcov = vcovHC(reg_poly4_full, "HC1"))

#On to the regressions
reg_poly5_full = glm(XboxABCount ~ as.factor(Hour) +as.factor(day) + count + count2 + count3 + count4 + count5 + X1 + X2 + X3 + X4 + XC1, data=ordered_US)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

#On to the regressions
reg_poly6_full = glm(XboxABCount ~ as.factor(Hour) +as.factor(day) + count + count2 + count3 + count4 + count5 + count6 + X1 + X2 + X3 + X4 + XC1, data=ordered_US)
summary(reg_poly6_full)
coeftest(reg_poly6_full, vcov = vcovHC(reg_poly6_full, "HC1"))

#On to the regressions
reg_poly7_full = glm(XboxABCount ~ as.factor(Hour) +as.factor(day) + count + count2 + count3 + count4 + count5 + count6 + count7  + X1 + X2 + X3 + X4 + XC1, data=ordered_US)
summary(reg_poly7_full)
coeftest(reg_poly7_full, vcov = vcovHC(reg_poly7_full, "HC1"))

coeftest(reg_poly4_full, vcov = vcovHC(reg_poly4_full, "HC1"))
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))
coeftest(reg_poly6_full, vcov = vcovHC(reg_poly6_full, "HC1"))
coeftest(reg_poly7_full, vcov = vcovHC(reg_poly7_full, "HC1"))

stargazer(ordered_US[c("XboxABCount")], type = "text", title="Descriptive statistics", digits=1, out="Summary_call.txt")

#########################
# Create various dummy variables
#########################

#Create dummies with correct initiation date for various treatments.
m1$dum1 <- ifelse(m1$dt >= "2016-10-11 00:00:00",1,0)
m1$dum2 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
m1$dum3 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
#####
#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11 00:00:00", m1$dt))
sky2<-which(grepl("2016-10-18 00:00:00", m1$dt))
sky3<-which(grepl("2016-11-01 00:00:00", m1$dt))
content1<-which(grepl("2016-11-17 00:00:00", m1$dt))


#Plot of the raw data
plot(m1$dt, m1$ChatsInBlock,  main = "Raw Data Aggregated to Hour Level", xlab = "Date", ylab = "Chat Initiations")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")



#stargazer will help with tables and display of data
library(stargazer)
#Table of summary statistics
stargazer(m1[c("ChatsInBlock")], type = "text", title="Descriptive statistics", digits=1, out="Summary.txt")



###################
# Combine hour and date data
##################
utils::View(phonedata)
ordered_US$time <- str_c(ordered_US$Hour,"00",sep=":")


#Main specification of 8th order Polynomial
reg_poly8_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + dum1 + dum2 + dum3 + dum4, data=m1)
summary(reg_poly8_full)
coeftest(reg_poly8_full, vcov = vcovHC(reg_poly8_full, "HC1"))


##### 8TH ORDER RESIDUAL PLOT
reg_hourly_resid_poly8 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m1)
m1$hourly_resid_poly8 <- resid(reg_hourly_resid_poly8)
plot(m1$dt, m1$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")

#Control for day of month fixed effects (A&B likely varies within month)
m1$DOM <-as.numeric(days(m1$dt))
m1$early_month <- ifelse(m1$DOM < 6,1,0)

#10th order Polynomial with early month
reg_poly10_early_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9+count10+early_month+ dum1 + dum2 + dum3 + dum4, data=m1)
summary(reg_poly10_early_full)
coeftest(reg_poly10_early_full, vcov = vcovHC(reg_poly8_early_full, "HC1"))

#Main specification of 10th order Polynomial
reg_poly10_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9 + count10 +dum1 + dum2 + dum3 + dum4, data=m1)
summary(reg_poly10_full)
coeftest(reg_poly10_full, vcov = vcovHC(reg_poly10_full, "HC1"))

##### 10TH ORDER RESIDUAL PLOT
reg_hourly_resid_poly10 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9 + count10, data=m1)
m1$hourly_resid_poly10 <- resid(reg_hourly_resid_poly10)
plot(m1$dt, m1$hourly_resid_poly10,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 10th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")


#10th order Polynomial with early month no black Friday
no_black <- subset(m1,m1$dt < "2016-11-24 00:00:00")
reg_poly8_early_black = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9+count10+dum1 + dum2 + dum3 + dum4, data=no_black)
summary(reg_poly8_early_black)
coeftest(reg_poly8_early_black, vcov = vcovHC(reg_poly8_early_black, "HC1"))


reg_poly10_early_black_resid = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9+count10, data=no_black)
no_black$reg_poly10_early_black_resid <- resid(reg_poly10_early_black_resid)
plot(no_black$dt, no_black$reg_poly10_early_black_resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 10th Order polynomial, no dummies)")
abline(v=as.numeric(no_black$dt[sky1]),col = "red")
abline(v=as.numeric(no_black$dt[sky2]),col = "red")
abline(v=as.numeric(no_black$dt[sky3]),col = "red")
abline(v=as.numeric(no_black$dt[content1]),col = "blue")



##### 6TH ORDER Polynomial Regression
reg_poly6_early_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + dum1 + dum2 + dum3 + dum4, data=m1)
summary(reg_poly6_early_full)
coeftest(reg_poly6_early_full, vcov = vcovHC(reg_poly6_early_full, "HC1"))


##### Skylight Only regressions
######################################
##### Subset to remove the new content 
m2 <-subset(m1,m1$dt <= "2016-11-17 00:00:00")

##### 6th order on skylight only rollout
reg_poly6_early_sky = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + dum1 + dum2 + dum3, data=m2)
summary(reg_poly6_early_sky)
coeftest(reg_poly6_early_sky, vcov = vcovHC(reg_poly6_early_sky, "HC1"))


##### 6TH ORDER RESIDUAL PLOT on SKylight only data
reg_hourly_resid_poly6_sky = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6, data=m2)
m2$reg_hourly_resid_poly6_sky <- resid(reg_hourly_resid_poly6_sky)
plot(m2$dt, m2$reg_hourly_resid_poly6_sky,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 6th Order polynomial, no dummies)")
abline(v=as.numeric(m2$dt[sky1]),col = "red")
abline(v=as.numeric(m2$dt[sky2]),col = "red")
abline(v=as.numeric(m2$dt[sky3]),col = "red")
abline(v=as.numeric(m2$dt[content1]),col = "blue")

##### 8th order on skylight only rollout
reg_poly8_early_sky = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + dum1 + dum2 + dum3, data=m2)
summary(reg_poly8_early_sky)
coeftest(reg_poly8_early_sky, vcov = vcovHC(reg_poly8_early_sky, "HC1"))
####  *** NOTE: For these skylight only regressions the polynomial time trend is not statistically significant


##### 8th order on skylight only rollout
reg_poly8_early_sky = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + dum1 + dum2 + dum3, data=m2)
summary(reg_poly8_early_sky)
coeftest(reg_poly8_early_sky, vcov = vcovHC(reg_poly8_early_sky, "HC1"))
####  *** NOTE: For these skylight only regressions the polynomial time trend is not statistically significant


##### Subset to remove the new content
# m1$dum2 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
# m1$dum3 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
# m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
###############################
# Choose one week on either side of these breaks

msky3 <-subset(m1,m1$dt >= "2016-10-24 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-11-08 00:00:00")

#Create polynomial for time trend
msky3$count <- seq(length(msky3$ChatsInBlock))
msky3$count2 <- msky3$count^2
msky3$count3 <- msky3$count^3
msky3$count4 <- msky3$count^4
msky3$count5 <- msky3$count^5
msky3$count6 <- msky3$count^6

##### 5Th ORDER Polynomial Regression
reg_poly3_sky3 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + dum3, data=msky3)
summary(reg_poly3_sky3)
coeftest(reg_poly3_sky3, vcov = vcovHC(reg_poly3_sky3, "HC1"))

##### 5th ORDER RESIDUAL PLOT on SKy3
sky3_only<-which(grepl("2016-11-01 00:00:00", msky3$dt))
reg_resid_poly3_sky3 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 +count4 +count5, data=msky3)
msky3$reg_resid_poly3_sky3 <- resid(reg_resid_poly3_sky3)
plot(msky3$dt, msky3$reg_resid_poly3_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 3rd Order polynomial, no dummy)")
abline(v=as.numeric(msky3$dt[sky3]),col = "red")



#############################
#####  Second Skylight Rollout
#############################

msky2 <-subset(m1,m1$dt >= "2016-10-11 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-10-24 00:00:00")

#Create polynomial for time trend
msky2$count <- seq(length(msky2$ChatsInBlock))
msky2$count2 <- msky2$count^2
msky2$count3 <- msky2$count^3
msky2$count4 <- msky2$count^4
msky2$count5 <- msky2$count^5
msky2$count6 <- msky2$count^6

##### 5th ORDER Polynomial Regression
reg_poly5_sky2 = glm(ChatsInBlock ~ as.factor(Hour)  + count + count2 + count3 +count4 +count5 + as.factor(ChatDOW) + dum2, data=msky2)
summary(reg_poly5_sky2)
coeftest(reg_poly5_sky2, vcov = vcovHC(reg_poly5_sky2, "HC1"))


##### 5th ORDER RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-10-18 00:00:00", msky2$dt))
reg_resid_poly5_sky2 = glm(ChatsInBlock ~ as.factor(Hour)  + count + count2 + count3 +count4 +count5 + as.factor(ChatDOW), data=msky2)
msky2$reg_resid_poly5_sky2 <- resid(reg_resid_poly5_sky2)
plot(msky2$dt, msky2$reg_resid_poly5_sky2,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 5th Order polynomial, no dummy)")
abline(v=as.numeric(msky2$dt[sky2_only]),col = "red")




#############################
#####  Content Change
# m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
#############################

msky4 <-subset(m1,m1$dt >= "2016-11-10 00:00:00")
msky4 <-subset(msky4,msky4$dt < "2016-11-24 00:00:00")

#Create polynomial for time trend
msky4$count <- seq(length(msky4$ChatsInBlock))
msky4$count2 <- msky4$count^2
msky4$count3 <- msky4$count^3
msky4$count4 <- msky4$count^4
msky4$count5 <- msky4$count^5
msky4$count6 <- msky4$count^6

##### 5th ORDER Polynomial Regression
reg_poly5_sky4 = glm(ChatsInBlock ~ as.factor(Hour)  + count + count2 + count3 +count4 +count5 +dum4, data=msky4)
summary(reg_poly5_sky4)
coeftest(reg_poly5_sky4, vcov = vcovHC(reg_poly5_sky4, "HC1"))


##### 5th ORDER RESIDUAL PLOT on SKy4
sky4_only<-which(grepl("2016-11-17 00:00:00", msky4$dt))
reg_resid_poly5_sky4 = glm(ChatsInBlock ~ as.factor(Hour)  + count + count2 + count3 +count4 +count5 + as.factor(ChatDOW), data=msky4)
msky4$reg_resid_poly5_sky4 <- resid(reg_resid_poly5_sky4)
plot(msky4$dt, msky4$reg_resid_poly5_sky4,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 5th Order polynomial, no dummy)")
abline(v=as.numeric(msky4$dt[sky4_only]),col = "red")


#############################
#####  Changes in volume by week
#############################
ByWeek <- aggregate(. ~ cut(m1$dt, "1 week"), ByWeek[setdiff(names(m1), "dt")], sum)


