### XBox Chat data
### Data comes from Marcus Collins' scope script from .ss structured stream called ChatSessionUnified in Cosmos

# These library permit use of heteroskedastic robust standard errors 
library(lmtest)
library(sandwich)
library(tidyverse)
library(stringr)


#setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto/Data")
setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto/Data")

phonedata <- read.csv("xbox_call_data_genesys.csv")
colnames(phonedata)

unique(phonedata$COUNTRY_CODE)

#Keep only XBox USA Chat data
newdata <- subset(phonedata,COUNTRY_CODE == "US")
ordered <-newdata[with(newdata, order(Date, Hour)), ]



newdata$time <- str_c(newdata$Hour,"00",sep=":")
date_parts = t(as.data.frame(strsplit(as.character(newdata$Date),' ')))
newdata$fetcha <- date_parts[,1]
newdata$dtt <- paste(newdata$fetcha,newdata$time)
newdata$dt <- as.POSIXct(newdata$dtt, format = "%m/%d/%Y %H:%M")
newdata <-newdata[with(newdata, order(newdata$dt)), ]
newdata <- subset(newdata,newdata$dt >= "2016-07-15 00:00:00")
newdata <- subset(newdata,newdata$dt < "2016-12-23 00:00:00")
newdata$date_formatted <- as.Date(newdata$dt, tz = "America/Los_Angeles")

newdata %>%    summarise_each (funs(mean, min, max, sd) , Calls_per_hour = XboxABCount)

#####################
# Daily Plot
#####################
vars <- c("XboxABCount","dt")
ordered_US <- newdata[vars]

ag3   <- aggregate(. ~ cut(ordered_US$dt, "1 day"), ordered_US[setdiff(names(ordered_US), "dt")], sum)
colnames(ag3)[1] <- "daily_time"
ag3$date_formatted <- as.Date(ag3$daily_time, tz = "America/Los_Angeles")


#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11", ag3$date_formatted))
sky2<-which(grepl("2016-10-18", ag3$date_formatted))
sky3<-which(grepl("2016-11-01", ag3$date_formatted))
sky4<-which(grepl("2016-12-15", ag3$date_formatted))
content1<-which(grepl("2016-11-17", ag3$date_formatted))
#Plot of the raw daily data 
plot(ag3$date_formatted, ag3$XboxABCount,  main = "Xbox A&B Call: Raw Data Aggregated to Daily Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(ag3$date_formatted[sky1]),col = "red")
abline(v=as.numeric(ag3$date_formatted[sky2]),col = "red")
abline(v=as.numeric(ag3$date_formatted[sky3]),col = "red")
abline(v=as.numeric(ag3$date_formatted[sky4]),col = "red")
abline(v=as.numeric(ag3$date_formatted[content1]),col = "blue")
#########################################



ordered <-newdata[with(newdata, order(dt, Hour)), ]


daily <- subset(phonedata,Hour == 0)
daily <- subset(daily,COUNTRY_CODE == "US")
daily$dt <- as.POSIXct(daily$Date, format = "%m/%d/%Y %H:%M")
daily <- daily[with(daily, order(dt)), ]
daily$date_formatted <- as.Date(daily$dt, tz = "America/Los_Angeles")
daily$count <- seq(length(daily$Hour))
daily$DOW <- weekdays(daily$date_formatted)


colnames(daily)

####################
# Order the data since not date formatted at the hourly level
####################
daily_small <- daily[c("date_formatted","count","DOW")]
m1 <- merge(newdata, daily_small, by="date_formatted")

hist(m1$XboxABCount, main = "Histogram of Xbox A&B Calls/hour: Raw Data", xlab = "Calls/Hour", ylab = "Frequency")


##### Subset to remove the new content
# m1$dum2 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
# m1$dum3 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
# m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
###############################
# Choose one week on either side of these breaks

#############################
#####  Third Skylight Rollout
#############################

#Remove DOW FEs from sample
fitted_DOW_HOD = glm(XboxABCount ~ as.factor(DOW)*as.factor(Hour), data=m1)
m1$XboxABCount_DOW_HOD <- resid(fitted_DOW_HOD)
summary(fitted_DOW_HOD)
coeftest(fitted_DOW_HOD, vcov = vcovHC(fitted_DOW_HOD, "HC1"))
fitted_DOW = glm(XboxABCount ~ as.factor(DOW), data=m1)
m1$XboxABCount_DOW <- resid(fitted_DOW)
summary(fitted_DOW)
coeftest(fitted_DOW, vcov = vcovHC(fitted_DOW, "HC1"))

m2 <-m1
break3<-"2016-11-01 00:00:00"
break3_date <- "2016-11-01"
msky3 <-subset(m2,m2$dt >= "2016-10-25 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-11-08 00:00:00")
msky3$sky30_50 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","XboxABCount")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d %H:%M:%S")
sky3<-which(grepl(break3, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$XboxABCount))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-11-06 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-11-06 00:00:00")
msky3 <-subset(msky3,msky3$dt > "2016-10-26 00:00:00")


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]

#utils::View(msky3_linear)
msky3_linear <-subset(msky3_linear,msky3_linear$dt >= "2016-10-27 00:00:00")
utils::View(msky3_linear)
msky3_linear$linear1 <- msky3_linear$linear1-2
msky3_linear$linear1 <- ifelse(msky3_linear$linear1<0 ,0,msky3_linear$linear1)


##### Linear regression
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~  linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour) + linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy3 DOW HOD
sky3_only<-which(grepl("2016-11-01 00:00:00", msky3_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW by HOD, no dummy)")
abline(v=as.numeric(msky3_linear$dt[sky3_only]),col = "red")

##### Linear RESIDUAL PLOT on SKy3 DOW
sky3_only<-which(grepl("2016-11-01 00:00:00", msky3_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour)  + linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW, no dummy)")
abline(v=as.numeric(msky3_linear$dt[sky3_only]),col = "red")




###############################
# Choose five days on either side of these breaks
# #m1$sky10_30 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
###############################

m2 <-m1
# Second break
break2<-"2016-10-18 00:00:00"
break2_date<-"2016-10-18"
msky2 <-subset(m2,m2$dt >= "2016-10-13 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-10-25 00:00:00")
msky2$sky10_30 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","XboxABCount")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$XboxABCount))
daily$linear2 <- daily$linear1 -(sky2-1) 
daily$linear1 <- ifelse(daily$daily_time >= break2,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break2,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-10-23 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-10-23 00:00:00")

# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]


#Convert to a single date variable
msky2$date_only<- as.Date(msky2$dt, tz = "America/Los_Angeles")
utils::View(msky2)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky2_linear <- merge(temp,msky2, by= "date_only")
msky2_linear <- msky2_linear[with(msky2_linear, order(dt)), ]
utils::View(msky2_linear)


##### Linear regression
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~  linear1+ linear2+ sky10_30, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour) + linear1+ linear2+ sky10_30, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy3 DOW HOD
sky3_only<-which(grepl("2016-10-18 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~ linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW by HOD, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky3_only]),col = "red")

##### Linear RESIDUAL PLOT on SKy3 DOW
sky3_only<-which(grepl("2016-10-18 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky3_only]),col = "red")


########################################

###############################
# Choose one week on either side of these breaks
# #m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
###############################
m2 <-m1
# Fourth break
break2<-"2016-12-15 00:00:00"
break2_date<-"2016-12-15"
msky2 <-subset(m2,m2$dt >= "2016-12-10 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-12-21 00:00:00")
msky2$sky50_90 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","XboxABCount")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$XboxABCount))
daily$linear2 <- daily$linear1 -(sky2-1) 
daily$linear1 <- ifelse(daily$daily_time >= break2,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break2,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-12-20 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-12-20 00:00:00")

# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky2$date_only<- as.Date(msky2$dt, tz = "America/Los_Angeles")
utils::View(msky2)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky2_linear <- merge(temp,msky2, by= "date_only")
msky2_linear <- msky2_linear[with(msky2_linear, order(dt)), ]
utils::View(msky2_linear)



##### Linear regression
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~  linear1+ linear2+ sky50_90, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour) + linear1+ linear2+ sky50_90, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy3 DOW HOD
sky3_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~ linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW by HOD, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky3_only]),col = "red")

##### Linear RESIDUAL PLOT on SKy3 DOW
sky3_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky3_only]),col = "red")

#Raw Data
plot(msky2_linear$dt, msky2_linear$XboxABCount,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(msky2_linear$dt[sky3_only]),col = "red")



#############################
#####  Content Change
# m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
#############################

m2 <- m1
# Second break
break2<-"2016-11-17 00:00:00"
break2_date<-"2016-11-17"
msky2 <-subset(m2,m2$dt >= "2016-11-12 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-11-23 00:00:00")
msky2$content <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","XboxABCount")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$XboxABCount))
daily$linear2 <- daily$linear1 -(sky2-1) 
daily$linear1 <- ifelse(daily$daily_time >= break2,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break2,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-11-22 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-11-22 00:00:00")

# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky2$date_only<- as.Date(msky2$dt, tz = "America/Los_Angeles")
#utils::View(msky2)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky2_linear <- merge(temp,msky2, by= "date_only")
msky2_linear <- msky2_linear[with(msky2_linear, order(dt)), ]
utils::View(msky2_linear)

##### Linear regression
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~   linear1+ linear2+ content, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour)  + linear1+ linear2+ content , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-11-17 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW_HOD ~  linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Calls/Hour (DOW by HOD FEs, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy2 Chat DUration
sky2_only<-which(grepl("2016-11-17 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(XboxABCount_DOW ~ as.factor(Hour)  +linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Durations", xlab = "Date", ylab = "Residual Calls/Hour (DOW FEs, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")

#Raw Data
plot(msky2_linear$dt, msky2_linear$XboxABCount,  main = "Calls/Hour", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##################
# Use all data in single regression
##################
m2 <-m1

m2$count2 <- m2$count^2
m2$count3 <- m2$count^3
m2$count4 <- m2$count^4
m2$count5 <- m2$count^5
m2$count6 <- m2$count^6
m2$count7 <- m2$count^7
m2$count8 <- m2$count^8
m2$count9 <- m2$count^9
m2$count10 <- m2$count^10

utils::View(m2)

##### 8TH ORDER RESIDUAL PLOT
reg_poly8 = glm(XboxABCount ~ as.factor(Hour)*as.factor(DOW) + X1 + X2 + X3 + X4 +XC1 +count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m2)
summary(reg_poly8)
coeftest(reg_poly8, vcov = vcovHC(reg_poly8, "HC1"))

##### 10TH ORDER RESIDUAL PLOT
reg_poly8 = glm(XboxABCount ~ as.factor(Hour)*as.factor(DOW) + X1 + X2 + X3 + X4 +XC1 +count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + count9 + count10, data=m2)
summary(reg_poly8)
coeftest(reg_poly8, vcov = vcovHC(reg_poly8, "HC1"))

plot(m1$dt, m1$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")

