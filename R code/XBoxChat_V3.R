### XBox Chat data
### Data comes from Marcus Collins' scope script from .ss structured stream called ChatSessionUnified in Cosmos

# These library permit use of heteroskedastic robust standard errors 
library(lmtest)
library(sandwich)
# These are useful for working with strings below
library(tidyverse)
library(stringr)
#library(chron)




setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto/Data")
#setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto/Data")

mydata <- read.csv("chat-vol-duration-features.csv")
colnames(mydata)
#Keep only XBox USA Chat data
Xdata <- subset(mydata,Subject == "A&B - Xbox")
newdata <- subset(Xdata,CountryCode == "USA")


##################
# Combine and Format 
##################
#utils::View(newdata)
newdata$time <- str_c(newdata$Hour,"00",sep=":")
date_parts = t(as.data.frame(strsplit(as.character(newdata$Date),' ')))
newdata$fetcha <- date_parts[,1]
newdata$dtt <- paste(newdata$fetcha,newdata$time)
newdata$dt <- as.POSIXct(newdata$dtt, format = "%m/%d/%Y %H:%M")
newdata <-newdata[with(newdata, order(newdata$dt)), ]
newdata <- subset(newdata,newdata$dt >= "2016-07-27 00:00:00")
newdata <- subset(newdata,newdata$dt < "2017-01-08 00:00:00")


#####################
# Daily Plot
#####################
temp_data <- newdata[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"
daily$date_formatted <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#Trim partial days
daily <- subset(daily,daily$date_formatted >= "2016-07-27 00:00:00")
daily <- subset(daily,daily$date_formatted <= "2017-01-06 00:00:00")

#FULL SAMPLE

#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11", daily$date_formatted))
sky2<-which(grepl("2016-10-18", daily$date_formatted))
sky3<-which(grepl("2016-11-01", daily$date_formatted))
sky4<-which(grepl("2016-12-15", daily$date_formatted))
content1<-which(grepl("2016-11-17", daily$date_formatted))
#Plot of the raw daily data 
plot(daily$date_formatted, daily$ChatsInBlock,  main = "Xbox A&B Chat: Raw Data Aggregated to Daily Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(daily$date_formatted[sky1]),col = "red")
abline(v=as.numeric(daily$date_formatted[sky2]),col = "red")
abline(v=as.numeric(daily$date_formatted[sky3]),col = "red")
abline(v=as.numeric(daily$date_formatted[content1]),col = "blue")
abline(v=as.numeric(daily$date_formatted[sky4]),col = "red")

#POST OCT 1

#Take relevant subset
daily <- subset(daily,daily$date_formatted >= "2016-10-01 00:00:00")

#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11", daily$date_formatted))
sky2<-which(grepl("2016-10-18", daily$date_formatted))
sky3<-which(grepl("2016-11-01", daily$date_formatted))
sky4<-which(grepl("2016-12-15", daily$date_formatted))
content1<-which(grepl("2016-11-17", daily$date_formatted))
#Plot of the raw daily data 
plot(daily$date_formatted, daily$ChatsInBlock,  main = "Xbox A&B Chat: Raw Data Aggregated to Daily Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(daily$date_formatted[sky1]),col = "red")
abline(v=as.numeric(daily$date_formatted[sky2]),col = "red")
abline(v=as.numeric(daily$date_formatted[sky3]),col = "red")
abline(v=as.numeric(daily$date_formatted[content1]),col = "blue")
abline(v=as.numeric(daily$date_formatted[sky4]),col = "red")

##############################
#Create count variable for subsequent merge
##############################

daily$count <- seq(length(daily$ChatsInBlock))
daily$count2 <- daily$count^2
daily$count3 <- daily$count^3
daily$count4 <- daily$count^4
daily$count5 <- daily$count^5
daily$count6 <- daily$count^6
daily$count7 <- daily$count^7
daily$count8 <- daily$count^8
daily$count9 <- daily$count^9
daily$count10 <- daily$count^10
daily$dtdaily <- daily$date_formatted
colnames(daily)

#########################
# Merge daily and SUBSET of hourly data
#########################
vars <- c("dtdaily","count", "count2","count3", "count4","count5", "count6","count7","count8","count9", "count10")
temp <- daily[vars]

newdata1 <- subset(newdata,newdata$dt >= "2016-10-01 00:00:00")
# temp$dt is formatted by as.Date from line 45: daily$date_formatted <- as.Date(daily$daily_time) and line 99: daily$dt <- daily$date_formatted
# Must do the same for the newdata1 dataframe
newdata1$date_only<- as.Date(newdata1$dt, tz = "America/Los_Angeles")
m1 <- merge(temp,newdata1, by.x="dtdaily", by.y = "date_only")
m1 <- m1[with(m1, order(dt)), ]
utils::View(m1)

##########################
#Create dummies with correct initiation date for various treatments.
m1$sky0_10 <- ifelse(m1$dt >= "2016-10-11 00:00:00",1,0)
m1$sky10_30 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
m1$sky30_50 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
m1$content <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
#####
#Location in column where rollout occurs
sky1<-which(grepl("2016-10-11 00:00:00", m1$dt))
sky2<-which(grepl("2016-10-18 00:00:00", m1$dt))
sky3<-which(grepl("2016-11-01 00:00:00", m1$dt))
sky4<-which(grepl("2016-12-15 00:00:00", m1$dt))
content1<-which(grepl("2016-11-17 00:00:00", m1$dt))
###########################


#Plot of the raw data
plot(m1$dt, m1$ChatsInBlock,  main = "Raw Data Aggregated to Hour Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")

#stargazer will help with tables and display of data
library(stargazer)
#Table of summary statistics
stargazer(m1[c("ChatsInBlock")], type = "text", title="Descriptive statistics", digits=1, out="Summary.txt")
utils::View(m1)

m1$ln_ChatsInBlock<-log(m1$ChatsInBlock)

#Main specification of 8th order Polynomial levels
reg_poly5_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m1)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

#Main specification of 8th order Polynomial logs
reg_poly5_full = glm(ln_ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m1)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))


#Main specification of 8th order Polynomial hour by day levels, HOD*DOW
reg_poly5_full = glm(ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m1)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

#Main specification of 8th order Polynomial hour by day logs, HOD*DOW
reg_poly5_full = glm(ln_ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m1)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))


##### 8TH ORDER RESIDUAL PLOT
reg_hourly_resid_poly8 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m1)
m1$hourly_resid_poly8 <- resid(reg_hourly_resid_poly8)
plot(m1$dt, m1$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")

##### 8TH ORDER RESIDUAL PLOT
reg_hourly_resid_poly8 = glm(ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m1)
m1$hourly_resid_poly8 <- resid(reg_hourly_resid_poly8)
plot(m1$dt, m1$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, HOD*DOW, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")


#########################################
# No Christmas
######################################
m2<-subset(m1,m1$dt < "2016-12-24 00:00:00")
##### 8TH ORDER RESIDUAL PLOT Levels
reg_hourly_resid_poly8 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m2)
m2$hourly_resid_poly8 <- resid(reg_hourly_resid_poly8)
plot(m2$dt, m2$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")

##### 8TH ORDER RESIDUAL PLOT Levels, HOD*DOW
reg_hourly_resid_poly8 = glm(ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8, data=m2)
m2$hourly_resid_poly8 <- resid(reg_hourly_resid_poly8)
plot(m2$dt, m2$hourly_resid_poly8,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 8th Order polynomial, no dummies)")
abline(v=as.numeric(m1$dt[sky1]),col = "red")
abline(v=as.numeric(m1$dt[sky2]),col = "red")
abline(v=as.numeric(m1$dt[sky3]),col = "red")
abline(v=as.numeric(m1$dt[sky4]),col = "red")
abline(v=as.numeric(m1$dt[content1]),col = "blue")


#Main specification of 8th order Polynomial levels
reg_poly5_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m2)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

#Main specification of 8th order Polynomial logs
reg_poly5_full = glm(ln_ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m2)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))


#Main specification of 8th order Polynomial hour by day levels, HOD*DOW
reg_poly5_full = glm(ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m2)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

#Main specification of 8th order Polynomial hour by day logs, HOD*DOW
reg_poly5_full = glm(ln_ChatsInBlock ~ as.factor(Hour)*as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m2)
summary(reg_poly5_full)
coeftest(reg_poly5_full, vcov = vcovHC(reg_poly5_full, "HC1"))

##################################################


#Control for day of month fixed effects (A&B likely varies within month)
#m1$DOM <-as.numeric(days(m1$dt))
#m1$early_month <- ifelse(m1$DOM < 6,1,0)

#8th order Polynomial with early month
#reg_poly6_early_full = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + count8 + sky0_10 + sky10_30 + sky30_50 + sky50_90 +content, data=m2)
#summary(reg_poly6_early_full)
#coeftest(reg_poly6_early_full, vcov = vcovHC(reg_poly6_early_full, "HC1"))


######################################
##### Individual breaks with linear time trend on both sides
######################################
#Location in column where rollout occurs
#m1$sky0_10 <- ifelse(m1$dt >= "2016-10-11 00:00:00",1,0)
#m1$sky30_50 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
#m1$content <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
#m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
###########################

###############################
# Choose 5 days on either side of these breaks
#
# Third break
###############################

break3<-"2016-11-01 00:00:00"
break3_date <- "2016-11-01"
msky3 <-subset(m2,m2$dt >= "2016-10-25 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-11-08 00:00:00")
msky3$sky30_50 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d %H:%M:%S")
sky3<-which(grepl(break3, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
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


##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

msky3_linear$ln_MeanChatDuration<-log(msky3_linear$MeanChatDuration)
##### Linear regression
reg_linear_sky3 = glm(ln_MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy3
sky3_only<-which(grepl("2016-11-01 00:00:00", msky3_linear$dt))
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$dt[sky3_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy3
sky3_only<-which(grepl("2016-11-01 00:00:00", msky3_linear$dt))
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Chat Duration (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$dt[sky3_only]),col = "red")



###############################
# Choose one week on either side of these breaks
# #m1$sky10_30 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
###############################

# Second break
break2<-"2016-10-18 00:00:00"
break2_date<-"2016-10-18"
msky2 <-subset(m2,m2$dt >= "2016-10-13 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-10-25 00:00:00")
msky2$sky10_30 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
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
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky10_30, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky10_30 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky10_30 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

msky2_linear$ln_MeanChatDuration<-log(msky2_linear$MeanChatDuration)
##### Linear regression
reg_linear_sky3 = glm(ln_MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky10_30 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-10-18 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy2 Chat DUration
sky2_only<-which(grepl("2016-10-18 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Durations", xlab = "Date", ylab = "Residual Chat Duration (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


########################################

###############################
# Choose one week on either side of these breaks
# #m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
###############################

# Second break
break2<-"2016-12-15 00:00:00"
break2_date<-"2016-12-15"
msky2 <-subset(m2,m2$dt >= "2016-12-10 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-12-21 00:00:00")
msky2$sky10_30 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
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
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky50_90, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

msky2_linear$ln_MeanChatDuration<-log(msky2_linear$MeanChatDuration)
##### Linear regression
reg_linear_sky3 = glm(ln_MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy2 Chat DUration
sky2_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Durations", xlab = "Date", ylab = "Residual Chat Duration (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")

#Raw Data
plot(msky2_linear$dt, msky2_linear$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##################
#  Full week around Dec 15 for DOW FEs
##################

###############################
# Choose 5 days on either side of these breaks
# #m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
###############################

# Second break
break2<-"2016-12-15 00:00:00"
break2_date<-"2016-12-15"
msky2 <-subset(m2,m2$dt >= "2016-12-08 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-12-23 00:00:00")
msky2$sky10_30 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky2-1) 
daily$linear1 <- ifelse(daily$daily_time >= break2,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break2,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-12-23 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-12-23 00:00:00")

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
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour) + ChatDOW + linear1+ linear2+ sky50_90, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)  + ChatDOW +linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + ChatDOW +linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

msky2_linear$ln_MeanChatDuration<-log(msky2_linear$MeanChatDuration)
##### Linear regression
reg_linear_sky3 = glm(ln_MeanChatDuration ~ as.factor(Hour)  + ChatDOW +linear1+ linear2+ sky50_90 , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)  +ChatDOW + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy2 Chat DUration
sky2_only<-which(grepl("2016-12-15 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  + ChatDOW +linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Durations", xlab = "Date", ylab = "Residual Chat Duration (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")

#Raw Data
plot(msky2_linear$dt, msky2_linear$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")



#############################
#####  Content Change
# m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)
#############################


# Second break
break2<-"2016-11-17 00:00:00"
break2_date<-"2016-11-17"
msky2 <-subset(m2,m2$dt >= "2016-11-13 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-11-22 00:00:00")
msky2$sky10_30 <- ifelse(msky2$dt >= break2,1,0)


#Create linear time trend before and after
temp_data <- msky2[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d", tz = "America/Los_Angeles")
sky2<-which(grepl(break2_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky2-1) 
daily$linear1 <- ifelse(daily$daily_time >= break2,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break2,0,daily$linear2)

#Daylight savings time for this break makes things a bit odd to deal with so must drop extra dates
daily <-subset(daily,daily$daily_time < "2016-11-21 00:00:00")
msky2 <-subset(msky2,msky2$dt < "2016-11-21 00:00:00")

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
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour) +  linear1+ linear2+ content, data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)  + linear1+ linear2+ content , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)   +linear1+ linear2+ content , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

msky2_linear$ln_MeanChatDuration<-log(msky2_linear$MeanChatDuration)
##### Linear regression
reg_linear_sky3 = glm(ln_MeanChatDuration ~ as.factor(Hour)   +linear1+ linear2+ content , data=msky2_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear RESIDUAL PLOT on SKy2
sky2_only<-which(grepl("2016-11-17 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)   + linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")


##### Linear RESIDUAL PLOT on SKy2 Chat DUration
sky2_only<-which(grepl("2016-11-17 00:00:00", msky2_linear$dt))
reg_linear_sky3 = glm(MeanChatDuration ~ as.factor(Hour)  +linear1+ linear2, data=msky2_linear)
msky2_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky2_linear$dt, msky2_linear$resid_linear_sky3,  main = "Chat Durations", xlab = "Date", ylab = "Residual Chat Duration (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")

#Raw Data
plot(msky2_linear$dt, msky2_linear$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(msky2_linear$dt[sky2_only]),col = "red")

#Raw Data
plot(daily$daily_time, daily$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour")
abline(v=as.numeric(daily$daily_time["2016-11-17"]),col = "red")


#############################
#####  Changes in volume by week
#############################



