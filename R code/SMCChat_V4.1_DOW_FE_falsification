### XBox Chat data
### Data comes from Marcus Collins' scope script from .ss structured stream called ChatSessionUnified in Cosmos

# These library permit use of heteroskedastic robust standard errors 
library(lmtest)
library(sandwich)
# These are useful for working with strings below
library(tidyverse)
library(stringr)
#library(chron)
library(plyr)
library(dplyr)




#setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto/Data")
setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto/Data")

mydata <- read.csv("chat_volume_duration_features-Aug2016-current.csv")
colnames(mydata)
unique(mydata$CountryCode)
unique(mydata$Subject)
#Keep only XBox USA Chat data
newdata <- subset(mydata,CountryCode == "USA")
newdata <- subset(newdata,IsSMC == "True")


#newdata <- subset(newdata,Subject == "A&B - Xbox")
#newdata <- subset(newdata,ProductFamily == "XBOX")
#newdata <- subset(newdata,IsSMC == "True")
#newdata <- subset(newdata,virtualAgent == "False")
#newdata <- subset(newdata,virtualAgent == "True")





##################
# Combine and Format 
##################
#utils::View(newdata)
newdata$time <- str_c(newdata$Hour,"00",sep=":")
date_parts = t(as.data.frame(strsplit(as.character(newdata$Date),' ')))
newdata$fetcha <- date_parts[,1]
newdata$dtt <- paste(newdata$fetcha,newdata$time)
newdata$dt <- as.POSIXct(newdata$dtt, format = "%m/%d/%Y %H:%M")
#newdata$dt <- as.POSIXct(newdata$DateHour, format = "%m/%d/%Y %H:%M:%S %I")
newdata <-newdata[with(newdata, order(newdata$dt)), ]
newdata <- subset(newdata,newdata$dt >= "2016-07-15 00:00:00")
newdata <- subset(newdata,newdata$dt < "2016-10-01 00:00:00")


#sumstats<-newdata %>%   group_by(Subject) %>%   summarise_each (funs(mean, min, max, sd), mean_Chats_per_hour = ChatsInBlock)


#####################
# Hourly Aggregation Plot
#####################
temp_data <- newdata[c("dt","ChatsInBlock")]
ag_hour <- aggregate(. ~ cut(temp_data$dt, "1 hour"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(ag_hour)[1] <- "daily_time"
ag_hour$date_formatted <- as.Date(ag_hour$daily_time, tz = "America/Los_Angeles")


#FULL SAMPLE

#8/17 30%
#9/1 50%
#9/7 100% 

#Location in column where rollout occurs
sky1<-which(grepl("2016-08-17", ag_hour$date_formatted))
sky2<-which(grepl("2016-09-01", ag_hour$date_formatted))
sky3<-which(grepl("2016-09-07", ag_hour$date_formatted))
#Plot of the raw daily data 
plot(ag_hour$date_formatted, ag_hour$ChatsInBlock,  main = "SMC Chat: Raw Data at Hourly Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(ag_hour$date_formatted[sky1]),col = "red")
abline(v=as.numeric(ag_hour$date_formatted[sky2]),col = "red")
abline(v=as.numeric(ag_hour$date_formatted[sky3]),col = "red")

###
#  THESE FIGURES ARE WRONG UNCLEAR WHAT IS HAPPENING.  It looks like the plot dimensions don't readjust 
###
plot3 <- subset(ag_hour,ag_hour$date_formatted >= "2016-09-02 00:00:00")
plot3 <- subset(plot3,plot3$date_formatted < "2016-09-12 00:00:00")
#Location in column where rollout occurs
sky3<-which(grepl("2016-09-07", plot3$date_formatted))
#Plot of the raw daily data 
plot(plot3$ChatsInBlock,  main = "SMC Chat: Raw Data at Hourly Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(plot3$date_formatted[sky3]),col = "red")



#####################
# Daily Aggregation Plot: Keeping all the names identical
#####################

ag_hour <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(ag_hour)[1] <- "daily_time"
ag_hour$date_formatted <- as.Date(ag_hour$daily_time, tz = "America/Los_Angeles")


#FULL SAMPLE

#8/17 30%
#9/1 50%
#9/7 100% 

#Location in column where rollout occurs
sky1<-which(grepl("2016-08-17", ag_hour$date_formatted))
sky2<-which(grepl("2016-09-01", ag_hour$date_formatted))
sky3<-which(grepl("2016-09-07", ag_hour$date_formatted))
#Plot of the raw daily data 
plot(ag_hour$date_formatted, ag_hour$ChatsInBlock,  main = "SMC Chat: Raw Data at Daily Level", xlab = "Date", ylab = "Chat Initiations", pch=19)
abline(v=as.numeric(ag_hour$date_formatted[sky1]),col = "red")
abline(v=as.numeric(ag_hour$date_formatted[sky2]),col = "red")
abline(v=as.numeric(ag_hour$date_formatted[sky3]),col = "red")


##############################
#Create count variable for subsequent merge
##############################
daily <- ag_hour
daily$count <- seq(length(daily$ChatsInBlock))
colnames(daily)

temp_data <- newdata[c("dt","ChatsInBlock")]
ag_hour <- aggregate(. ~ cut(temp_data$dt, "1 hour"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(ag_hour)[1] <- "hourly_time"
ag_hour$date_formatted <- as.Date(ag_hour$hourly_time, tz = "America/Los_Angeles")

#########################
# Merge daily and SUBSET of hourly data
#########################

daily <- daily[c("date_formatted","count")]
m1 <- merge(daily,ag_hour, by="date_formatted")
m1 <- m1[with(m1, order(hourly_time)), ]
utils::View(m1)
m1$dt <- m1$date_formatted

##########################
#Create dummies with correct initiation date for various treatments.
m1$sky0_30 <- ifelse(m1$dt >= "2016-08-17 00:00:00",1,0)
m1$sky30_50 <- ifelse(m1$dt >= "2016-09-01 00:00:00",1,0)
m1$sky50_100 <- ifelse(m1$dt >= "2016-09-07 00:00:00",1,0)
#####
#Location in column where rollout occurs
sky1<-which(grepl("2016-08-17 00:00:00", m1$dt))
sky2<-which(grepl("2016-09-01 00:00:00", m1$dt))
sky3<-which(grepl("2016-09-07 00:00:00", m1$dt))
###########################

#m2 <-aggregate(.~dt,data=temp_data,sum)
#m2<-ddply(m1,.(as.numeric(m1$ChatsInBlock),colSums))
#temp_data <- newdata[c("dt","ChatsInBlock")]
#daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
#colnames(daily)[1] <- "daily_time"
#daily$date_formatted <- as.Date(daily$daily_time, tz = "America/Los_Angeles")


#stargazer will help with tables and display of data
library(stargazer)
#Table of summary statistics
stargazer(m1[c("ChatsInBlock")], type = "text", title="Descriptive statistics", digits=1, out="Summary.txt")
#utils::View(m1)

m1$DOW <- weekdays(m1$dt)
m1$Hour <- hour(m1$hourly_time)
m1$ln_ChatsInBlock<-log(m1$ChatsInBlock)

m2 <- m1
m2$count2 <- m2$count^2
m2$count3 <- m2$count^3
m2$count4 <- m2$count^4
m2$count5 <- m2$count^5
m2$count6 <- m2$count^6
m2$count7 <- m2$count^7
m2$count8 <- m2$count^8



#Poly specification of 8th order Polynomial levels
reg_poly8_full = glm(ChatsInBlock ~ as.factor(Hour)*as.factor(DOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_30 + sky30_50 + sky50_100, data=m2)
summary(reg_poly8_full)
coeftest(reg_poly8_full, vcov = vcovHC(reg_poly8_full, "HC1"))

#Poly specification of 8th order Polynomial levels
reg_poly8_full = glm(ln_ChatsInBlock ~ as.factor(Hour)*as.factor(DOW) + count + count2 + count3 + count4 + count5 + count6 + count7 +count8 + sky0_30 + sky30_50 + sky50_100, data=m2)
summary(reg_poly8_full)
coeftest(reg_poly8_full, vcov = vcovHC(reg_poly8_full, "HC1"))



##################################################
###############################
# Choose 7 days on either side of these breaks
#
#Location in column where rollout occurs
#sky1<-which(grepl("2016-08-17 00:00:00", m1$dt))
#sky2<-which(grepl("2016-09-01 00:00:00", m1$dt))
#sky3<-which(grepl("2016-09-07 00:00:00", m1$dt))
###########################
# First break
###############################

fitted_DOW_HOD = glm(ChatsInBlock ~ as.factor(DOW)*as.factor(Hour), data=m1)
m1$SMCChat_DOW_HOD <- resid(fitted_DOW_HOD)


break3<-"2016-08-17 00:00:00"
break3_date <- "2016-08-17"
msky3 <-subset(m1,m1$dt >= "2016-08-10 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-08-24 00:00:00")
msky3$sky0_30 <- ifelse(msky3$dt >= break3,1,0)



#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"
daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#Raw Data
plot(msky3$hourly_time, msky3$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour", pch=19)
abline(v=as.numeric(msky3$hourly_time[break3]),col = "red")

#Raw Data
plot(daily$daily_time, daily$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour", pch=19)
abline(v=as.numeric(daily$daily_time[break3_date]),col = "red")




#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky0_30 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky0_30, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky0_30, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-08-17 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")


###############################
# Choose one week on either side of these breaks
#sky2<-which(grepl("2016-09-01 00:00:00", m1$dt))
###############################



break3<-"2016-09-01 00:00:00"
break3_date <- "2016-09-01"
msky3 <-subset(m1,m1$dt >= "2016-08-26 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-09-06 00:00:00")
msky3$sky30_50 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky30_50, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky3_50, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-09-01 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")

######################################



break3<-"2016-09-07 00:00:00"
break3_date <- "2016-09-07"
msky3 <-subset(m1,m1$dt >= "2016-09-02 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-09-13 00:00:00")
msky3$sky50_100 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))


plot(msky3$hourly_time,msky3$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Raw Data)")
#plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3$hourly_time[break3]),col = "red")



plot(msky3$hourly_time,msky3$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Raw Data)")
abline(v=as.numeric(msky3$hourly_time[break3]),col = "red")


##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-09-01 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")


########################################
# Shorter Time Windows
###############################
# Choose one week on either side of these breaks
# #m1$sky50_90 <- ifelse(m1$dt >= "2016-12-15 00:00:00",1,0)
###############################


break3<-"2016-08-17 00:00:00"
break3_date <- "2016-08-17"
msky3 <-subset(m1,m1$dt >= "2016-08-12 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-08-22 00:00:00")
msky3$sky0_30 <- ifelse(msky3$dt >= break3,1,0)



#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"
daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#Raw Data
plot(msky3$hourly_time, msky3$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour", pch=19)
abline(v=as.numeric(msky3$hourly_time[break3]),col = "red")

#Raw Data
plot(daily$daily_time, daily$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "Chats/Hour", pch=19)
abline(v=as.numeric(daily$daily_time[break3_date]),col = "red")




#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky0_30 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky0_30, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky0_30, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-08-17 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")


###############################
# Choose one week on either side of these breaks
#sky2<-which(grepl("2016-09-01 00:00:00", m1$dt))
###############################



break3<-"2016-09-01 00:00:00"
break3_date <- "2016-09-01"
msky3 <-subset(m1,m1$dt >= "2016-08-26 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-09-05 00:00:00")
msky3$sky30_50 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky30_50 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky30_50, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky3_50, data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-09-01 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")

######################################



break3<-"2016-09-07 00:00:00"
break3_date <- "2016-09-07"
msky3 <-subset(m1,m1$dt >= "2016-09-02 00:00:00")
msky3 <-subset(msky3,msky3$dt < "2016-09-12 00:00:00")
msky3$sky50_100 <- ifelse(msky3$dt >= break3,1,0)


#Create linear time trend before and after
temp_data <- msky3[c("dt","ChatsInBlock")]
daily <- aggregate(. ~ cut(temp_data$dt, "1 day"), temp_data[setdiff(names(temp_data), "dt")], sum)
colnames(daily)[1] <- "daily_time"

daily$daily_time <- as.Date(daily$daily_time, tz = "America/Los_Angeles")
#daily$daily_time <- as.POSIXct(daily$daily_time, format = "%Y-%m-%d")
sky3<-which(grepl(break3_date, daily$daily_time))
#daily$date_formatted <- as.Date(daily$daily_time)
daily$linear1 <- seq(length(daily$ChatsInBlock))
daily$linear2 <- daily$linear1 -(sky3-1) 
daily$linear1 <- ifelse(daily$daily_time >= break3,0,daily$linear1)
daily$linear2 <- ifelse(daily$daily_time < break3,0,daily$linear2)


# Create subsets of data to merge
vars <- c("daily_time","linear1", "linear2")
temp <- daily[vars]

#Convert to a single date variable
msky3$date_only<- as.Date(msky3$dt, tz = "America/Los_Angeles")
utils::View(msky3)
temp$date_only<- as.Date(temp$daily_time, tz = "America/Los_Angeles")
msky3_linear <- merge(temp,msky3, by= "date_only")
msky3_linear <- msky3_linear[with(msky3_linear, order(dt)), ]


##### Linear regression
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ln_ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))

##### Linear regression
reg_linear_sky3 = glm(ChatsInBlock ~ as.factor(Hour)+ as.factor(DOW) + linear1+ linear2+ sky50_100 , data=msky3_linear)
summary(reg_linear_sky3)
coeftest(reg_linear_sky3, vcov = vcovHC(reg_linear_sky3, "HC1"))



##### Linear RESIDUAL PLOT on SKy1
sky3_only<-which(grepl("2016-09-01 00:00:00", msky3_linear$hourly_time))
reg_linear_sky3 = glm(SMCChat_DOW_HOD ~ linear1+ linear2, data=msky3_linear)
msky3_linear$resid_linear_sky3 <- resid(reg_linear_sky3)
plot(msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$dt, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
#plot(msky3_linear$hourly_time, msky3_linear$resid_linear_sky3,  main = "Chat Initiations", xlab = "Date", ylab = "Residual Live Chats/Hour (Unique Linear trends, no dummy)")
abline(v=as.numeric(msky3_linear$hourly_time[sky3_only]),col = "red")
