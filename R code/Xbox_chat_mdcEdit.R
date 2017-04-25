
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

setwd("C:/Users/macolli/Documents/SkylightCustomer/EconometricModel/")
#setwd("C:/Users/jlariv/OneDrive - Microsoft/Toronto")

mydata <- read.csv("xbox_chat_volume_features.csv")
colnames(mydata)
mydata$DelaySeqNo5 <-mydata$DelaySeqNo^5
mydata$DelaySeqNo6 <-mydata$DelaySeqNo^6
mydata$DelaySeqNo7 <-mydata$DelaySeqNo^7
mydata$DelaySeqNo8 <-mydata$DelaySeqNo^8
mydata$AB <- as.numeric(mydata$Subject == "A&B - Xbox")
reg1 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + as.factor(Subject)*SL1 +as.factor(Subject)*SL2 +as.factor(Subject)*SL3, data=mydata)
summary(reg1)
reg2 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + DelaySeqNo5 + DelaySeqNo6 + DelaySeqNo7 + DelaySeqNo8 +as.factor(Subject)*SL1 +as.factor(Subject)*SL2 +as.factor(Subject)*SL3, data=mydata)
summary(reg2)
reg3 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + DelaySeqNo5 + DelaySeqNo6 + DelaySeqNo7 + DelaySeqNo8 + AB*SL1 +AB*SL2 +AB*SL3, data=mydata)
summary(reg3)


newdata <- subset(mydata,Subject == "A&B - Xbox")
reg4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + DelaySeqNo5 + DelaySeqNo6 + DelaySeqNo7 + DelaySeqNo8 + AB*SL1 +AB*SL2 +AB*SL3, data=newdata)
summary(reg4)


#Histogrm of the data
newdata2 <- subset(newdata,SL2 == "True")
require(ggplot2)
#Must look at more narrow time window.
ggplot(newdata, aes(x=ChatsInBlock, fill=SL3)) + geom_density(alpha=.15)
ggplot(newdata2, aes(x=ChatsInBlock, fill=SL3)) + geom_density(alpha=.15)


#### Robust Errors
#install.packages("lmtest")
#install.packages("sandwich")
library(lmtest)
library(sandwich)

coeftest(reg4, vcov = vcovHC(reg4, "HC1"))
#NOTE: Using HC1 is consistent with STATA but HC3 is the vcocHC default in R 

###########
# Plot residuals with the polynomial line on it.
###########
reg5 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW), data=newdata)
reg6 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + DelaySeqNo5 + DelaySeqNo6 + DelaySeqNo7 + DelaySeqNo8, data=newdata)
summary(reg5)
newdata$reg5predicted <- predict(reg5,type = "response")
reg5resid <- resid(reg5)
newdata$reg5resid <- resid(reg5)
newdata$reg6resid <- resid(reg6)


# 9/1/2016 12:45:33 AM
newdata$dt <- as.POSIXct(newdata$BlockBeginTime, format = "%m/%d/%Y %I:%M:%S %p")
plot(newdata$dt, newdata$reg5resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual")
plot(newdata$dt, newdata$reg6resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual")

## Picture is much too chaotic to make sense of.  Lets take a subset of the data and re-estimate the model.
# 2016-11-02 00:20:16
newdata2 <- subset(newdata,newdata$dt >= "2016-10-01 00:00:00")
plot(newdata2$dt, newdata2$reg6resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual")


newdata3 <- subset(newdata2,newdata2$dt <= "2016-12-01 00:00:00")
reg8 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + DelaySeqNo + DelaySeqNo2 + DelaySeqNo3 + DelaySeqNo4 + DelaySeqNo5 + DelaySeqNo6 + DelaySeqNo7 + DelaySeqNo8, data=newdata3)
summary(reg8)
newdata3$reg8resid <- resid(reg8)
plot(newdata3$dt, newdata3$reg8resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual")

newdata4 <- subset(newdata3,newdata3$dt >= "2016-10-16 00:00:00")
newdata4 <- subset(newdata4,newdata4$dt <= "2016-10-19 00:00:00")
plot(newdata4$dt, newdata4$reg8resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual")
plot(newdata4$dt, newdata4$ChatsInBlock,  main = "Chat Initiations", xlab = "Date", ylab = "ChatsPerBlock")



## Aggregate to hourly data in order to increase the signal to noise ratio
newdata4 <- newdata2
ag4 <- aggregate(. ~ cut(newdata4$dt, "1 hour"), newdata4[setdiff(names(newdata4), "dt")], sum)
ag5 <- aggregate(. ~ cut(newdata4$dt, "1 hour"), newdata4[setdiff(names(newdata4), "dt")], mean)
# Cut creates weird variable names which need to be augmented.
colnames(ag4)[1] <- "hourly_time"
colnames(ag5)[1] <- "hourly_time"
#Take only sums from the first manipulation.  
temp_data <- ag4[c("hourly_time","ChatsInBlock")]
m1 <- merge(temp_data, ag5, by="hourly_time")
m1$ChatsInBlock.y <-NULL
colnames(m1)[colnames(m1)=="ChatsInBlock.x"] <- "ChatsInBlock"

## Aggregate to hourly data in order to increase the signal to noise ratio
ag4 <- aggregate(. ~ cut(newdata3$dt, "1 hour"), newdata3[setdiff(names(newdata3), "dt")], sum)
ag5 <- aggregate(. ~ cut(newdata3$dt, "1 hour"), newdata3[setdiff(names(newdata3), "dt")], mean)
# Cut creates weird variable names which need to be augmented.
colnames(ag4)[1] <- "hourly_time"
colnames(ag5)[1] <- "hourly_time"
#Take only sums from the first manipulation.  
temp_data <- ag4[c("hourly_time","ChatsInBlock")]
m1 <- merge(temp_data, ag5, by="hourly_time")
m1$ChatsInBlock.y <-NULL
colnames(m1)[colnames(m1)=="ChatsInBlock.x"] <- "ChatsInBlock"
m1$count <- seq(length(m1$ChatsInBlock))
m1$count2 <- m1$count^2
m1$count3 <- m1$count^3
m1$count4 <- m1$count^4
m1$count5 <- m1$count^5
m1$count6 <- m1$count^6
m1$count7 <- m1$count^7

#Something was added in the cut function
m1$SL1 <- m1$SL1-1
m1$SL2 <- m1$SL2-1
m1$SL3 <- m1$SL3-1 


#This code below doesn't work for me 
m1$hourly_time <- as.character(m1$hourly_time)
m1$dt <- as.POSIXct(m1$hourly_time, format = "%Y-%m-%d %H:%M:%S")

m1$dum1 <- ifelse(m1$dt >= "2016-10-11 00:00:00",1,0)
m1$dum2 <- ifelse(m1$dt >= "2016-10-18 00:00:00",1,0)
m1$dum3 <- ifelse(m1$dt >= "2016-11-01 00:00:00",1,0)
m1$dum4 <- ifelse(m1$dt >= "2016-11-17 00:00:00",1,0)

reg_hourly1 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + dum1 + dum2 + dum3 + dum4, data=m1)
summary(reg_hourly1)
coeftest(reg_hourly1, vcov = vcovHC(reg_hourly1, "HC1"))

plot(m1$dt, m1$ChatsInBlock,  main = "Raw Data Aggregated to Hour Level", xlab = "Date", ylab = "Chat Initiations")
abline(v=as.numeric(m1$dt[241]),col = "red")
temp <-241+(24*7)
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")

m1$hourly_resid_poly7dum <- resid(reg_hourly1)
plot(m1$dt, m1$hourly_resid_poly7dum, main = "Chat Initations", xlab = "Date", ylab = "residual (7th order poly + dummies)")
abline(v=as.numeric(m1$dt[241]),col = "red")
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")
abline(v=as.numeric(m1$dt[743+336]),col = "red")

reg_hourly_resid_poly7 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7, data=m1)
m1$hourly_resid_poly7 <- resid(reg_hourly_resid_poly7)
plot(m1$dt, m1$hourly_resid_poly7,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 7th Order polynomial)")
abline(v=as.numeric(m1$dt[241]),col = "red")
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")
abline(v=as.numeric(m1$dt[743+336]),col = "red")

#### NO POLYNOMIAL RESIDUAL PLOT
reg_hourly_resid = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW), data=m1)
m1$hourly_resid <- resid(reg_hourly_resid)
plot(m1$dt, m1$hourly_resid,  main = "Chat Initiations", xlab = "Date", ylab = "residual (excluding polynomial)")
abline(v=as.numeric(m1$dt[241]),col = "red")
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")

##### 4TH ORDER RESIDUAL PLOT
reg_hourly_resid_poly4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 , data=m1)
m1$hourly_resid_poly4 <- resid(reg_hourly_resid_poly4)
plot(m1$dt, m1$hourly_resid_poly4,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 4th Order polynomial)")
#Must figure out way to not have to hard code these numbers
abline(v=as.numeric(m1$dt[241]),col = "red")
temp <-241+(24*7)
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")
abline(v=as.numeric(m1$dt[743+336]),col = "red")

##### 4TH ORDER POLYNOMIAL REGRESSION
reg_hourly_poly4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4+dum1+dum2+dum3+dum4, data=m1)
summary(reg_hourly_poly4)
coeftest(reg_hourly_poly4, vcov = vcovHC(reg_hourly_poly4, "HC1"))


reg_hourly_poly4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4, data=m1)
plot(m1$dt, m1$hourly_resid_poly4,  main = "Chat Initiations", xlab = "Date", ylab = "residual (inc 4th Order polynomial)")
#Must figure out way to not have to hard code these numbers
abline(v=as.numeric(m1$dt[241]),col = "red")
temp <-241+(24*7)
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")

#Rollout of Xbox
m1$percent <- ifelse(m1$dt >= "2016-10-11 00:00:00",.1,0)
m1$percent <- ifelse(m1$dt >= "2016-10-18 00:00:00",.3,m1$percent)
m1$percent <- ifelse(m1$dt >= "2016-11-01 00:00:00",.5,m1$percent)
plot(m1$dt, m1$percent,  main = "Xbox A&B Rollout as percent of Total ", xlab = "Date", ylab = "Percent Surfaced with Skylight", ylim=c(0, 1))
abline(v=as.numeric(m1$dt[241]),col = "red")
abline(v=as.numeric(m1$dt[temp]),col = "red")
abline(v=as.numeric(m1$dt[743]),col = "red")

m2 <- subset(m1,m1$dt < "2016-12-01 00:00:00")
reg_hourly_poly4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4+dum1+dum2+dum3+dum4, data=m2)
summary(reg_hourly_poly4)
coeftest(reg_hourly_poly4, vcov = vcovHC(reg_hourly_poly4, "HC1"))

reg_hourly_poly7 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4 + count5 + count6 + count7 + dum1 + dum2 + dum3 + dum4, data=m2)
summary(reg_hourly_poly7)
coeftest(reg_hourly_poly7, vcov = vcovHC(reg_hourly_poly7, "HC1"))

m3 <- subset(m2,m2$dt > "2016-11-02 00:00:00")
reg_content_poly4 = glm(ChatsInBlock ~ as.factor(Hour) + as.factor(ChatDOW) + count + count2 + count3 + count4+dum4, data=m3)
summary(reg_content_poly4)
coeftest(reg_content_poly4, vcov = vcovHC(reg_content_poly4, "HC1"))




