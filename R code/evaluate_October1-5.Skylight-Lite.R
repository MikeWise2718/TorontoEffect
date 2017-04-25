#library(strucchange)
library(plyr)
library(dplyr)
library(doBy)
library(reshape2)
library(tidyr)
#library(glmnet)
#library(MSwM)

setwd("C:/Users/jlariv/Documents/OneDrive for Business/Toronto")


mydata <- read.csv("October1-5.Skylight-Lite.csv")
colnames(mydata)

play_size=10000
mydata <- mydata[1:play_size,]

# Check nature of each variable
mydata2<-mydata[order(mydata$DialogID),]
View(mydata2)
mydata2<-mydata2[order(mydata2$Intent),]
#Do this for all colnames and track in COSMOS readme within C:\Users\jlariv\Documents\OneDrive for Business\Toronto



mydata$time <- t(as.data.frame(strsplit(mydata$QueryTime,'.')))
mydata$time <- as.POSIXlt(mydata$QueryTime)
rdate<-as.Date(mydata$QueryTime,"%Y-%m-%d")