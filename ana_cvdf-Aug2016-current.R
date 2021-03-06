library(tidyverse,quietly=T,warn.conflicts=F)
library(lubridate,quietly=T,warn.conflicts=F)
library(scales,quietly=T,warn.conflicts=F)

smcdates <- c("2016-08-17/red/0-30%","2016-09-1/red/30-50%","2016-09-07/red/50-100%")
xabdates <- c("2016-10-11/red/0-10%","2016-10-18/red/10-30%","2016-11-01/red/30-50%","2016-11-17/blue/content","2016-12-15/red/50-90%")

addvlines <- function(vlines){
  abl <- function(l) {abline(v=as.POSIXct((l[[1]])),col=l[[2]])}
  sar <- strsplit(vlines,"/")
  jnk <- sapply(sar,abl)
}
sumandcoef <- function(fit){
  if (printsumm){ summary(fit) }
  if (printcoef){ coeftest(fit, vcov = vcovHC(fit, "HC1"))}
}
dailyplt <- function(x,y,mtit="",xlab="date",ylab=NULL,vlines=smcdates){
  ymx <- max(y) + 1000
  plot( x,y, main = mtit,xlab = xlab,ylab=ylab,ylim=c(0,ymx),pch=19,type="l" )
  if (!is.null(vlines)){
    addvlines(vlines)
  }
}

fname <- "data/chat-vol-duration-features.csv"
fname <- "../data/chat_volume_duration_features-Aug2016-current.csv"

tztz <- "Us/Pacific"

cvdf <- read.csv(fname)
cvdf$BlockBeginTime1 <- gsub("3/13/2016 2:","3/13/2016 3:",cvdf$BlockBeginTime)
cvdf$DateHour1 <- gsub("3/13/2016 2:","3/13/2016 3:",cvdf$DateHour)
cvdf$bbt <- as.POSIXct(cvdf$BlockBeginTime1,format="%m/%d/%Y %I:%M:%S %p",tz=tztz)
cvdf$dtd <- as.POSIXct(cvdf$Date,format="%m/%d/%Y %I:%M:%S %p",tz=tztz)
cvdf$dt <- as.POSIXct(cvdf$DateHour1,format="%m/%d/%Y %I:%M:%S %p",tz=tztz)
cvdf$dnum <- round(as.numeric(difftime(cvdf$dtd,as.POSIXct("2015-01-01",tz=tztz),"days")),0)

ddf <- cvdf %>% group_by(dnum) %>% summarize(dt=min(dt),cib = sum(ChatsInBlock),entrycount=n() )

ggdailyplt <- function(x,y,mtit="",xlab="date",ylab=NULL,vlines=NULL){
  if (is.null(ylab)){
    ylab <- substitute(y)
  }
  ymx <- max(y) + 1000
  ddf <- data.frame(x=x,y=y)
  gp <- ggplot(ddf) + geom_line(aes(x,y)) +
          xlab(xlab) + ylab(ylab) + ggtitle(mtit) +
          scale_x_datetime("Date",breaks = date_breaks("1 months"))
  if (!is.null(vlines)){
    # split the lines and convert to data.frame
    sar <- strsplit(vlines,"/")
    ldf <- data.frame(t(matrix(unlist(sar),length(sar[[1]]),length(sar))))
    names(ldf) <- c("dt","clr","lab")
    ldf$dt <- as.numeric(as.POSIXct(ldf$dt,tz=tztz))
    gp <- gp + geom_vline(xintercept=ldf$dt,color=ldf$clr)
  }
  return(gp)
}


dailyplt(ddf$dt,ddf$cib,"Chats In Block",ylab="Sum",vlines =smcdates )
ggdailyplt(ddf$dt,ddf$cib,"Chats In Block",ylab="Sum",vlines=smcdates)
ggdailyplt(ddf$dt,ddf$entrycount,"Entry Count",ylab="Count",vlines=smcdates)

