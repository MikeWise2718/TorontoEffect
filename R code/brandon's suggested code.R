dt <- c("8/5/2016 12:34:56 AM", "9/8/2015 11:50:01 PM", "11/5/2016 1:25:15 PM")
id <- c(1,2,3)
df <- data.frame(cbind(id, dt))
df$dt <- as.character(df$dt)
dtparts <-  t(as.data.frame(strsplit(df$dt,' ')))
row.names(dtparts) = NULL
dtparts[,2] <- paste(dtparts[,2], dtparts[,3], sep = " ")
date <- dtparts[,1]
time <- dtparts[,2]
df$date <- date
df$time <- time
df$date <- strptime(df$date,format='%m/%d/%Y')
df$time <- strptime(df$time,format='%I:%M:%S %p')
df$dt = strptime(df$dt,format='%m/%d/%Y %I:%M:%S %p')