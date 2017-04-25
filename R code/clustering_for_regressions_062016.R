setwd("C:/Users/cunningham/Dropbox")
library(geosphere)

##Import collapsed data for entire one year
##The data gives the mean and sd speed, and mean score of data quality... 
##...(10 = imputed data, 20 = Real time data with less confidence 30= real time data with high confidence)
##data is at the segment level, one speed/sd/n variables for each hour between 5AM and 10PM
collapsed <- read.csv("DC_WMBreaks_Congestion/clustering/inrix_traffic_forclusters.csv")
##merge with segments for segment characteristics
segmentscsv <- read.csv("TMC data/inrix_segments.csv")
for(element in c("NB | NB|NORTHBOUND", "SB | SB|SOUTHBOUND", "EB | EB|EASTBOUND", "WB | WB|WESTBOUND",
                 "CLOCKWISE", "COUNTERCLOCKWISE", "I-", "US-")){
  string_vector=as.character(segmentscsv$direction)
  x <- grepl(element,string_vector)
  segmentscsv <- cbind(segmentscsv, (as.numeric(x)))
  colnames(segmentscsv)[ncol(segmentscsv)] <- element
}
colnames(segmentscsv)[(ncol(segmentscsv)-7):ncol(segmentscsv)] <- c("NB", "SB", "EB", "WB", "CW", "CCW", "IS", "US")
collapsed = merge(collapsed, segmentscsv, by="tmc")
for(i in 2:63){
  collapsed = collapsed[!is.na(collapsed[ ,i]), ]
}
collapsed <- collapsed[ ,'mscore' != substr( colnames(collapsed), 1, 6)]

##X <- data we want to cluster on; Y <- lat long for plotting
##column names 1198 503
##[1] "tmc"              "mspeed5"          "sdspeed5"         "mspeed6"          "sdspeed6"         "mspeed7"         
##[7] "sdspeed7"         "mspeed8"          "sdspeed8"         "mspeed9"          "sdspeed9"         "mspeed10"        
##[13] "sdspeed10"        "mspeed11"         "sdspeed11"        "mspeed12"         "sdspeed12"        "mspeed13"        
##[19] "sdspeed13"        "mspeed14"         "sdspeed14"        "mspeed15"         "sdspeed15"        "mspeed16"        
##[25] "sdspeed16"        "mspeed17"         "sdspeed17"        "mspeed18"         "sdspeed18"        "mspeed19"        
##[31] "sdspeed19"        "mspeed20"         "sdspeed20"        "mspeed21"         "sdspeed21"        "mspeed22"        
##[37] "sdspeed22"        "mspeed7_mspeed5"  "mspeed8_mspeed5"  "mspeed9_mspeed5"  "mspeed10_mspeed5" "mspeed15_mspeed5"
##[43] "mspeed16_mspeed5" "mspeed17_mspeed5" "mspeed18_mspeed5" "road"             "direction"        "intersection"    
##[49] "state"            "county"           "zip"              "start_latitude"   "start_longitude"  "end_latitude"    
##[55] "end_longitude"    "miles"            "road_order"       "mplat"            "mplong"           "Block"           
##[61] "CountyFIPS"       "CountyName"       "BlockGroup"       "Tract"            "NB"               "SB"              
##[67] "EB"               "WB"               "CW"               "CCW"              "IS"               "US"              

x <- collapsed[ ,c(2:45, 65:72)]  ##Cluster variables: mean/sd speed by hour, difference between mean and reference time for some hours
colnames(x) <- colnames(collapsed)[c(2:45, 65:72)]
z <- x
for(i in 1:ncol(x)){
  xmax <- max(x[ ,i])
  xmin <- min(x[ ,i])
  diff <- xmax-xmin
  x[ ,i] <- x[ ,i]/diff
}

##Clustering
for(i in 1:1){  ##10 tries for each cluster size so we can test with multiple groupings if we choose
  set.seed(i)
  ##set the number of clusters we want to use
  for(j in c(8,10)){ ##clusters of size 8, 10, 15)
    k <- j
    ##K means clustering
    cl <- kmeans(x, k)  ##k is the number of clusters, x is our set of clustering variables
    ##col.rainbow <- rainbow(k)
    ##Lat long plot with all segment midpoints, color by cluster
    ##setwd("DC_WMBreaks_Congestion/clustering")
    ##png(filename = "kcluster.png" ,width = 850,height=1150)
    ##plot(y[ ,1:2], col = cl$cluster, pch=20, cex=2.5)
    ##dev.off()
    ##Plot each individual cluster
    ##png(filename = "kclusters.png" ,width = 3400,height=2300) 
    ##par(mfrow=c(2,4))
    ##for(i in 1:k){
    ##  plot(y[cl$cluster==i,1:2], col = col.rainbow[i], pch=20, cex=2.5)
    #}
    ##dev.off()
    collapsed <- cbind(collapsed, cl$cluster) ##append data with cluster ids
    colnames(collapsed)[ncol(collapsed)] <- paste("cluster_", k, "_seed_", i, sep="")
  }
}

segment_clusters = data.frame(cbind(as.character(collapsed$tmc), collapsed$cluster_10_seed_1)) ##using k=10
segmentscsv$tmc <- as.character(segmentscsv$tmc) ##making sure segements aren't stored as factors
segment_clusters$X1 <- as.character(segment_clusters$X1) ##X1 is the segment variable
segmentscsv <- merge(segmentscsv, segment_clusters, by.x = "tmc", by.y="X1")
colnames(segmentscsv) <- c(colnames(segmentscsv)[1:(ncol(segmentscsv)-1)], "cluster_id")




breaks <- read.csv("DC_WMBreaks_Congestion/DCWATER/FOIAll.csv")

###dt variable - hours since 7/1/2014 at midnight
dt_rep <- numeric()
dt_end <- numeric()
dt_msu <- numeric()
dt_sd <- numeric()
for(i in 1:nrow(breaks)){
  x <- paste(as.character(breaks$last_date[i]), as.character(breaks$last_time[i]), sep=" ")
  x <- strptime(x, format="%Y/%m/%d %H:%M:%S")
  dt_msu<- c(dt_msu, ((as.numeric(x)/60/60-390052)))
}
for(i in 1:nrow(breaks)){
  x <- paste(as.character(breaks$Reported.Date[i]), as.character(breaks$Reported.Time[i]), sep=" ")
  x <- strptime(x, format="%m/%d/%Y %I:%M:%S %p")
  dt_rep <- c(dt_rep, ((as.numeric(x)/60/60-390052)))
  x <- paste(as.character(breaks$Actual.Completion.Date[i]), as.character(breaks$Actual.Completion.Time[i]), sep=" ")
  x <- strptime(x, format="%m/%d/%Y %I:%M:%S %p")
  dt_end <- c(dt_end, ((as.numeric(x)/60/60-390052)))
  x <- paste(as.character(breaks$Actual.Completion.Date[i]), "5:00:00 AM", sep=" ")
  x <- strptime(x, format="%m/%d/%Y %I:%M:%S %p")
  dt_sd <- c(dt_sd, ((as.numeric(x)/60/60-390052)))
}
breaks <- cbind(breaks, dt_rep, dt_end, dt_msu, dt_sd)
##colnames(breaks) <- c(colnames(breaks)[1:8], "material", "yearbuilt", "msuid", "severity", "latitude", "longitutde", colnames(breaks)[15:18])


##start date is the later of reported date and 12 hours before the work completion
##breaks$dt_start = breaks$dt_sd
for(i in 1:nrow(breaks)){
  if(!is.na(breaks$dt_end[i])){
    breaks$dt_start[i] <- max(breaks$dt_rep[i], breaks$dt_end[i]-12)
  }
}
##msu_used <- rep(0, nrow(breaks))
##for(i in 1:nrow(breaks)){
##  if(!is.na(breaks$dt_msu[i])){
##    if(!is.na(breaks$dt_end[i])){
##      if(breaks$dt_msu[i] < breaks$dt_end[i]){
##        breaks$dt_start[i] <- breaks$dt_msu[i]
##        msu_used[i] <- 1
##      }
##    }
##  }
##}

##clear unneccesary objects from workspace
rm(dt_end, dt_rep, i, x, y, j ,k)

colnames(breaks) <- c(colnames(breaks)[1:8], "material", "yearbuilt", "msuid", "severity", "latitude", "longitude", colnames(breaks[15:ncol(breaks)]))

Sys.time()
segment_affected <- data.frame()
cluster_affected <- data.frame()
for(i in 1:nrow(breaks)){ #for each break
  n = 0
  n2 = 0
  for(j in 1:nrow(segmentscsv)){  ##for each segment
    d <- distHaversine(c(breaks$longitude[i], breaks$latitude[i]), c(segmentscsv$mplong[j], segmentscsv$mplat[j]))
    ##d is the distance between the break and the segment midpoint   
    d<- d/1609.34
    if(d <= .25){  ##.25 miles will be our "treatment distance"
      n = n+1 ##for each row, we want one column for each segment. This sets the column number
      n2=n2+1 ##same as above but for clusters
      s <- as.character(segmentscsv$tmc[j])
      c <- as.character(segmentscsv$cluster_id[j])
      segment_affected[i, n] <- s
      cluster_affected[i, n2] <- c
      if(n2 > 1){
        for(w in 1:max(n2-1, 1)){
          if(cluster_affected[i,w] == cluster_affected[i,n2]){ ##Replace a cluster which has already been...
            ##...identified with an NA, so that we don't have repeats in different columns
            cluster_affected[i,n2] <- NA
            n2 = n2-1
          }
        }
      }
    }
  }
}
Sys.time()

######################
Sys.time()
segment_affected_.5 <- data.frame()
cluster_affected_.5 <- data.frame()
for(i in 1:nrow(breaks)){ #for each break
  n = 0
  n2 = 0
  for(j in 1:nrow(segmentscsv)){  ##for each segment
    d <- distHaversine(c(breaks$longitude[i], breaks$latitude[i]), c(segmentscsv$mplong[j], segmentscsv$mplat[j]))
    ##d is the distance between the break and the segment midpoint   
    d<- d/1609.34
    if(d <= .5){  
      n = n+1 ##for each row, we want one column for each segment. This sets the column number
      n2=n2+1 ##same as above but for clusters
      s <- as.character(segmentscsv$tmc[j])
      c <- as.character(segmentscsv$cluster_id[j])
      segment_affected_.5[i, n] <- s
      cluster_affected_.5[i, n2] <- c
      if(n2 > 1){
        for(w in 1:max(n2-1, 1)){
          if(cluster_affected_.5[i,w] == cluster_affected_.5[i,n2]){ ##Replace a cluster which has already been...
            ##...identified with an NA, so that we don't have repeats in different columns
            cluster_affected_.5[i,n2] <- NA
            n2 = n2-1
          }
        }
      }
    }
  }
}
Sys.time()
######################

######################
Sys.time()
segment_affected_.1 <- data.frame()
cluster_affected_.1 <- data.frame()
for(i in 1:nrow(breaks)){ #for each break
  n = 0
  n2 = 0
  for(j in 1:nrow(segmentscsv)){  ##for each segment
    d <- distHaversine(c(breaks$longitude[i], breaks$latitude[i]), c(segmentscsv$mplong[j], segmentscsv$mplat[j]))
    ##d is the distance between the break and the segment midpoint   
    d<- d/1609.34
    if(d <= .1){  
      n = n+1 ##for each row, we want one column for each segment. This sets the column number
      n2=n2+1 ##same as above but for clusters
      s <- as.character(segmentscsv$tmc[j])
      c <- as.character(segmentscsv$cluster_id[j])
      segment_affected_.1[i, n] <- s
      cluster_affected_.1[i, n2] <- c
      if(n2 > 1){
        for(w in 1:max(n2-1, 1)){
          if(cluster_affected_.1[i,w] == cluster_affected_.1[i,n2]){ ##Replace a cluster which has already been...
            ##...identified with an NA, so that we don't have repeats in different columns
            cluster_affected_.1[i,n2] <- NA
            n2 = n2-1
          }
        }
      }
    }
  }
}
Sys.time()
######################

######################
Sys.time()
segment_affected_.15 <- data.frame()
cluster_affected_.15 <- data.frame()
for(i in 1:nrow(breaks)){ #for each break
  n = 0
  n2 = 0
  d7 <- data.frame()
  for(j in 1:nrow(segmentscsv)){  ##for each segment
    d <- distHaversine(c(breaks$longitude[i], breaks$latitude[i]), c(segmentscsv$mplong[j], segmentscsv$mplat[j]))
    ##d7[j, 1] <- d/1609.34 
    ##d7[j, 2] <- as.character(segmentscsv$tmc[j])
    ##d7[j, 3] <- as.character(segmentscsv$cluster_id[j])
    ##d is the distance between the break and the segment midpoint   
    d<- d/1609.34
    if(d <= .15){  
      n = n+1 ##for each row, we want one column for each segment. This sets the column number
      n2=n2+1 ##same as above but for clusters
      s <- as.character(segmentscsv$tmc[j])
      c <- as.character(segmentscsv$cluster_id[j])
      segment_affected_.15[i, n] <- s
      cluster_affected_.15[i, n2] <- c
      if(n2 > 1){
        for(w in 1:max(n2-1, 1)){
          if(cluster_affected_.15[i,w] == cluster_affected_.15[i,n2]){ ##Replace a cluster which has already been...
            ##...identified with an NA, so that we don't have repeats in different columns
            cluster_affected_.15[i,n2] <- NA
            n2 = n2-1
          }
        }
      }
    }
  }
}
Sys.time()
######################

bcol <- ncol(breaks)
scol <- ncol(segment_affected)
ccol <- ncol(cluster_affected)

scol_.5  <- ncol(segment_affected_.5 )
ccol_.5  <- ncol(cluster_affected_.5 )

scol_.1  <- ncol(segment_affected_.1 )
ccol_.1  <- ncol(cluster_affected_.1 )


scol_.15  <- ncol(segment_affected_.15 )
ccol_.15  <- ncol(cluster_affected_.15 )

#########################

breaks_.25 <- cbind(breaks, segment_affected)
colnames(breaks_.25)[(bcol+1):(ncol(breaks_.25))] <- paste("C", colnames(breaks_.25)[(bcol+1):(ncol(breaks_.25))], sep="" )
breaks_.25 <- cbind(breaks_.25, cluster_affected) ##we now have a dataframe with break ids, ea ch segment and cluster affected

breaks_.5 <- cbind(breaks, segment_affected_.5)
colnames(breaks_.5)[(bcol+1):(ncol(breaks_.5))] <- paste("C", colnames(breaks_.5)[(bcol+1):(ncol(breaks_.5))], sep="" )
breaks_.5 <- cbind(breaks_.5, cluster_affected_.5) ##we now have a dataframe with break ids, ea ch segment and cluster affected

breaks_.1 <- cbind(breaks, segment_affected_.1)
colnames(breaks_.1)[(bcol+1):(ncol(breaks_.1))] <- paste("C", colnames(breaks_.1)[(bcol+1):(ncol(breaks_.1))], sep="" )
breaks_.1 <- cbind(breaks_.1, cluster_affected_.1) ##we now have a dataframe with break ids, ea ch segment and cluster affected

breaks_.15 <- cbind(breaks, segment_affected_.15)
colnames(breaks_.15)[(bcol+1):(ncol(breaks_.15))] <- paste("C", colnames(breaks_.15)[(bcol+1):(ncol(breaks_.15))], sep="" )
breaks_.15 <- cbind(breaks_.15, cluster_affected_.15) ##we now have a dataframe with break ids, ea ch segment and cluster affected




times <- c(-1000:8800) ##Hours since 7/1/14 index
times <- data.frame(times)
breaks_.25 <- breaks_.25[ ,c(1, 13, 14, 18, 21:ncol(breaks_.25))]
breaks_.5 <- breaks_.5[ ,c(1, 13, 14, 18, 21:ncol(breaks_.5))]
breaks_.1 <- breaks_.1[ ,c(1, 13, 14, 18, 21:ncol(breaks_.1))]
breaks_.15 <- breaks_.15[ ,c(1, 13, 14, 18, 21:ncol(breaks_.15))]

breaks2 <- reshape(breaks_.25, varying=6:(scol+5), v.names = "CV", direction="long") ##Want to make a segment level dataset with break ids as variables
breaks2 <- breaks2[!is.na(breaks2$CV), ]
breaks2$break_seg = breaks2$id + breaks2$time*.1
breaks2 <- breaks2[order(breaks2$break_seg), ]
breaks2 <- breaks2[,c(1:5, (ccol+7):ncol(breaks2))]

breaks2_cl <- reshape(breaks_.25, varying=(scol+6):ncol(breaks_.25), v.names = "V", direction="long") ##Same as above, but cluster level
breaks2_cl <- breaks2_cl[!is.na(breaks2_cl$V), ]
breaks2_cl$order = breaks2_cl$id + breaks2_cl$time*.1
breaks2_cl <- breaks2_cl[order(breaks2_cl$order), ]
breaks2_cl <- breaks2_cl[ ,c(1:5, (scol+6):ncol(breaks2_cl))]

breaks3 <- reshape(breaks_.5, varying=6:(scol_.5+5), v.names = "CV", direction="long") ##Want to make a segment level dataset with break ids as variables
breaks3 <- breaks3[!is.na(breaks3$CV), ]
breaks3$break_seg = breaks3$id + breaks3$time*.1
breaks3 <- breaks3[order(breaks3$break_seg), ]
breaks3 <- breaks3[,c(1:5, (ccol_.5+7):ncol(breaks3))]

breaks3_cl <- reshape(breaks_.5, varying=(scol_.5+6):ncol(breaks_.5), v.names = "V", direction="long") ##Same as above, but cluster level
breaks3_cl <- breaks3_cl[!is.na(breaks3_cl$V), ]
breaks3_cl$order = breaks3_cl$id + breaks3_cl$time*.1
breaks3_cl <- breaks3_cl[order(breaks3_cl$order), ]
breaks3_cl <- breaks3_cl[,c(1:5, (scol_.5+6):ncol(breaks3_cl))]

breaks4 <- reshape(breaks_.1, varying=6:(scol_.1+5), v.names = "CV", direction="long") ##Want to make a segment level dataset with break ids as variables
breaks4 <- breaks4[!is.na(breaks4$CV), ]
breaks4$break_seg = breaks4$id + breaks4$time*.1
breaks4 <- breaks4[order(breaks4$break_seg), ]
breaks4 <- breaks4[,c(1:5, (ccol_.1+7):ncol(breaks4))]

breaks4_cl <- reshape(breaks_.1, varying=(scol_.1+6):ncol(breaks_.1), v.names = "V", direction="long") ##Same as above, but cluster level
breaks4_cl <- breaks4_cl[!is.na(breaks4_cl$V), ]
breaks4_cl$order = breaks4_cl$id + breaks4_cl$time*.1
breaks4_cl <- breaks4_cl[order(breaks4_cl$order), ]
breaks4_cl <- breaks4_cl[,c(1:5, (scol_.1+6):ncol(breaks4_cl))]

breaks5 <- reshape(breaks_.15, varying=6:(scol_.15+5), v.names = "CV", direction="long") ##Want to make a segment level dataset with break ids as variables
breaks5 <- breaks5[!is.na(breaks5$CV), ]
breaks5$break_seg = breaks5$id + breaks5$time*.1
breaks5 <- breaks5[order(breaks5$break_seg), ]
breaks5 <- breaks5[,c(1:5, (ccol_.15+7):ncol(breaks5))]

breaks5_cl <- reshape(breaks_.15, varying=(scol_.15+6):ncol(breaks_.15), v.names = "V", direction="long") ##Same as above, but cluster level
breaks5_cl <- breaks5_cl[!is.na(breaks5_cl$V), ]
breaks5_cl$order = breaks5_cl$id + breaks5_cl$time*.1
breaks5_cl <- breaks5_cl[order(breaks5_cl$order), ]
breaks5_cl <- breaks5_cl[,c(1:5, (scol_.15+6):ncol(breaks5_cl))]

library(sqldf)
time_breaks <- sqldf("select * from times inner join breaks2
                     on (times.times > breaks2.dt_start and times.times<= breaks2.dt_end) ")
time_breaks_cl <- sqldf("select * from times inner join breaks2_cl
                        on (times.times > breaks2_cl.dt_start and times.times<= breaks2_cl.dt_end) ")
###^This block keeps only the times when each break was happening from the hour list - giving us a segment/hour & cluster/hour set
time_breaks_.5 <- sqldf("select * from times inner join breaks3
                        on (times.times > breaks3.dt_start and times.times<= breaks3.dt_end) ")
time_breaks_cl_.5 <- sqldf("select * from times inner join breaks3_cl
                           on (times.times > breaks3_cl.dt_start and times.times<= breaks3_cl.dt_end) ")

time_breaks_.1 <- sqldf("select * from times inner join breaks4
                        on (times.times > breaks4.dt_start and times.times<= breaks4.dt_end) ")
time_breaks_cl_.1 <- sqldf("select * from times inner join breaks4_cl
                           on (times.times > breaks4_cl.dt_start and times.times<= breaks4_cl.dt_end) ")

time_breaks_.15 <- sqldf("select * from times inner join breaks5
                         on (times.times > breaks5.dt_start and times.times<= breaks5.dt_end) ")
time_breaks_cl_.15 <- sqldf("select * from times inner join breaks5_cl
                            on (times.times > breaks5_cl.dt_start and times.times<= breaks5_cl.dt_end) ")

head(time_breaks)
head(time_breaks_.1)
head(time_breaks_.15)
head(time_breaks_.5)

head(time_breaks_cl)
head(time_breaks_cl_.1)
head(time_breaks_cl_.15)
head(time_breaks_cl_.5)

hnumber <- with(time_breaks, ave(times, times, FUN = seq_along)) ##reordering
time_breaks <- cbind(time_breaks, hnumber)
time_breaks <- reshape(time_breaks, v.names = colnames(time_breaks)[c(2:6, 8:9)], idvar=c("times", "CV"), direction="wide", timevar="hnumber")
hnumber_cl <- with(time_breaks_cl, ave(times, times, FUN = seq_along))
time_breaks_cl <- cbind(time_breaks_cl, hnumber_cl)
time_breaks_cl <- reshape(time_breaks_cl, v.names = colnames(time_breaks_cl)[c(2:7, 9:10)], idvar=c("times", "V"), direction="wide", timevar="hnumber_cl")

time_breaks <- time_breaks[ ,1:2]
time_breaks_cl <- time_breaks_cl[ ,1:2]
##
hnumber_.5 <- with(time_breaks_.5, ave(times, times, FUN = seq_along)) ##reordering
time_breaks_.5 <- cbind(time_breaks_.5, hnumber_.5)
time_breaks_.5 <- reshape(time_breaks_.5, v.names = colnames(time_breaks_.5)[c(2:6, 8:9)], idvar=c("times", "CV"), direction="wide", timevar="hnumber_.5")
hnumber_cl_.5 <- with(time_breaks_cl_.5, ave(times, times, FUN = seq_along))
time_breaks_cl_.5 <- cbind(time_breaks_cl_.5, hnumber_cl_.5)
time_breaks_cl_.5 <- reshape(time_breaks_cl_.5, v.names = colnames(time_breaks_cl_.5)[c(2:7, 9:10)], idvar=c("times", "V"), direction="wide", timevar="hnumber_cl_.5")

time_breaks_.5 <- time_breaks_.5[ ,1:2]
time_breaks_cl_.5 <- time_breaks_cl_.5[ ,1:2]
##
hnumber_.1 <- with(time_breaks_.1, ave(times, times, FUN = seq_along)) ##reordering
time_breaks_.1 <- cbind(time_breaks_.1, hnumber_.1)
time_breaks_.1 <- reshape(time_breaks_.1, v.names = colnames(time_breaks_.1)[c(2:6, 8:9)], idvar=c("times", "CV"), direction="wide", timevar="hnumber_.1")
hnumber_cl_.1 <- with(time_breaks_cl_.1, ave(times, times, FUN = seq_along))
time_breaks_cl_.1 <- cbind(time_breaks_cl_.1, hnumber_cl_.1)
time_breaks_cl_.1 <- reshape(time_breaks_cl_.1, v.names = colnames(time_breaks_cl_.1)[c(2:7, 9:10)], idvar=c("times", "V"), direction="wide", timevar="hnumber_cl_.1")

time_breaks_.1 <- time_breaks_.1[ ,1:2]
time_breaks_cl_.1 <- time_breaks_cl_.1[ ,1:2]
##
hnumber_.15 <- with(time_breaks_.15, ave(times, times, FUN = seq_along)) ##reordering
time_breaks_.15 <- cbind(time_breaks_.15, hnumber_.15)
time_breaks_.15 <- reshape(time_breaks_.15, v.names = colnames(time_breaks_.15)[c(2:6, 8:9)], idvar=c("times", "CV"), direction="wide", timevar="hnumber_.15")
hnumber_cl_.15 <- with(time_breaks_cl_.15, ave(times, times, FUN = seq_along))
time_breaks_cl_.15 <- cbind(time_breaks_cl_.15, hnumber_cl_.15)
time_breaks_cl_.15 <- reshape(time_breaks_cl_.15, v.names = colnames(time_breaks_cl_.15)[c(2:7, 9:10)], idvar=c("times", "V"), direction="wide", timevar="hnumber_cl_.15")

time_breaks_.15 <- time_breaks_.15[ ,1:2]
time_breaks_cl_.15 <- time_breaks_cl_.15[ ,1:2]

colnames(time_breaks)
colnames(time_breaks_cl)
colnames(time_breaks_.5)
colnames(time_breaks_cl_.5)
colnames(time_breaks_.1)
colnames(time_breaks_cl_.1)
colnames(time_breaks_.15)
colnames(time_breaks_cl_.15)
############################Start here

####
Sys.time()
bks <- data.frame() ##append each work order number for a given segment/hour cluster/hour
for(i in 1:nrow(time_breaks)){
  n=2
  bks[i, 1] = time_breaks$times[i]
  bks[i, 2] = time_breaks$CV[i]
  for(j in 1:nrow(breaks_.25)){
    if( (!is.na(breaks_.25$dt_end[j])) & (time_breaks$times[i] >= breaks_.25$dt_start[j]) & (time_breaks$times[i] <= breaks_.25$dt_end[j])){
      for(k in 6:ncol(breaks_.25)){
        if(!is.na(breaks_.25[j,k])){
          if(as.character(time_breaks$CV[i])==as.character(breaks_.25[j, k])){
            n=n+1
            bks[i, n] <- breaks_.25$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()
####
Sys.time()
bks_cl <- data.frame()
for(i in 1:nrow(time_breaks_cl)){
  n=2
  bks_cl[i, 1] = time_breaks_cl$times[i]
  bks_cl[i, 2] = time_breaks_cl$V[i]
  for(j in 1:nrow(breaks_.25)){
    if( (!is.na(breaks_.25$dt_end[j])) & (time_breaks_cl$times[i] >= breaks_.25$dt_start[j]) & (time_breaks_cl$times[i] <= breaks_.25$dt_end[j])){
      for(k in 6:ncol(breaks_.25)){
        if(!is.na(breaks_.25[j,k])){
          if(as.character(time_breaks_cl$V[i])==as.character(breaks_.25[j, k])){
            n=n+1
            bks_cl[i, n] <- breaks_.25$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()

####
Sys.time()
bks_.5 <- data.frame() ##append each work order number for a given segment/hour cluster/hour
for(i in 1:nrow(time_breaks_.5)){
  n=2
  bks_.5[i, 1] = time_breaks_.5$times[i]
  bks_.5[i, 2] = time_breaks_.5$CV[i]
  for(j in 1:nrow(breaks_.5)){
    if( (!is.na(breaks_.5$dt_end[j])) & (time_breaks_.5$times[i] >= breaks_.5$dt_start[j]) & (time_breaks_.5$times[i] <= breaks_.5$dt_end[j])){
      for(k in 6:ncol(breaks_.5)){
        if(!is.na(breaks_.5[j,k])){
          if(as.character(time_breaks_.5$CV[i])==as.character(breaks_.5[j, k])){
            n=n+1
            bks_.5[i, n] <- breaks_.5$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()
####
Sys.time()
bks_cl_.5 <- data.frame()
for(i in 1:nrow(time_breaks_cl_.5)){
  n=2
  bks_cl_.5[i, 1] = time_breaks_cl_.5$times[i]
  bks_cl_.5[i, 2] = time_breaks_cl_.5$V[i]
  for(j in 1:nrow(breaks_.5)){
    if( (!is.na(breaks_.5$dt_end[j])) & (time_breaks_cl_.5$times[i] >= breaks_.5$dt_start[j]) & (time_breaks_cl_.5$times[i] <= breaks_.5$dt_end[j])){
      for(k in 6:ncol(breaks_.5)){
        if(!is.na(breaks_.5[j,k])){
          if(as.character(time_breaks_cl_.5$V[i])==as.character(breaks_.5[j, k])){
            n=n+1
            bks_cl_.5[i, n] <- breaks_.5$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()

####
Sys.time()
bks_.1 <- data.frame() ##append each work order number for a given segment/hour cluster/hour
for(i in 1:nrow(time_breaks_.1)){
  n=2
  bks_.1[i, 1] = time_breaks_.1$times[i]
  bks_.1[i, 2] = time_breaks_.1$CV[i]
  for(j in 1:nrow(breaks_.1)){
    if( (!is.na(breaks_.1$dt_end[j])) & (time_breaks_.1$times[i] >= breaks_.1$dt_start[j]) & (time_breaks_.1$times[i] <= breaks_.1$dt_end[j])){
      for(k in 6:ncol(breaks_.1)){
        if(!is.na(breaks_.1[j,k])){
          if(as.character(time_breaks_.1$CV[i])==as.character(breaks_.1[j, k])){
            n=n+1
            bks_.1[i, n] <- breaks_.1$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()
####
Sys.time()
bks_cl_.1 <- data.frame()
for(i in 1:nrow(time_breaks_cl_.1)){
  n=2
  bks_cl_.1[i, 1] = time_breaks_cl_.1$times[i]
  bks_cl_.1[i, 2] = time_breaks_cl_.1$V[i]
  for(j in 1:nrow(breaks_.1)){
    if( (!is.na(breaks_.1$dt_end[j])) & (time_breaks_cl_.1$times[i] >= breaks_.1$dt_start[j]) & (time_breaks_cl_.1$times[i] <= breaks_.1$dt_end[j])){
      for(k in 6:ncol(breaks_.1)){
        if(!is.na(breaks_.1[j,k])){
          if(as.character(time_breaks_cl_.1$V[i])==as.character(breaks_.1[j, k])){
            n=n+1
            bks_cl_.1[i, n] <- breaks_.1$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()

####
Sys.time()
bks_.15 <- data.frame() ##append each work order number for a given segment/hour cluster/hour
for(i in 1:nrow(time_breaks_.15)){
  n=2
  bks_.15[i, 1] = time_breaks_.15$times[i]
  bks_.15[i, 2] = time_breaks_.15$CV[i]
  for(j in 1:nrow(breaks_.15)){
    if( (!is.na(breaks_.15$dt_end[j])) & (time_breaks_.15$times[i] >= breaks_.15$dt_start[j]) & (time_breaks_.15$times[i] <= breaks_.15$dt_end[j])){
      for(k in 6:ncol(breaks_.15)){
        if(!is.na(breaks_.15[j,k])){
          if(as.character(time_breaks_.15$CV[i])==as.character(breaks_.15[j, k])){
            n=n+1
            bks_.15[i, n] <- breaks_.15$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()
####
Sys.time()
bks_cl_.15 <- data.frame()
for(i in 1:nrow(time_breaks_cl_.15)){
  n=2
  bks_cl_.15[i, 1] = time_breaks_cl_.15$times[i]
  bks_cl_.15[i, 2] = time_breaks_cl_.15$V[i]
  for(j in 1:nrow(breaks_.15)){
    if( (!is.na(breaks_.15$dt_end[j])) & (time_breaks_cl_.15$times[i] >= breaks_.15$dt_start[j]) & (time_breaks_cl_.15$times[i] <= breaks_.15$dt_end[j])){
      for(k in 6:ncol(breaks_.15)){
        if(!is.na(breaks_.15[j,k])){
          if(as.character(time_breaks_cl_.15$V[i])==as.character(breaks_.15[j, k])){
            n=n+1
            bks_cl_.15[i, n] <- breaks_.15$Work.Order.Number[j]
          }
        }
      }
    }
  }
}
Sys.time()



###Hours where there is a break anywhere in DC
Sys.time()
bks_alldc <- data.frame(cbind(times, 0))
colnames(bks_alldc)[2] <- 'mbreak'
for(i in 1:nrow(bks_alldc)){
  for(j in 1:nrow(breaks)){
    if(bks_alldc$mbreak[i]==0){
      if(!is.na(breaks$dt_end[j])){
        if(bks_alldc$times[i] >= breaks$dt_start[j] & bks_alldc$times[i] <= breaks$dt_end[j]){
          bks_alldc$mbreak[i] <- 1
        }
      }
    }
  }
} 
Sys.time()

####End




###Map plot########
setwd("TMC data")
##import shapefiles with data for census, road segments, blockgorup boundaries
library(rgdal)
library(raster)
segments <- readOGR(dsn = "DC_TMC_v1503_Shapefile", "DC_TMC_v15031503_tmcs")
for(i in 1:nrow(segments@data)){
  for(j in 1:nrow(segmentscsv)){
    if(as.character(segments@data$Tmc[i])==as.character(segmentscsv$tmc[j])){
      segments@data$X2[i] <- as.character(segmentscsv$cluster_id[j])
    }
  }
}
##segments@data <- merge(segments@data, segment_clusters, by.x="Tmc", by.y = "X1")
setwd("C:/Users/cunningham/Dropbox")
col.set <- rainbow(10)
png(filename = "DC_WMBreaks_Congestion/clustering/kclusters_corrected_0708.png", width = 3200,height=3000)
par(mfrow=c(3,4))
for(i in 1:10){
  plot(segments[segments@data$X2==i, ], col="black", bg="white", xlim=c(-77.13, -76.90), ylim=c(38.76, 39.00))
  box(lwd=100, col="white")
  title(main=i, col.main="black", cex.main=8, line=-4, adj=0)
  index <- numeric()
  cindex <- colnames(breaks_.15) %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  for(w in 1:nrow(breaks_.15)){
    x <- numeric()
    for(j in 1:8){
      if(!is.na((breaks_.15[, cindex])[w, j])){
        x <- c(x, (breaks_.15[, cindex])[w, j])
      }
    }
    if(as.character(i) %in% x){
      index <- c(index, w)
    }
  }
  pts <- cbind(breaks_.15[index, c(1, 3:2)])
  points(pts[ ,2:3], pch=1, cex=3.6, col="black")
  ##text(pts[ ,2], pts[ ,3], pts[ ,1] , cex=0.6, pos=4, col="White")
}
plot(segments[segments@data$X2==1, ], col=col.set[1], bg="white", xlim=c(-77.13, -76.90), ylim=c(38.76, 39.00))
box(lwd=100, col="white")
title(main="ALL", col.main="black", cex.main=8, line=-4, adj=0)
for(i in 2:10){
  plot(segments[segments@data$X2==i, ], col=col.set[i], add=T, xlim=c(-77.13, -76.90), ylim=c(38.76, 39.00))
  box(lwd=100, col="white")
  title(main="ALL", col.main="black", cex.main=8, line=-4, adj=0)
}
plot(segments, bg="white", col="white", xlim=c(-77.13, -76.90), ylim=c(38.76, 39.00))
box(lwd=100, col="white")
##scalebar(1.609)
scalebar(4.8273, xy=c(-77.015, 38.88), type='bar', divs=6, label=("3 mi"), col="black", cex=8)
title(main="SCALE", col.main="black", cex.main=8, line=-4, adj=0)

dev.off()
###########

library(foreign)
setwd("L:/Project-Water_Infrastructure")
write.csv(bks, 'DC_WMBreaks_Congestion/segment_hour_breakids.csv', row.names=FALSE)
write.csv(bks_cl, 'DC_WMBreaks_Congestion/cluster_hour_breakids.csv', row.names=FALSE)
write.csv(bks_.5, 'DC_WMBreaks_Congestion/segment_hour_breakids_.5.csv', row.names=FALSE)
write.csv(bks_cl_.5, 'DC_WMBreaks_Congestion/cluster_hour_breakids_.5.csv', row.names=FALSE)
write.csv(bks_.1, 'DC_WMBreaks_Congestion/segment_hour_breakids_.1.csv', row.names=FALSE)
write.csv(bks_cl_.1, 'DC_WMBreaks_Congestion/cluster_hour_breakids_.1.csv', row.names=FALSE)
write.csv(bks_.15, 'DC_WMBreaks_Congestion/segment_hour_breakids_.15.csv', row.names=FALSE)
write.csv(bks_cl_.15, 'DC_WMBreaks_Congestion/cluster_hour_breakids_.15.csv', row.names=FALSE)
write.csv(bks_alldc, 'DC_WMBreaks_Congestion/hour_dcbreaks.csv', row.names=FALSE)


segment_clusters$X2 <- as.character(segment_clusters$X2)
colnames(segment_clusters) <- c("tmc", "cluster_id")
write.csv(segment_clusters, 'DC_WMBreaks_Congestion/segment_clusters.csv', row.names=FALSE)

###Use these csvs in stata file to merge with traffic data

