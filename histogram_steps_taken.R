#   2) Histogram of the total number of steps taken each day
#install.packages("astsa", dependencies = TRUE)
# if we do not remove na's then the total is skewed towards 0 
activityDataAg <- aggregate(activityData$steps,list(activityData$date),sum)
names(activityDataAg) <- c("date","totalSteps")
#activityDataAg <- activityDataAg[complete.cases(activityDataAg),]
hist(activityDataAg$totalSteps, xlab = "TotalSteps", breaks = 10)

##3) Mean and median number of steps taken each day


stepSummary <- as.table(summary(activityDataAg$totalSteps))
stepsMedian <- stepSummary[3]
stepsMean <- stepSummary[4]


# 4) Time series plot of the average number of steps taken
# removed na's
actDataAg <- activityDataAg[complete.cases(activityDataAg),]
#activityTS= ts(rnorm(48), start=c(2293,6), frequency=1)
plot(actDataAg$date,actDataAg$totalSteps,type = "l", xlab = "date",ylab = "total steps")


#  5)  The 5-minute interval that, on average, contains the maximum number of steps


activity5min <- activityData[complete.cases(activityData),]
activity5min <- aggregate(activity5min$steps,list(activity5min$interval),mean)
names(activity5min) <- c("interval","avgsteps")
activity5min$avgsteps <- round(activity5min$avgsteps,1)
plot(activity5min$interval,activity5min$avgsteps,type = "l", xlab = "5min interval", ylab = "5min Avg Steps")



#activityTS= ts(rnorm(48), start=c(2293,6), frequency=1)



#  6 Code to describe and show a strategy for imputing missing data

stepSummary <- as.table(summary(activityDataAg$totalSteps))

rowsNA <- nrow(activityData[!complete.cases(activityData),])

library(zoo)
  # use activity 5 min interval variable from number 5
#actDataMeanNA <- activityData[!complete.cases(activityData),]
#library(zoo)
activityIntMerge <- merge(activityData,activity5min, by = "interval", all.x = TRUE)
activityIntMerge$steps <- ifelse(is.na(activityIntMerge$steps) == TRUE, activityIntMerge$avgsteps, activityIntMerge$steps)
activityIntMergeSum <- aggregate(activityIntMerge$steps,list(activityIntMerge$date),sum)
names(activityIntMergeSum) <- c("date","totalSteps")
activityIntMergeSum$date <-  as.Date(activityIntMergeSum$date,format = "%Y-%m-%d")
activityIntMergeSum$totalSteps <- as.numeric(activityIntMergeSum$totalSteps)
### 7 Histogram of the total number of steps taken each day after missing values are imputed
hist(activityIntMergeSum$totalSteps, main = "Total Steps NA filled with Interval Avg",xlab = "TotalSteps", breaks = 10 )

NAFillStepsSummary <- as.table(summary(activityIntMergeSum$totalSteps))



activityIntMergeAvg<- aggregate(activityIntMerge$steps,list(activityIntMerge$interval),mean)
names(activityIntMergeAvg) <- c("interval","avgSteps")
activityIntMergeAvg$avgSteps <- round(activityIntMergeAvg$avgSteps,1)
plot(activityIntMergeAvg$interval,activityIntMergeAvg$avgSteps,type = "l", main = "Interval after NA Filled",xlab = "5min interval", ylab = "5min Avg Steps")




###### 8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

activityData$wkdy <- weekdays(activityData$date)
activityData$weektype <- ifelse((activityData$wkdy == "Saturday" | activityData$wkdy == "Sunday"), "Weekend", "Weekday" )
activityWk <- activityData[complete.cases(activityData),]
activityWkend <- aggregate(activityWk[which(activityWk$weektype =="Weekend"),"steps"]
                           , list(activityWk[which(activityWk$weektype =="Weekend"),"interval"]),mean)
names(activityWkend) <- c("interval","avgSteps")
activityWkend$interval <- as.integer(activityWkend$interval)
activityWkend$avgSteps <- round(as.numeric(activityWkend$avgSteps),1)

activityWkdy <- aggregate(activityWk[which(activityWk$weektype =="Weekday"),"steps"]
                           , list(activityWk[which(activityWk$weektype =="Weekday"),"interval"]),mean)
names(activityWkdy ) <- c("interval","avgSteps")
activityWkdy$interval <- as.integer(activityWkdy$interval)
activityWkdy$avgSteps <- round(as.numeric(activityWkdy$avgSteps),1)
par(mfrow=c(2,1))
#par(mfcol=c(2,1))
plot(activityWkdy$interval, activityWkdy$avgSteps, type = "l", xlab = "", ylab = "WkDay Avg Steps")
plot(activityWkend$interval, activityWkend$avgSteps, type = "l",xlab = "interval", ylab = "WkEnd Avg Steps")
mtext("Weekend vs. Weekday Avg Steps", side=3, outer=TRUE, line=-3) 


#############################################



hist(activityNAfill$avgSteps, xlab = "MeanSteps", breaks = 10)


activityDataMean <- aggregate(activityData$steps,list(activityData$date),mean)
names(activityDataMean) <- c("date","meanSteps")
hist(activityDataMean$meanSteps, xlab = "Mean Steps with NA", breaks = 10)






#activityDataAg[which(is.na(activityDataAg$totalSteps) == TRUE),]
activityDataMerge <- merge(activityDataAg, activityNAfill, by = "date", all.x = TRUE)
activityDataMerge$date <- as.Date(activityDataMerge$date,format = "%Y-%m-%d")
activityDataMerge$totalSteps <- as.numeric(activityDataMerge$totalSteps)
activityDataMerge$avgSteps <- as.numeric(activityDataMerge$avgSteps)

#activity

activityDataFull <- merge(activityData,activityDataMerge, by = "date")
activityDataFull$steps <- as.numeric(activityData$steps)
activityDataFull$interval <- as.numeric(activityData$interval)
activityDataFull$date <- as.Date(activityData$date,format = "%Y-%m-%d")

head(activityDataFull[which(activityDataFull == "2012-11-09"),],10)
#activitySumFillNA <- 

hist(activityNAfill$avgSteps, xlab = "MeanSteps", breaks = 20)

#activityMatch <- match(activityData[which(is.na(activityData$steps) == TRUE),],activityNAfill)
##activityData<- as.data.table(activityData)

head(activityData[which(activityData$date != "2012-10-08" & activityData$date != "2012-10-01" & !complete.cases(activityData)),],100)
head(activityData[which(activityData$date == "2012-11-01" & !complete.cases(activityData)),],100)


unique(activityData[which(is.na(activityData) == TRUE),"date"])
# [1] "2012-10-01" "2012-10-08" "2012-11-01" "2012-11-04" "2012-11-09" "2012-11-10" "2012-11-14" "2012-11-30"
activityData$steps[is.na(activityData$steps)] <- round(mean(activityData$steps, na.rm = TRUE))

for(i in 1:ncol(activityData)){
  activityData[is.na(activityData[,i]), i] <- mean(activityData[,i], na.rm = TRUE)
}


####



###### other notes

actDataAg$date = as.Date(actDataAg$date,format = "%y-%m-%d")
plot(actDataAg$totalSteps~as.Date(actDataAg$date,"%y-%m-%d"),type="l",
     xlab="Date",ylab="Total Steps")


activityTSdd<-data.frame(activityDate=seq(as.Date("1999-01-01"), as.Date("2014-01-10"), by="6 mon"),
  salpr = cumsum(rnorm(31))
)



ggplot(data=actDataAg, aes(x=actDataAg$date, y=actDataAg$totalSteps)) +
  geom_bar(stat="sum") + theme(axis.text.x = element_text(angle = 90, hjust = 1)
                               ,axis.title.x = element_text(face="bold", size=12)
                               ,legend.position="none"
  )+labs(x="Date",y="Total Steps")

geom_abline(data=actDataAg, aes(x=actDataAg$date, y=actDataAg$totalSteps))+ggplot()


ggplot(data=actDataAg, aes(x=actDataAg$date, y=actDataAg$totalSteps)) + geom_point()
  geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1)
                    ,axis.title.x = element_text(face="bold", size=12)
                    ,legend.position="none"
                   )+labs(x="Date",y="Total Steps")
