#   2) Histogram of the total number of steps taken each day
#install.packages("astsa", dependencies = TRUE)
# if we do not remove na's then the total is skewed towards 0 
activityDataAg <- aggregate(activityData$steps,list(activityData$date),sum)
names(activityDataAg) <- c("date","totalSteps")
#activityDataAg <- activityDataAg[complete.cases(activityDataAg),]
hist(activityDataAg$totalSteps, xlab = "TotalSteps", breaks = 20)

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
activityMean <- aggregate(activityData$steps,list(activityData$date),mean)
names(activityMean) <- c("date","avgSteps")
actDataMean <- activityMean[complete.cases(activityMean),]
actDataMeanNA <- activityMean[!complete.cases(activityMean),]

activityNAfill <- na.locf(activityMean[order(activityMean$date, decreasing = TRUE),], fromLast = TRUE)
activityNAfill <- activityNAfill[order(activityNAfill$date,decreasing = FALSE),]
activityNAfill$avgSteps <- as.numeric(activityNAfill$avgSteps)
activityNAfill$date <- as.Date(activityNAfill$date,format = "%Y-%m-%d")

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
