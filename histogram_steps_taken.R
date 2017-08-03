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
activity5min <- activityData[which(activityData$interval == 5),]
activity5min <- aggregate(activity5min$steps,list(activity5min$date),mean)
names(activity5min) <- c("date","avgsteps")
activity5min <- activity5min[complete.cases(activity5min),]
plot(activity5min$date,activity5min$avgsteps,type = "l", xlab = "date", ylab = "5min Avg Steps")

activity5min[which(activity5min$avgsteps == max(activity5min$avgsteps)),"date"]

#activityTS= ts(rnorm(48), start=c(2293,6), frequency=1)



#  6 Code to describe and show a strategy for imputing missing data











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
