---
output:
  md_document:
    variant: markdown
title: 'PA1\_githubMarkdown'
---

Activity Report PA1 {#activity-report-pa1 .tabset .tabset-fade .tabset-pills}
-------------------

### Report

#### 1) Read and Process Data

``` {.r}
library(data.table)

webURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destURL <- file.path(getwd(),paste("activity_monitoring_data",".zip",sep = ""))
localFileURL <- file.path(getwd(),paste("activity",".csv",sep = ""))
download.file(webURL,destfile = destURL)

unzip(destURL)
#activityData <- as.data.frame(read.csv(localFileURL))
activityData <- read.csv(localFileURL, quote = "\"",stringsAsFactors = FALSE)
activityData$steps <- as.numeric(activityData$steps)
activityData$interval <- as.numeric(activityData$interval)
activityData$date <- as.Date(activityData$date,format = "%Y-%m-%d")
```

#### 2) Histogram of the total number of steps taken each day

``` {.r}
#   2) Histogram of the total number of steps taken each day
activityDataAg <- aggregate(activityData$steps,list(activityData$date),sum)
names(activityDataAg) <- c("date","totalSteps")
#activityDataAg <- activityDataAg[complete.cases(activityDataAg),]
hist(activityDataAg$totalSteps, xlab = "TotalSteps", breaks = 10)
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-2-1.png)

#### 3) Mean and median number of steps taken each day

``` {.r}
#3) Mean and median number of steps taken each day
stepSummary <- as.table(summary(activityDataAg$totalSteps))
stepsMedian <- stepSummary[3]
stepsMean <- stepSummary[4]
```

The total median steps are 10765 and the mean for the total steps taken
is 10766

#### 4) Time series plot of the average number of steps taken

``` {.r}
# 4) Time series plot of the average number of steps taken
# removed na's
actDataAg <- activityDataAg[complete.cases(activityDataAg),]
#activityTS= ts(rnorm(48), start=c(2293,6), frequency=1)
plot(actDataAg$date,actDataAg$totalSteps,type = "l", xlab = "date",ylab = "total steps")
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-4-1.png)

#### 5) The 5-minute interval that, on average, contains the maximum number of steps

``` {.r}
# 5   The 5-minute interval that, on average, contains the maximum number of steps
activity5min <- activityData[complete.cases(activityData),]
activity5min <- aggregate(activity5min$steps,list(activity5min$interval),mean)
names(activity5min) <- c("interval","avgsteps")
activity5min$avgsteps <- round(activity5min$avgsteps,1)
plot(activity5min$interval,activity5min$avgsteps,type = "l", xlab = "5min interval", ylab = "5min Avg Steps")
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-5-1.png)

The 5 minutes interval with the most steps is 835. The average number of
steps at this interval is 206.2.

###### 6 Code to describe and show a strategy for imputing missing datas

``` {.r}
#stepSummary <- as.table(summary(activityDataAg$totalSteps))
#rowsNA <- nrow(activityData[!complete.cases(activityData),])
```

There are 2304 days that have NA's

``` {.r}
#  6 Code to describe and show a strategy for imputing missing data

stepSummary <- as.table(summary(activityDataAg$totalSteps))

rowsNA <- nrow(activityData[!complete.cases(activityData),])



#library(zoo)
activityIntMerge <- merge(activityData,activity5min, by = "interval", all.x = TRUE)
activityIntMerge$steps <- ifelse(is.na(activityIntMerge$steps) == TRUE, activityIntMerge$avgsteps, activityIntMerge$steps)
activityIntMergeSum <- aggregate(activityIntMerge$steps,list(activityIntMerge$date),sum)
names(activityIntMergeSum) <- c("date","totalSteps")
activityIntMergeSum$date <-  as.Date(activityIntMergeSum$date,format = "%Y-%m-%d")
activityIntMergeSum$totalSteps <- as.numeric(activityIntMergeSum$totalSteps)
```

###### 7) Histogram of the total number of steps taken each day after missing values are imputed

``` {.r}
hist(activityIntMergeSum$totalSteps, main = "Total Steps NA filled with Interval Avg",xlab = "TotalSteps", breaks = 10 )
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-8-1.png)

``` {.r}
NAFillStepsSummary <- as.table(summary(activityIntMergeSum$totalSteps))
```

The strategy to fill na's included calculating the average steps per
interval. Then NA's were replaced with the average value in respect to
its interval. The mean after replacing the NA's is 10766 and the median
is 10766. The total daily steps and the average per inverval had no
significant changes after filling the NA's, as can be seen on the
charts.

``` {.r}
activityIntMergeAvg<- aggregate(activityIntMerge$steps,list(activityIntMerge$interval),mean)
names(activityIntMergeAvg) <- c("interval","avgSteps")
activityIntMergeAvg$avgSteps <- round(activityIntMergeAvg$avgSteps,1)
plot(activityIntMergeAvg$interval,activityIntMergeAvg$avgSteps,type = "l", main = "Interval after NA Filled",xlab = "5min interval", ylab = "5min Avg Steps")
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-9-1.png)

###### 8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` {.r}
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
mtext("Weekend vs. Weekday Avg Steps", side=3, outer=TRUE, line=-1) 
```

![](PA1_githubMarkdown_files/figure-markdown/unnamed-chunk-10-1.png)

### License and Copyright Notice

     Project: https://github.com/pimikeymike/reproducible-research
     Copyright 2017 Michael Garcia. All Rights Reserved.
     Inquiries mgar_datascience at protonmail.com
     Licensed under the Apache License, Version 2.0 (the "License");
     you may not use this file except in compliance with the License.
     You may obtain a copy of the License at
         http://www.apache.org/licenses/LICENSE-2.0
     Unless required by applicable law or agreed to in writing, software
     distributed under the License is distributed on an "AS IS" BASIS,
     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     See the License for the specific language governing permissions and
     limitations under the License.
     
     License Management through http://fossa.io/
