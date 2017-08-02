### Week 1 Project ###
library(data.table)

webURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destURL <- file.path(getwd(),paste("activity_monitoring_data",".zip",sep = ""))
localFileURL <- file.path(getwd(),paste("activity",".csv",sep = ""))
download.file(webURL,destfile = destURL)

unzip(destURL)
activityData <- as.data.frame(fread(localFileURL))
activityData$steps <- as.numeric(activityData$steps)
activityData$interval <- as.numeric(activityData$interval)
