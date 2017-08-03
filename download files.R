### Week 1 Project ###
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

  ## works only if the file has been opened in excel or default program is excel
#activityData$date <- as.Date(activityData$date,format = "%m/%d/%y",  origin = "1904-01-01")







lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)
#Sys.getlocale()
#[1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"
#Sys.setlocale("LC_TIME", "en_US.UTF-8")