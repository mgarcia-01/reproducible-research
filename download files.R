### Week 1 Project ###
library(data.table)
webURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destURL <- file.path(getwd(),paste("activity_monitoring_data",".csv",sep = ""))
actMonitor <- read.csv(destURL)
                 
download.file(webURL,method = "auto", destfile = destURL)