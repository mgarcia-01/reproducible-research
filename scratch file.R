## number 5 unused 
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