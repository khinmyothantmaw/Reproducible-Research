library(readr)
library(dplyr)
library(ggplot2)
activity <- read_csv("activity.csv")

changeIntervalToTime <- function(interval){
  format(strptime(substr(as.POSIXct(sprintf("%04.0f", interval), format= "%H%M"), 12,16), '%H:%M'), '%H:%M')
}

activity <- mutate(activity, time = changeIntervalToTime(interval))

stepsPerDay <- summarize(activity, totalSteps = sum(steps, na.rm = TRUE), .by = date )

p <- ggplot(stepsPerDay, aes(x=date, y=totalSteps)) + geom_bar(stat = "identity") + labs(x = "Date of Activity", y = "Total Steps Taken")
print(p)

medianSteps <- median(stepsPerDay$totalSteps)
meanSteps <- round(mean(stepsPerDay$totalSteps))


fiveMinuteInterval <- summarize(activity, averageSteps = mean(steps, na.rm = TRUE), .by = interval )
fiveMinuteInterval <- mutate(fiveMinuteInterval, time = changeIntervalToTime(interval))

with(fiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Time of Day", ylab = "Average Steps Taken"))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))
missingData <- activity
for(i in 1:nrow(missingData)){
  if(is.na(missingData[i,]$steps)) {
    missingItem <- missingData[i, ]
    intervalItem <- subset(fiveMinuteInterval, interval == missingItem$interval)
    missingData[i,]$steps <- intervalItem$averageSteps
  }
}
filledDataStepsPerDay <- summarize(missingData, totalSteps = sum(steps, na.rm = TRUE), .by = date )

p <- ggplot(filledDataStepsPerDay, aes(x=date, y=totalSteps)) + geom_bar(stat = "identity") + labs(x = "Date of Activity", y = "Total Steps Taken")
print(p)

activity$dayType <- ifelse(weekdays(activity$date, abbreviate = TRUE) %in% c("Sat", "Sun"), "weekends", "weekdays") 

weekends <- subset(activity, activity$dayType == "weekends")
weekdays <- subset(activity, activity$dayType == "weekdays")

weekdaysfiveMinuteInterval <- summarize(weekdays, averageSteps = mean(steps, na.rm = TRUE), .by = interval )
weekendsfiveMinuteInterval <- summarize(weekends, averageSteps = mean(steps, na.rm = TRUE), .by = interval )

with(weekdaysfiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Weekends", ylab = "Average steps"))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))
with(weekendsfiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Weekdays", ylab = "Average steps"))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))