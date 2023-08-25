library(readr)
library(dplyr)
library(ggplot2)
options(scipen = 999)
activity <- read_csv("activity.csv")

changeIntervalToTime <- function(interval){
  format(strptime(substr(as.POSIXct(sprintf("%04.0f", interval), format= "%H%M"), 12,16), '%H:%M'), '%H:%M')
}

activity <- mutate(activity, time = changeIntervalToTime(interval))

stepsPerDay <- summarize(activity, totalSteps = sum(steps, na.rm = TRUE), .by = date )


hist(stepsPerDay$totalSteps, xlab = "Date of Activity", ylab = "Total Steps Taken", main = "Histogram of the total number of steps taken each day ")

p <- ggplot(stepsPerDay, aes(x=date, y=totalSteps)) + geom_bar(stat = "identity") + labs(x = "Date of Activity", y = "Total Steps Taken")
print(p)

medianSteps <- median(stepsPerDay$totalSteps)
meanSteps <- round(mean(stepsPerDay$totalSteps))


fiveMinuteInterval <- summarize(activity, averageSteps = mean(steps, na.rm = TRUE), .by = interval )
fiveMinuteInterval <- mutate(fiveMinuteInterval, time = changeIntervalToTime(interval))

with(fiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Time of Day", ylab = "Average Steps Taken"))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))


imputingData <- activity

for(i in 1:nrow(imputingData)){
  if(is.na(imputingData[i,]$steps)) {
    imputingItem <- imputingData[i, ]
    intervalItem <- subset(fiveMinuteInterval, interval == imputingItem$interval)
    imputingData[i,]$steps <- intervalItem$averageSteps
  }
}
filledDataStepsPerDay <- summarize(imputingData, totalSteps = sum(steps, na.rm = TRUE), .by = date )

hist(filledDataStepsPerDay$totalSteps, xlab = "Date of Activity", ylab = "Total Steps Taken", main = "Histogram of the total number of steps taken each day (Imputed data)")

activity$dayType <- ifelse(weekdays(activity$date, abbreviate = TRUE) %in% c("Sat", "Sun"), "weekends", "weekdays") 
activity$dayType <- as.factor(activity$dayType)

weekends <- subset(activity, activity$dayType == "weekends")
weekdays <- subset(activity, activity$dayType == "weekdays")

weekdaysfiveMinuteInterval <- summarize(weekdays, averageSteps = mean(steps, na.rm = TRUE), .by = interval )
weekendsfiveMinuteInterval <- summarize(weekends, averageSteps = mean(steps, na.rm = TRUE), .by = interval )

par(mar = c(4,4,2,2), mfrow = c(2,1))

with(weekdaysfiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Weekends", ylab = "Average steps", ylim = c(0, 250)))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))
with(weekendsfiveMinuteInterval, plot(interval, averageSteps, type = "l", xaxt = "n", xlab = "Weekdays", ylab = "Average steps", ylim = c(0, 250)))
axis(1, labels = c("00:00", "04:00", "08:00","12:00", "16:00", "20:00", "23:55"), at = c(0,400, 800, 1200, 1600, 2000, 2355))
