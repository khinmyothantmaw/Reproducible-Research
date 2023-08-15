library(readr)
library(dplyr)
activity <- read_csv("activity.csv")

// calculate total number of steps per day
stepsPerDay <- summarize(activity, totalSteps = sum(steps, na.rm = TRUE), .by = date )

changeIntervalToTime <- function(interval){
  format(strptime(substr(as.POSIXct(sprintf("%04.0f", interval), format= "%H%M"), 12,16), '%H:%M'), '%I:%M %p')
}

activity <- mutate(activity, time = changeIntervalToTime(interval))
