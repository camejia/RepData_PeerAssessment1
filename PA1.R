# This is a "scratchpad" to test commands for the R markdown knitr file

setwd("~/GitHub/forks/rdpeng/RepData_PeerAssessment1")

# Loading and Preprocessing the Data --------------------------------------

# Link text: "Activity monitoring data"
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
              "repdata-data-activity.zip")
dateDownloaded <- date()

unzip("repdata-data-activity.zip")
activity <- read.csv("activity.csv", header = TRUE,
                     colClasses = c(NA, "Date", NA))

# Mean Total Number of Steps Taken per Day --------------------------------

totalStepsByDate <- aggregate(steps ~ date, data = activity, FUN = sum)
library(ggplot2)
qplot(steps, data = totalStepsByDate, geom = "histogram",
      main = "Total Number of Steps Taken per Day") 
meanTotalStepsByDate <- mean(totalStepsByDate$steps)
meanTotalStepsByDate <- median(totalStepsByDate$steps)

# Average Daily Activity Pattern ------------------------------------------

meanStepsByInterval <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(interval, steps, data = meanStepsByInterval, geom = "line",
      main = "Average Number of Steps Taken per Interval") 
meanStepsByInterval$interval[which.max(meanStepsByInterval$steps)]

# Imputing Missing Values -------------------------------------------------

ok <- complete.cases(activity)
incompleteCases <- sum(!ok)

imputed <- merge(activity, meanStepsByInterval, by = "interval")
imputed$steps.x[is.na(imputed$steps.x)] <- imputed$steps.y[is.na(imputed$steps.x)]
names(imputed)[names(imputed) == "steps.x"] <- "steps"
imputed <- subset(imputed, select = -steps.y)

impTotalStepsByDate <- aggregate(steps ~ date, data = imputed, FUN = sum)

qplot(steps, data = impTotalStepsByDate, geom = "histogram",
      main = "Total Number of Steps Taken per Day\n(with imputed missing values)") 
mean(impTotalStepsByDate$steps)
median(impTotalStepsByDate$steps)

# Differences in Activity Patterns between Weekdays and Weekends ----------

imputed$daytype <- factor(weekdays(imputed$date) %in% c("Saturday", "Sunday"),
                            labels = c("weekday", "weekend"))

impMeanStepsByInterval <- aggregate(steps ~ interval + daytype, data = imputed, FUN = mean)
p <- qplot(interval, steps, data = impMeanStepsByInterval, geom = "line",
      main = "Average Number of Steps Taken per Interval") 
p + facet_wrap(~ daytype)


