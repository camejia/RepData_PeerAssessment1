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

activity1 <- aggregate(steps ~ date, data = activity, FUN = sum)

library(ggplot2)
qplot(steps, data = activity1, geom = "histogram",
      main = "Total Number of Steps Taken per Day") 
mean(activity1$steps)
median(activity1$steps)

# Average Daily Activity Pattern ------------------------------------------

activity2 <- aggregate(steps ~ interval, data = activity, FUN = mean)
qplot(interval, steps, data = activity2, geom = "line",
      main = "Average Number of Steps Taken per Interval") 
activity2$interval[which.max(activity2$steps)]

# Imputing Missing Values -------------------------------------------------

ok <- complete.cases(activity)
sum(!ok)

activity3 <- merge(activity, activity2, by = "interval")
activity3$steps.x[is.na(activity3$steps.x)] <- activity3$steps.y[is.na(activity3$steps.x)]
names(activity3)[names(activity3) == "steps.x"] <- "steps"
activity3 <- subset(activity3, select = -steps.y)

activity4 <- aggregate(steps ~ date, data = activity3, FUN = sum)

qplot(steps, data = activity4, geom = "histogram",
      main = "Total Number of Steps Taken per Day\n(with imputed missing values)") 
mean(activity4$steps)
median(activity4$steps)

# Differences in Activity Patterns between Weekdays and Weekends ----------

activity3$daytype <- factor(weekdays(activity3$date) %in% c("Saturday", "Sunday"),
                            labels = c("weekday", "weekend"))
activity3$daytype <- weekdays(activity3$date) %in% c("Saturday", "Sunday")

activity5 <- aggregate(steps ~ interval + daytype, data = activity3, FUN = mean)
p <- qplot(interval, steps, data = activity5, geom = "line",
      main = "Average Number of Steps Taken per Interval") 
p + facet_wrap(~ daytype)


