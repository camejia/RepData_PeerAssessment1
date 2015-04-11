setwd("~/GitHub/forks/rdpeng/RepData_PeerAssessment1")

unzip("activity.zip")
activity <- read.csv("activity.csv", header = TRUE)
activity$date <- as.Date(activity$date)

steps <- aggregate(steps ~ date, data = activity, FUN = sum)
histogram(steps)

library(ggplot2)
qplot(steps, data = steps, geom = "histogram") 
mean(steps$steps)
median(steps$steps)
