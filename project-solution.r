# Course 5 - Project 1
# Analyzing Activity Monitoring Data

# Loading libraries
library(dplyr)
library(ggplot2)

# Loading and preprocessing the data
# 1. Load the data
data_raw <- read.csv("data/activity.csv")
# 2. Removing NAs
data <- na.omit(data_raw)

# What is mean total number of steps taken per day?
# 1. Calculate the total number of steps taken per day
data_by_day <- group_by(data, date)
steps_by_day <- summarise(data_by_day, total = sum(steps))
print(steps_by_day)
# 2. Make a histogram of the total number of steps taken each day
hist(steps_by_day$total, main = "Total Number of Steps per Day", xlab = "Steps per Day")
# 3. Calculate and report the mean and median of the total number of steps taken per day
print(summary(steps_by_day))

# What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Organizing the data
steps_time_series <- aggregate(steps ~ interval, data, mean)
# Time series plot
plot(steps_time_series$interval, steps_time_series$steps, type='l', 
     main="Average number of steps taken all days", xlab="Interval", 
     ylab="Average number of steps")
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
# Find the row with maximum number of steps
max_steps <- which.max(steps_time_series$steps)
print(steps_time_series[max_steps,])

# Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset.
print(sum(is.na(data_raw)))
# 2. Devise a strategy for filling in all of the missing values in the dataset.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
data_new <- data_raw
# Replacing NA by zero
data_new[is.na(data_new)] <- 0
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
hist(data_new$steps, main="Histogram of total number of steps each day", 
     xlab="Total number of steps in a day")
print(mean(data_new$steps))
print(median(data_new$steps))

# Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
data_new['type_of_day'] <- weekdays(as.Date(data_new$date))
data_new$type_of_day[data_new$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_new$type_of_day[data_new$type_of_day != "weekend"] <- "weekday"
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
# Convert type_of_day from character to factor
data_new$type_of_day <- as.factor(data_new$type_of_day)

# Calculate average steps by interval across all days
plot_data <- aggregate(steps ~ interval + type_of_day, data_new, mean)

# creat a plot
qplot(interval, 
      steps, 
      data = plot_data, 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)