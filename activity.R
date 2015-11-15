getwd()
setwd('./Data/reproducible')

activity <- read.csv('activity.csv', header=T, sep=",")
str(activity)
summary(activity)

library(dplyr)
library(tidyr)
library(ggplot2)
activity <- tbl_df(activity)
str(activity)
summary(activity)
is.factor(activity$date)

# Calculate the total number of steps taken per day
activity %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) 

# Make a histogram of the total number of steps taken each day
data1 <- activity %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) 

hist(data1$tot_steps, main="Total Number of Steps per Day", col="blue", 
     xlab="Steps", ylab="Count (1,000)")



# Calculate and report the mean and median of the total number of steps taken per day
activity %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) %>%
    print(n=61)


# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
data2 <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE))
data2 <- tbl_df(data2)
str(data2)
plot(data2$interval, data2$mean_steps, type="l", main="Time-series Plot",
     xlab="Interval", ylab="Average Steps")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
activity %>%
    group_by(interval) %>%
    summarise(max_step = max(mean(steps, na.rm = TRUE))) %>%
    summarise(himax_step = max(max_step, na.rm = TRUE)) 
    
              
# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(activity))
colSums(is.na(activity));
apply(is.na(activity),2,sum); 
sapply(activity, function(x) sum(is.na(x)))

summary(activity)

newdata <- na.omit(activity)
summary(newdata)


# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
activity2 <- transform(activity, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
colSums(is.na(activity2))
summary(activity2)

# Make a histogram of the total number of steps taken each day and Calculate 
data_imputed <- activity2 %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) 

hist(data_imputed$tot_steps, main="Total Number of Steps per Day", 
     col="red", xlab="Steps", ylab="Count(1,000)")

# and report the mean and median total number of steps taken per day. 
before_imp <- activity %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) 
    
after_imp <- activity2 %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) 

# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
summary(before_imp); summary(after_imp)

par(mfrow=c(1,2))
hist(data1$tot_steps, main="Total Number of Steps per Day", col="blue", 
     xlab="Steps", ylab="Count (1,000)")
hist(data_imputed$tot_steps, main="Total Number of Steps per Day", 
     col="red", xlab="Steps", ylab="Count(1,000)")

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# 1. Use the dataset with the filled-in missing values for this part.
# Create a new factor variable in the dataset with two levels - 
# "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of 
# the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot 
# should look like using simulated data.
activity2$day <- as.Date(activity2$date)

weekdays_1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity2$day_cat <- c('weekend', 'weekday')[(weekdays(activity2$day) %in% weekdays_1)+1L]

activity2$day_cat <- as.factor(activity2$day_cat)
summary(activity2$day_cat)

mean_steps <- mean(activity2$steps, na.rm=TRUE)

library(lattice)
xyplot(steps~interval | factor(day_cat), data=activity2, pch=19,
       main="Average Steps Taken for Interval", xlab="Interval", ylab="Avg Number of Steps",
       layout=c(1,2), type="l")




