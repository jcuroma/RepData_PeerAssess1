---
title: "Peer Assessment1"
output: html_document
---

### Loading and preprocessing the data

```r
activity <- read.csv('activity.csv', header=T, sep=",")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

### Bring some packages necessary for analysis

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(ggplot2)
```

### What is mean total number of steps taken per day?
#### 1. Calculate the total number of steps taken per day

```r
activity %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) %>%
    print(n=61)
```

```
## Source: local data frame [61 x 2]
## 
##          date tot_steps
## 1  2012-10-01        NA
## 2  2012-10-02       126
## 3  2012-10-03     11352
## 4  2012-10-04     12116
## 5  2012-10-05     13294
## 6  2012-10-06     15420
## 7  2012-10-07     11015
## 8  2012-10-08        NA
## 9  2012-10-09     12811
## 10 2012-10-10      9900
## 11 2012-10-11     10304
## 12 2012-10-12     17382
## 13 2012-10-13     12426
## 14 2012-10-14     15098
## 15 2012-10-15     10139
## 16 2012-10-16     15084
## 17 2012-10-17     13452
## 18 2012-10-18     10056
## 19 2012-10-19     11829
## 20 2012-10-20     10395
## 21 2012-10-21      8821
## 22 2012-10-22     13460
## 23 2012-10-23      8918
## 24 2012-10-24      8355
## 25 2012-10-25      2492
## 26 2012-10-26      6778
## 27 2012-10-27     10119
## 28 2012-10-28     11458
## 29 2012-10-29      5018
## 30 2012-10-30      9819
## 31 2012-10-31     15414
## 32 2012-11-01        NA
## 33 2012-11-02     10600
## 34 2012-11-03     10571
## 35 2012-11-04        NA
## 36 2012-11-05     10439
## 37 2012-11-06      8334
## 38 2012-11-07     12883
## 39 2012-11-08      3219
## 40 2012-11-09        NA
## 41 2012-11-10        NA
## 42 2012-11-11     12608
## 43 2012-11-12     10765
## 44 2012-11-13      7336
## 45 2012-11-14        NA
## 46 2012-11-15        41
## 47 2012-11-16      5441
## 48 2012-11-17     14339
## 49 2012-11-18     15110
## 50 2012-11-19      8841
## 51 2012-11-20      4472
## 52 2012-11-21     12787
## 53 2012-11-22     20427
## 54 2012-11-23     21194
## 55 2012-11-24     14478
## 56 2012-11-25     11834
## 57 2012-11-26     11162
## 58 2012-11-27     13646
## 59 2012-11-28     10183
## 60 2012-11-29      7047
## 61 2012-11-30        NA
```

#### 2. Make a histogram of the total number of steps taken each day.

```r
data1 <- activity %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) 

hist(data1$tot_steps, main="Total Number of Steps per Day", 
     col="blue", xlab="Steps", ylab="Count (1,000)")
```

![plot of chunk createhistogram](figure/createhistogram-1.png) 

#### 3. Calculate and report the mean and median of the total number of steps taken per day.

```r
activity %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) %>%
    print(n=61)
```

```
## Source: local data frame [61 x 3]
## 
##          date mean_steps median_steps
## 1  2012-10-01        NaN           NA
## 2  2012-10-02  0.4375000            0
## 3  2012-10-03 39.4166667            0
## 4  2012-10-04 42.0694444            0
## 5  2012-10-05 46.1597222            0
## 6  2012-10-06 53.5416667            0
## 7  2012-10-07 38.2465278            0
## 8  2012-10-08        NaN           NA
## 9  2012-10-09 44.4826389            0
## 10 2012-10-10 34.3750000            0
## 11 2012-10-11 35.7777778            0
## 12 2012-10-12 60.3541667            0
## 13 2012-10-13 43.1458333            0
## 14 2012-10-14 52.4236111            0
## 15 2012-10-15 35.2048611            0
## 16 2012-10-16 52.3750000            0
## 17 2012-10-17 46.7083333            0
## 18 2012-10-18 34.9166667            0
## 19 2012-10-19 41.0729167            0
## 20 2012-10-20 36.0937500            0
## 21 2012-10-21 30.6284722            0
## 22 2012-10-22 46.7361111            0
## 23 2012-10-23 30.9652778            0
## 24 2012-10-24 29.0104167            0
## 25 2012-10-25  8.6527778            0
## 26 2012-10-26 23.5347222            0
## 27 2012-10-27 35.1354167            0
## 28 2012-10-28 39.7847222            0
## 29 2012-10-29 17.4236111            0
## 30 2012-10-30 34.0937500            0
## 31 2012-10-31 53.5208333            0
## 32 2012-11-01        NaN           NA
## 33 2012-11-02 36.8055556            0
## 34 2012-11-03 36.7048611            0
## 35 2012-11-04        NaN           NA
## 36 2012-11-05 36.2465278            0
## 37 2012-11-06 28.9375000            0
## 38 2012-11-07 44.7326389            0
## 39 2012-11-08 11.1770833            0
## 40 2012-11-09        NaN           NA
## 41 2012-11-10        NaN           NA
## 42 2012-11-11 43.7777778            0
## 43 2012-11-12 37.3784722            0
## 44 2012-11-13 25.4722222            0
## 45 2012-11-14        NaN           NA
## 46 2012-11-15  0.1423611            0
## 47 2012-11-16 18.8923611            0
## 48 2012-11-17 49.7881944            0
## 49 2012-11-18 52.4652778            0
## 50 2012-11-19 30.6979167            0
## 51 2012-11-20 15.5277778            0
## 52 2012-11-21 44.3993056            0
## 53 2012-11-22 70.9270833            0
## 54 2012-11-23 73.5902778            0
## 55 2012-11-24 50.2708333            0
## 56 2012-11-25 41.0902778            0
## 57 2012-11-26 38.7569444            0
## 58 2012-11-27 47.3819444            0
## 59 2012-11-28 35.3576389            0
## 60 2012-11-29 24.4687500            0
## 61 2012-11-30        NaN           NA
```

### What is the average daily activity pattern?
#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval(x-axis) and the average
####    number of steps taken, averaged across all days (y-axis)

```r
data2 <- activity %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE))
data2 <- tbl_df(data2)
str(data2)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	288 obs. of  2 variables:
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean_steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, "drop")= logi TRUE
```

```r
plot(data2$interval, data2$mean_steps, type="l", main="Time-series Plot",
     xlab="Interval", ylab="Average Steps")
```

![plot of chunk timeseriesplot](figure/timeseriesplot-1.png) 

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum 
#### number of steps?

```r
activity %>%
    group_by(interval) %>%
    summarise(max_step = max(mean(steps, na.rm = TRUE))) %>%
    summarise(himax_step = max(max_step, na.rm = TRUE)) 
```

```
## Source: local data frame [1 x 1]
## 
##   himax_step
## 1   206.1698
```

### Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset 
#### (i.e. the total number of rows with NAs)

```r
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset.
* Planning to use the mean substitution for missing values

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2 <- transform(activity, steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
activity2 <- tbl_df(activity2)
colSums(is.na(activity2))
```

```
##    steps     date interval 
##        0        0        0
```

```r
summary(activity2)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 37.38   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

#### 4.a. Make a histogram of the total number of steps taken each day.  

```r
data_imputed <- activity2 %>%
    group_by(date) %>%
    summarise(tot_steps = sum(steps)) 

hist(data_imputed$tot_steps, main="Total Number of Steps per Day", 
     col="red", xlab="Steps", ylab="Count(1,000)")
```

![plot of chunk histogram](figure/histogram-1.png) 

#### 4.b. Calculate and report the mean and median total number of steps taken per day. 

```r
before_imp <- activity %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) 
    
after_imp <- activity2 %>%
    group_by(date) %>%
    summarise(mean_steps = mean(steps, na.rm=TRUE),
              median_steps = median(steps, na.rm=TRUE)) 
```

#### 4.c. Do these values differ from the estimates from the first part of the assignment? 
#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
summary(before_imp); summary(after_imp)
```

```
##          date      mean_steps       median_steps
##  2012-10-01: 1   Min.   : 0.1424   Min.   :0    
##  2012-10-02: 1   1st Qu.:30.6979   1st Qu.:0    
##  2012-10-03: 1   Median :37.3785   Median :0    
##  2012-10-04: 1   Mean   :37.3826   Mean   :0    
##  2012-10-05: 1   3rd Qu.:46.1597   3rd Qu.:0    
##  2012-10-06: 1   Max.   :73.5903   Max.   :0    
##  (Other)   :55   NA's   :8         NA's   :8
```

```
##          date      mean_steps       median_steps   
##  2012-10-01: 1   Min.   : 0.1424   Min.   : 0.000  
##  2012-10-02: 1   1st Qu.:34.0938   1st Qu.: 0.000  
##  2012-10-03: 1   Median :37.3826   Median : 0.000  
##  2012-10-04: 1   Mean   :37.3826   Mean   : 4.903  
##  2012-10-05: 1   3rd Qu.:44.4826   3rd Qu.: 0.000  
##  2012-10-06: 1   Max.   :73.5903   Max.   :37.383  
##  (Other)   :55
```

```r
par(mfrow=c(1,2))
hist(data1$tot_steps, main="Total Number of Steps per Day", col="blue", 
     xlab="Steps", ylab="Count (1,000)")
hist(data_imputed$tot_steps, main="Total Number of Steps per Day", 
     col="red", xlab="Steps", ylab="Count(1,000)")
```

![plot of chunk summarycomparison](figure/summarycomparison-1.png) 

* There are more frequencies in the means for the data set with imputation than those for the orginal 
data set. Less variability is shown in the imputed data set.


### Are there differences in activity patterns between weekdays and weekends?
#### 1. Use the dataset with the filled-in missing values for this part.
#### Create a new factor variable in the dataset with two levels - 
#### "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 

```r
activity2$day <- as.Date(activity2$date)

weekdays_1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity2$day_cat <- c('weekend', 'weekday')[(weekdays(activity2$day) %in% weekdays_1)+1L]
activity2$day_cat <- as.factor(activity2$day_cat)
summary(activity2$day_cat)
```

```
## weekday weekend 
##   12960    4608
```


#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of 
#### the 5-minute interval (x-axis) and the average number of steps taken, 
#### averaged across all weekday days or weekend days (y-axis). 

```r
mean_steps <- mean(activity2$steps, na.rm=TRUE)

library(lattice)
xyplot(steps~interval | factor(day_cat), data=activity2, pch=19,
       main="Average Steps Taken for Interval", xlab="Interval", ylab="Avg Number of Steps",
       layout=c(1,2), type="l")
```

![plot of chunk createpanelplot](figure/createpanelplot-1.png) 


