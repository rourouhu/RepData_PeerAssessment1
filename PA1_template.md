#Reproducible Research 
##Peer Assessment 1
========================================================

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

**Loading and preprocessing the data**

```r
setwd("C:/Users/Laptop/Desktop/data scientist track/5. Reproducible/hw1")
activity <- read.csv("activity.csv")
```

**Calculate the total number of steps taken per day**

```r
sum_perday <- aggregate(steps~date,FUN=sum, data=activity)
sum_perday
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

**Make a histogram of the total number of steps taken each day**

```r
hist(sum_perday$steps, main="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

**Calculate and report the mean and median of the total number of steps taken per day**

```r
step_mean <- mean(sum_perday$steps)
step_median <- median(sum_perday$steps)
step_mean
```

```
## [1] 10766.19
```

```r
step_median
```

```
## [1] 10765
```

**Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
avg_perinterval <- aggregate(steps~interval,FUN=mean, data=activity)
plot(x=avg_perinterval$interval,y=avg_perinterval$steps,type="l", main="time series plot of interval vs avg steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
which(avg_perinterval$step==max(avg_perinterval$step))
```

```
## [1] 104
```


**Calculate and report the total number of missing values in the dataset**

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

**devise a strategy for filling in all of the missing values in the dataset**
assign the mean of steps to the missing values

```r
activity$steps[which(is.na(activity$steps))] <- mean(activity$steps, na.rm=TRUE)
```

**Create a new dataset that is equal to the original dataset but with the missing data filled in**

```r
newactivity <- activity
```

**Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

```r
sum_perday1 <- aggregate(steps~date,FUN=sum, data=newactivity)
hist(sum_perday1$steps, main="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
step_mean1 <- mean(sum_perday1$steps)
step_median1 <- median(sum_perday1$steps)
step_mean1
```

```
## [1] 10766.19
```

```r
step_mean
```

```
## [1] 10766.19
```

```r
step_median1
```

```
## [1] 10766.19
```

```r
step_median
```

```
## [1] 10765
```

**Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day**

```r
newactivity$date <- as.Date(newactivity$date, "%Y-%m-%d")
week_days <- weekdays(newactivity$date)
for(i in 1:17568){
      if(week_days[i] == "Saturday") newactivity$weekday[i] <- "weekend"
      else if(week_days[i] == "Sunday") newactivity$weekday[i] <- "weekend"
      else newactivity$weekday[i] <- "weekday"
      }
```

**Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data**

```r
newactivity1 <- subset(newactivity, weekday == "weekday")
newactivity2 <- subset(newactivity, weekday == "weekend")
avg_perinterval1 <- aggregate(steps~interval,FUN=mean, data=newactivity1)
avg_perinterval2 <- aggregate(steps~interval,FUN=mean, data=newactivity2)
par(mfrow=c(2,1))
par(mar=c(1,1,1,1))
plot(x=avg_perinterval1$interval,y=avg_perinterval1$steps,type="l", main="weekday")
plot(x=avg_perinterval2$interval,y=avg_perinterval2$steps,type="l", main="weekend")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

knit2html("PA1_template.Rmd")
