# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
#Download and unzip the data file
address <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
address <- sub("^https", "http", address) #https does not work
zipname <- "activity.zip"
download.file(address,zipname)
unzip(zipname)
csvname <- "activity.csv"
activity <- read.csv(csvname)

#housekeeping - remove the zip as it is no longer needed
file.remove(zipname, csvname)
```

```
## [1] TRUE TRUE
```

```r
#housekeeping
rm(address, zipname, csvname)
```

## What is mean total number of steps taken per day?


```r
stepsperdate <- (aggregate(steps ~ date, data=activity, sum))
hist(stepsperdate$steps, breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanstepsperdate <- mean(stepsperdate$steps)
meanstepsperdate
```

```
## [1] 10766.19
```

```r
medianstepsperdate <- median(stepsperdate$steps)
medianstepsperdate
```

```
## [1] 10765
```

The mean total numbers of steps taken per day is 10766.19.
The median total numbers of steps taken per day is 10765.


## What is the average daily activity pattern?


```r
meanstepsperinterval <- (aggregate(steps ~ interval, data=activity, mean))
plot(meanstepsperinterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
interval_with_max_steps <- meanstepsperinterval$interval[which(meanstepsperinterval$steps == max(meanstepsperinterval$steps))]
interval_with_max_steps 
```

```
## [1] 835
```

The interval with the most steps is 835.

## Imputing missing values

I replace the missing values with the mean for that period.


```r
number_of_missing_values <- sum(is.na(activity$steps))
number_of_missing_values
```

```
## [1] 2304
```

```r
intervals_missing_steps <- activity$interval[is.na(activity$steps)]
means_for_missing_steps <-
sapply(intervals_missing_steps,
       function(x) meanstepsperinterval$steps[
         meanstepsperinterval$interval==x
       ]
       )

new_activity <- activity
new_activity$steps[is.na(new_activity$steps)] <- means_for_missing_steps

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
head(new_activity)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

The number of missing values is 2304.


```r
new_stepsperdate <- (aggregate(steps ~ date, data=new_activity, sum))
hist(new_stepsperdate$steps, breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
newmean_stepsperdate <- mean(new_stepsperdate$steps)
newmean_stepsperdate
```

```
## [1] 10766.19
```

```r
newmedian_stepsperdate <- median(new_stepsperdate$steps)
newmedian_stepsperdate
```

```
## [1] 10766.19
```
The mean total numbers of steps taken per day for the data set with imputed missing values
is 10766.19
versus 10766.19 in the data set with missing values.
The median total numbers of steps taken per day for the data set with imputed missing values
is 10766.19
versus 10765 in the data set with missing values.



## Are there differences in activity patterns between weekdays and weekends?

```r
library(lubridate)

activity$weekdays <- weekdays(ymd(activity$date))

activity$weekend <- "weekday"
activity$weekend[activity$weekdays %in%
                 c("Saturday","Sunday") ] <- "weekend"

meanstepsweekend <- (aggregate(steps ~ interval + weekend,
                               data=activity, mean))

library(lattice)
xyplot(steps ~ interval | weekend,
       data=meanstepsweekend,
       layout=c(1,2,1),
       panel=panel.lines)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The activity pattern during weekdays shows a clear spike in the morning. The activity pattern during the weekend is more evenly distributed.
