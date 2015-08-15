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
#aggregate the sum of the number of steps per day
stepsperdate <- (aggregate(steps ~ date, data=activity, sum))

#plot the histogram
hist(stepsperdate$steps, breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#calculate and print the mean steps per day, excluding missing values 
meanstepsperdate <- mean(stepsperdate$steps, na.rm = TRUE)
meanstepsperdate
```

```
## [1] 10766.19
```

```r
#calculate and print the median steps per day, excluding missing values 
medianstepsperdate <- median(stepsperdate$steps, na.rm = TRUE)
medianstepsperdate
```

```
## [1] 10765
```

The mean total numbers of steps taken per day is 10766.19.
The median total numbers of steps taken per day is 10765.


## What is the average daily activity pattern?

To get a good picture of the daily activity pattern, I need to calculate the mean number of steps per interval. Plotting this as a time series plot, this will give an insight in the pattern.

```r
#calculate the mean steps per interval, excluding missing values
meanstepsperinterval <- (aggregate(steps ~ interval, data=activity, function(x) mean(x, na.rm = TRUE)))
plot(meanstepsperinterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#let's see which interval corresponds to the max mean number of steps taken
interval_with_max_steps <- meanstepsperinterval$interval[which(meanstepsperinterval$steps == max(meanstepsperinterval$steps))]
#and print it
interval_with_max_steps 
```

```
## [1] 835
```

Not a lot of steps are taken during the night. The pattern show as spike during the morning. The rest of the day shows more or less the same amount of activity. The interval with the most steps is 835.

## Imputing missing values

As we have seen, there is an activity pattern that has different values depending on the time interval. So it I don't choose to replace the missing values with mean value of that day. The strategy I choose, is to replace the missing values using the mean for that interval.


```r
#first lets see how many values are missing, just curious
number_of_missing_values <- sum(is.na(activity$steps))
number_of_missing_values
```

```
## [1] 2304
```

```r
#lets see which intervals are missing steps
intervals_missing_steps <- activity$interval[is.na(activity$steps)]

#now make a vector with the corresponding means for the missing interval
#we reuse the vector meanstepsperinterval, that we allready created
means_for_missing_steps <-
sapply(intervals_missing_steps,
       function(x) meanstepsperinterval$steps[
         meanstepsperinterval$interval==x
       ]
       )

#make a copy of the steps data to keep the original data
activity$orig_steps <- activity$steps
#now replace the missing values with the corresponding means
activity$steps[is.na(activity$steps)] <- means_for_missing_steps

#head(activity)
#head(new_activity)
```

The number of missing values is 2304.


```r
#aggregate the sum of the number of steps per day
#this should be more, because we have now imputed the missing values
new_stepsperdate <- (aggregate(steps ~ date, data=activity, sum))
hist(new_stepsperdate$steps, breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
#calculate and print the mean steps per day
newmean_stepsperdate <- mean(new_stepsperdate$steps)
newmean_stepsperdate
```

```
## [1] 10766.19
```

```r
#calculate and print the median steps per day
newmedian_stepsperdate <- median(new_stepsperdate$steps)
newmedian_stepsperdate
```

```
## [1] 10766.19
```
The mean total numbers of steps taken per day for the data set with imputed missing values
is 10766.19
versus 10766.19 in the data set containing missing values.
The median total numbers of steps taken per day for the data set with imputed missing values
is 10766.19
versus 10765 in the data set containing missing values.


## Are there differences in activity patterns between weekdays and weekends?
I will use the dates to find the days of the week. Then I will use the days of the week to determine what dates are weekdays and what days are weekend. 

```r
#use lubridate package to work with dates
library(lubridate)

#add a vector with the weekdays corresponding to the date
activity$weekdays <- weekdays(ymd(activity$date))

#add a vector weekend, with a default value of "weekday"...
activity$weekend <- "weekday"
#...and replace those correpsonding with Saturday of Sunday with "weekend" 
activity$weekend[activity$weekdays %in%
                 c("Saturday","Sunday") ] <- "weekend"

#now aggregate the mean number of steps by interval
#including the weekend plot
meanstepsweekend <- (aggregate(steps ~ interval + weekend,
                               data=activity, mean))

#load lattice package for making a lattice plot
library(lattice)
#plot steps versus interval, in two plots depending on the weekend flag
#use panel.lines to plot a line
xyplot(steps ~ interval | weekend,
       data=meanstepsweekend,
       layout=c(1,2,1),
       panel=panel.lines)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The activity pattern during weekdays shows a clear spike in the morning. The activity pattern during the weekend is more evenly distributed over the day.
