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



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
