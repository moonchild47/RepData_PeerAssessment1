---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Exploring a dataset from a personal activity monitoring device
which collects data at 5 minute intervals through out the day.

The data consists of two months of data from an anonymous individual 
collected during the months of October and November, 2012 
and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

```r
library(ggplot2)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
destfile <- "file.zip" 
if (!file.exists(destfile)) { 
  download.file(url, destfile = destfile, method = "curl") 
  unzip(destfile, exdir = ".") 
} 

# load data into R 
activity <- read.csv("activity.csv") 

# convert to date format
activity$date <- as.Date(activity$date,"%Y-%m-%d")
```



## What is mean total number of steps taken per day?


```r
steps_per_day <- aggregate(x = activity$steps,
                           FUN = sum,
                           by = list(date = activity$date),
                           na.rm = TRUE)

ggplot(data = steps_per_day, aes(x)) + geom_histogram(binwidth = 500) +
  xlab("Number of steps") + 
  ylab(expression("Number of days with this total number of steps"  %+-% 500)) + 
  ggtitle("Number of days with a certain total number of steps " )
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
mean(steps_per_day$x)
```

```
## [1] 9354.23
```


```r
median(steps_per_day$x)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
steps_per_interval <- aggregate(x = activity$steps,
                                FUN = mean,
                                by = list(interval = activity$interval),
                                na.rm = TRUE)
ggplot(data = steps_per_interval, aes(x = interval, y = x))+
  geom_line() + xlab("number of 5 min time interval") + 
  ylab("average step number over two months") + 
  ggtitle("Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
steps_per_interval$interval[which.max(steps_per_interval$x)]
```

```
## [1] 835
```

The interval 835 (related to 8:35 a.m.) contains the maximum number of steps.

## Imputing missing values


```r
# count NA values
sum(is.na(activity$steps))
```

```
## [1] 2304
```


```r
# Create an additional column with the mean of each day
# then go through rows and replace NAs with values from that column
activity_imputed <- merge(activity,steps_per_day)
activity_imputed$steps[is.na(activity_imputed$steps)] <- 
  activity_imputed$x[is.na(activity_imputed$steps)] 
```



```r
# calculate total number of steps per day again

steps_per_day_imputed <- aggregate(x = activity_imputed$steps,
                                   FUN = sum,
                                   by = list(date = activity_imputed$date),
                                   na.rm = TRUE)

ggplot(data = steps_per_day_imputed, aes(x)) + geom_histogram(binwidth = 500) +
  xlab("Number of steps") + 
  ylab(expression("Number of days with this total number of steps"  %+-% 500)) + 
  ggtitle("Number of days with a certain total number of steps " )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
mean(steps_per_day_imputed$x)
```

```
## [1] 9354.23
```



```r
median(steps_per_day_imputed$x)
```

```
## [1] 10395
```

Here it does not change anything because the missing data is only present
on days where there was no data at all, therefore completing the data 
with the daily mean of zero does not change the total number

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels - "weekday"
# and "weekend" indicating whether a given date is a weekday or weekend day.
activity_imputed$weekday <- factor((weekdays(activity_imputed$date) %in% c("Saturday", "Sunday")), 
                             levels = c(TRUE, FALSE), labels = c("weekend", "weekday"))
```



```r
# Calculate average number of steps per interval and whether it is a weekday
steps_per_interval_weekday <- aggregate(x = activity_imputed$steps,
                                FUN = mean,
                                by = list(interval = activity_imputed$interval,
                                          weekday = activity_imputed$weekday),
                                na.rm = TRUE)
ggplot(data = steps_per_interval_weekday, aes(x = interval, y = x))+
  geom_line() + facet_wrap(~weekday) +
  xlab("number of 5 min time interval") + 
  ylab("average step number over two months") + 
  ggtitle("Average daily activity pattern for weekend and weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

