# Reproducible Research: Peer Assessment 1


##Load packages.

```r
library(dplyr)
library(lattice)
library(mice)
```


## Loading and preprocessing the data

```r
activity <- read.csv(unzip("activity.zip"))
activity_day <- activity %>%
      group_by(date) %>%
      summarise(steps = sum(steps, na.rm=TRUE))
```


## What is mean total number of steps taken per day?


##2. Histogram of the total number of steps taken each day

```r
barplot(activity_day$steps, main = "Number of steps by days", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/barplot-1.png)<!-- -->

##3. Mean and median number of steps taken each day

```r
mean_meadian <- activity %>%
    group_by(date) %>%
    summarise(mean = mean(steps, na.rm=TRUE), median=median(steps, na.rm=TRUE))
mean_meadian
```

```
## # A tibble: 61 x 3
##          date     mean median
##        <fctr>    <dbl>  <dbl>
## 1  2012-10-01      NaN     NA
## 2  2012-10-02  0.43750      0
## 3  2012-10-03 39.41667      0
## 4  2012-10-04 42.06944      0
## 5  2012-10-05 46.15972      0
## 6  2012-10-06 53.54167      0
## 7  2012-10-07 38.24653      0
## 8  2012-10-08      NaN     NA
## 9  2012-10-09 44.48264      0
## 10 2012-10-10 34.37500      0
## # ... with 51 more rows
```

## What is the average daily activity pattern?

```r
activity_interval <- activity %>%
      group_by(interval) %>%
      summarise(mean_steps = mean(steps, na.rm=TRUE))

with(activity_interval, plot(interval, mean_steps, type="l"))
```

![](PA1_template_files/figure-html/daily pattern-1.png)<!-- -->

```r
activity[which(activity$steps == max(activity$steps, na.rm=TRUE)),]
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```

## Imputing missing values


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
imputed_Data <- mice(activity, m=2, maxit = 2, method = 'pmm', seed = 500)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   2   1  steps
##   2   2  steps
```

```r
activity_complete <- complete(imputed_Data,2)

activity_day_complete <- activity_complete %>%
      group_by(date) %>%
      summarise(steps = sum(steps))

barplot(activity_day_complete$steps, main = "Total number of steps by days (with no missing values)", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/imputation-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_complete$weekdays <- weekdays(as.Date(activity_complete$date,'%Y-%m-%d'))
activity_complete$day <- "weekday"
activity_complete$day[activity_complete$weekdays %in% c("Saturday", "Sunday")] <- "weekend"

activity_interval_complete <- activity_complete %>%
      group_by(interval, day) %>%
      summarise(steps = mean(steps))
xyplot (steps~interval | day, data=activity_interval_complete, type="l")
```

![](PA1_template_files/figure-html/weekdays vs weekends-1.png)<!-- -->
