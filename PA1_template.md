# PA1_template
Wenjin Sun  
March 28, 2017  



This is an R Markdown document for Coursera courser Reproducible Research Project 1.

Code for reading in the dataset and/or processing the data

```r
rm(list=ls())
setwd("E:/Predictive Analytics/CODE/R/04 Reproducible Research/Project 1")
dt <- read.csv("activity.csv",header=T,stringsAsFactors = FALSE,sep=",")
dt$date<- as.POSIXct(dt$date, format="%Y-%m-%d")
```

Histogram of the total number of steps taken each day

```r
daystep <- aggregate(dt$steps,by=list(day=dt$date),FUN=sum)
hist(daystep$x,main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Mean and median number of steps taken each day

```r
as.integer(mean(daystep$x,na.rm=TRUE))
```

```
## [1] 10766
```

```r
as.integer(median(daystep$x,na.rm=TRUE))
```

```
## [1] 10765
```

Time series plot of the average number of steps taken


```r
clean <- dt[!is.na(dt$steps),]
avgsteps <- aggregate(clean$steps,by=list(day=clean$date),FUN=mean)
plot(avgsteps$day,avgsteps$x,type = "l",main="Time series plot of the average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The 5-minute interval that, on average, contains the maximum number of steps


```r
interval <- aggregate(clean$steps,by=list(interval=clean$interval),FUN=mean)
interval[interval$x == max(interval$x),]
```

```
##     interval        x
## 104      835 206.1698
```

Code to describe and show a strategy for imputing missing data

```r
mean(is.na(dt$steps))
```

```
## [1] 0.1311475
```

```r
newdt <- data.frame(dt)
newdt$steps <- ifelse(is.na(newdt$steps), mean(newdt$steps, na.rm = TRUE), newdt$steps)
```

Histogram of the total number of steps taken each day after missing values are imputed

```r
daystepnew <- aggregate(newdt$steps,by=list(day=newdt$date),FUN=sum)
hist(daystepnew$x,main="Histogram of the total number of steps taken each day after imputing missing data")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
newdt$wDay <- factor((weekdays(newdt$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
interval2 <- aggregate(newdt$steps,by=list(wDay=newdt$wDay,interval=newdt$interval),FUN=mean)
library(lattice) 
xyplot(x~interval|wDay, data=interval2, type="l", layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

