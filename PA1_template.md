# Reproducible Research: Peer Assessment 1
**If any of the plots are generated you could find them in the folder instructions_fig**

## Loading and preprocessing the data

Here we are going to load the data for this assignment


```r
activity<-read.csv(file = "activity.csv", sep = ",")
activity$date<-as.POSIXct(activity$date, format="%Y-%m-%d")
activity$weekdays<-weekdays(activity$date)
activity$daytype<-ifelse(activity$weekday == "sábado" | activity$weekday == "domingo", "weekend", "weekday")
head(activity)
```

```
##   steps       date interval weekdays daytype
## 1    NA 2012-10-01        0    lunes weekday
## 2    NA 2012-10-01        5    lunes weekday
## 3    NA 2012-10-01       10    lunes weekday
## 4    NA 2012-10-01       15    lunes weekday
## 5    NA 2012-10-01       20    lunes weekday
## 6    NA 2012-10-01       25    lunes weekday
```

## What is mean total number of steps taken per day?

This shows the number of steps taken per day:


```r
sums_activity<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
head(sums_activity)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

Here you could see a histogram that shows the number od steps taken each day


```r
hist(sums_activity, col = "red", xlab = "Total number of steps", main = "Histogram of the total number of steps taken each day\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Here you can see the mean and the median of the total number of steps taken per day


```r
mean(sums_activity)
```

```
## [1] 9354.23
```

```r
median(sums_activity)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Here you could see the average numver of steps done for each 5-minutes interval


```r
means_activity<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
head(means_activity)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

And this is the time series plot generated


```r
plot(unique(activity$interval),means_activity, type = "l", xlab = "Interval", ylab = "Averege number of steps",
     main = "Time-series of the average number of steps per intervals\n(NA removed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Here you will see what is the five-minutes interval, on average across all the days in the dataset that contains the maximum number of steps. Also you could see it's respective value.


```r
max_pos <- which(means_activity == max(means_activity))
means_activity[max_pos]
```

```
##      835 
## 206.1698
```

## Imputing missing values

Here I could show you the total number of missing values in the dataset


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

In the next codeblock I have replaced the the NA values by the mean of the steps attribute


```r
na_rows<-which(is.na(activity$steps))
replacing_vector<-rep(mean(activity$steps,na.rm = TRUE), times=length(na_rows))
activity[na_rows,"steps"]<-replacing_vector
head(activity)
```

```
##     steps       date interval weekdays daytype
## 1 37.3826 2012-10-01        0    lunes weekday
## 2 37.3826 2012-10-01        5    lunes weekday
## 3 37.3826 2012-10-01       10    lunes weekday
## 4 37.3826 2012-10-01       15    lunes weekday
## 5 37.3826 2012-10-01       20    lunes weekday
## 6 37.3826 2012-10-01       25    lunes weekday
```

As you could see the NA values of the first day have been changed by the mean.

Then the number of steps taken per day are calculated as I did in the first question.


```r
sums_activity2<-tapply(activity$steps,activity$date,sum,na.rm=TRUE)
head(sums_activity2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

Then I have created the histogram that shows the number of steps taken each day but having the NA values replaced


```r
hist(sums_activity2, col = "red", xlab = "Total number of steps", main = "Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

As I did in the firs question here I show the values of the mean of the total number of steps taken per day accordint to the fact that we have the NA values replaced


```r
mean(sums_activity2)
```

```
## [1] 10766.19
```

```r
median(sums_activity2)
```

```
## [1] 10766.19
```

As you could see the values differ grately from the estimates of the first question. The impact of imputing the missing values means that we have more data so it's obvious that the mean and the median will change


## Are there differences in activity patterns between weekdays and weekends?

You will have seen that in the part of processing the data I have generated two colums that shows the weekday and also what type of day is it (weekday ot weekend days). Here you see the average of steps taken across all weekdays or weekend days of the 5-minutes interval.


```r
mean_data_activity<-aggregate(activity$steps, by=list(activity$interval, activity$weekday, activity$daytype), mean)
names(mean_data_activity) <- c("interval","weekday","daytype", "mean")
head(mean_data_activity)
```

```
##   interval weekday daytype     mean
## 1        0  jueves weekday 9.375844
## 2        5  jueves weekday 4.153622
## 3       10  jueves weekday 4.153622
## 4       15  jueves weekday 5.042511
## 5       20  jueves weekday 4.153622
## 6       25  jueves weekday 5.375844
```

So this is the panel plot (using **lattice** package) generated accross the average of steps  by interval and days type.


```r
library(lattice)
xyplot(mean ~ interval | daytype, mean_data_activity, type="l", xlab="Interval",  ylab="Number of steps", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
