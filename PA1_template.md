---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---


## Loading and preprocessing the data

We load the .csv file into R


```r
data <- read.csv("activity.csv", as.is=TRUE)
```
We take a look at the file to check if we need to clean something.

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
head(data)
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
We observe that the data is already stored in a clean and tidy way, so the only thing we need to do is to convert the date column to a more useful type:

```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

We will get an idea of the distribution of the activity over the whole period by plotting a histogram of the stepscounts of each day.

```r
dailysteps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
plot(unique(data$date), dailysteps, "s", main="Total steps each day", xlab="Number of steps", ylab="Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

We now obtain the mean and median of the stepcounts for each day:

```r
mean(dailysteps)
```

```
## [1] 9354.23
```

```r
median(dailysteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Now we observe the pattern the activity follows each day. 

```r
avg <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(1:288, avg, type="l", ylab="Average number of steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

We find also the time interval with the higher stepcount:

```r
avg[which(avg==max(avg))]
```

```
##      835 
## 206.1698
```


## Imputing missing values

As previously mentioned, we will perform our analysis first on the raw data and then in a new dataset without missing values. Therefore, we will construct a new collection replacing each NA value with the mean number steps taken in that same 5 minute interval.

Lets first look how many NAvalues there are:

```r
NAvalues <- which(is.na(data$steps))
length(NAvalues)
```

```
## [1] 2304
```

So there are 2304 rows with an NA value intheir steps field. We will fix this by creating a new dataset with the NA values replaced with the number of steps taken in the corresponding interval averaged across all days.



```r
intervalmean <-tapply(data$steps, as.character(data$interval), mean, na.rm=TRUE)
data2 <- data
data2[NAvalues,]$steps <- intervalmean[as.character(data2[NAvalues,]$interval)]
```

Now, if we have a look at the list of stepcounts that were missing in the original dataset we see that they have been substitued by the corresponding means:


```r
head(data[which(is.na(data$steps)),])
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
head(data2[which(is.na(data$steps)),])
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

We repeat now our previous steps in order to observe the effect of removing the NAs. First, we redraw the steps versus day histogram. We present the first plot for comparision purposes.


```r
dailysteps2 <- tapply(data2$steps, data2$date, sum, na.rm=TRUE)
par(mfrow=c(1, 2))
plot(unique(data$date), dailysteps, "s", main="Total steps each day", xlab="Number of steps", ylab="Day", sub="(No NA step values remove)")
plot(unique(data2$date), dailysteps2, "s", main="Total steps each day", xlab="Number of steps", ylab="Day", sub="(All NA step values remove)")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

We can see how the NA values have been replaced by the average value, resulting in a better visualization of the data; those cells that are almost 0 do now represent values close to 0, while before they also represented NA values.

We now obtain the mean and median of the stepcounts for each day:

```r
mean(dailysteps2)
```

```
## [1] 10766.19
```

```r
median(dailysteps2)
```

```
## [1] 10766.19
```

Compare those results with the ones we obtained before (mean: 9354.2295082, median:10395). Interestingly, the mean and the median have equal values now, which could be due to the reduced number of outliers (which skew the mean but not the media) in the second dataset. 

## Are there differences in activity patterns between weekdays and weekends?

We add another variable to our dataset with a factor indicating if the date is a weekday or a weekend

```r
data2$daytype = as.factor(ifelse(weekdays(data2$date) %in% c("Samstag", "Sonntag"), "weekend", "weekday"))
```

Now we plot two different graphs

```r
weekdays <- which(data2$daytype=="weekday")
weekdaymeans <- tapply(data2[weekdays,]$steps, data2[weekdays,]$interval, mean)
weekendmeans <- tapply(data2[-weekdays,]$steps, data2[-weekdays,]$interval, mean)
par(mfrow=c(2,1))
plot(unique(data$interval), weekdaymeans, type="l", main="Weekdays", ylab="Aveage step number", xlab="Interval")
plot(unique(data$interval), weekendmeans, type="l", main="Weekends", ylab="Aveage step number", xlab="Interval")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

We can clearly see how both patterns differ: during the weekdays the activity starts and ends earlier in the day, due to people getting up and going to bed earlier, and in the weekends, and it seems the biggest part of the activity happens around 8:00, when most conmutes happen. In the weekends the activity starts more gradually, and then presents more and bigger "spikes" than in the weekdays, possibly corresponding to workout sessions.
