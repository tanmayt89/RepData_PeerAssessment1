---
title: "Reproducible_Research_CP1"
output: html_document
---



We are loading and pre-processing data which we will use to extrapolate and answer questions about people's daily movement activities.


```r
library("ggplot2")
actData <- read.csv("./activity.csv")
summary(actData)
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
names(actData)
```

```
## [1] "steps"    "date"     "interval"
```


```r
head(actData)
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

```r}
pairs(actData)
```

###What is mean total number of steps taken per day?

1.	Calculate the total number of steps taken per day.



```r
stepsPerDay <- aggregate(steps ~ date, actData, sum, na.rm=TRUE)
```

2.	If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
hist(stepsPerDay$steps)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

3.	Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
```

```
## [1] 10766.19
```

```r
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

```
## [1] 10765
```

-The mean total number of steps taken each day is stored in variable meanStepsPerDay.
-The median total number of steps taken each day is stored in variable medianStepsPerDay.

###What is the average daily activity pattern?

1.	Make a time series plot (i.e.  type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
stepsPerInterval<-aggregate(steps~interval, data=actData, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

```
## [1] 835
```

- The 5-minute interval accross all the days containing the maximum number of steps is stored in variable intervalWithMaxNbSteps.


###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.	Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
totalValuesMissings <- sum(is.na(actData$steps))
totalValuesMissings
```

```
## [1] 2304
```
- The total number of missing values in the dataset is stored in the variable totalValuesMissings.

2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Let’s use a simple strategy : we’ll fill in all the missing values in the dataset with the mean per interval. Here’s the function that will return, for a particular interval, the mean value.


```r
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}
```

3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actDataNoNA<-actData
for(i in 1:nrow(actDataNoNA)){
    if(is.na(actDataNoNA[i,]$steps)){
        actDataNoNA[i,]$steps <- getMeanStepsPerInterval(actDataNoNA[i,]$interval)
    }
}
```

4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=actDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
-The mean total number of steps taken each day with no missing values is stored in variable meanStepsPerDayNoNA.
-The median total number of steps taken each day with no missing values is stored in variable medianStepsPerDayNoNA.

The mean didn’t change after the replacements of NAs, the median changed about 0.1% of the original value.

###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.	Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
actDataNoNA$date <- as.Date(strptime(actDataNoNA$date, format="%Y-%m-%d"))
actDataNoNA$day <- weekdays(actDataNoNA$date)
for (i in 1:nrow(actDataNoNA)) {
    if (actDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        actDataNoNA[i,]$day<-"weekend"
    }
    else{
        actDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(actDataNoNA$steps ~ actDataNoNA$interval + actDataNoNA$day, actDataNoNA, mean)
```

2.	Make a panel plot containing a time series plot (i.e., type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
