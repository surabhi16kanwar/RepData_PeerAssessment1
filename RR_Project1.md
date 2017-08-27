# Reproducable Research- Assignment 1



## 1. Loading and processing the data


```r
getwd()
```

```
## [1] "C:/Users/surtabhi/Desktop"
```

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.4.1
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.1
```

```r
rr <- read.csv(file="activity.csv", header=TRUE)

# Remove NAs from data

rr_clean <- rr[ with (rr, { !(is.na(rr$steps)) } ), ]
```

## 2. Mean total number of steps taken per day (ignoring NA values)


```r
steps_day <- group_by(rr_clean, date)

steps_per_day <- summarise(steps_day, total = sum(steps))

# Histogram for total steps in each day

hist(steps_per_day$total, main="Histogram for total number of steps each day", xlab="Total steps in one day")
```

![](RR_Project1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

# Mean, median of total number of steps taken

```r
summary(steps_per_day)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

## 3. Average daily activity pattern
## Time series plot


```r
steps_interval <- aggregate(steps ~ interval, rr_clean, mean)
 
plot(steps_interval$interval, steps_interval$steps, type='l', main="Average daily activity pattern", xlab="Interval", ylab="Average number of steps")
```

![](RR_Project1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Interval with maximum number of steps

```r
max_steps_row <- which.max(steps_interval$steps)
 
steps_interval[max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

## 4.Dealing with missing values

## Total number of missing values

```r
sum(is.na(rr))
```

```
## [1] 2304
```

## Filling the missing values with the mean of 5-minute interval

```r
data_impute <- rr
for (i in 1:nrow(data_impute)) {
     if (is.na(data_impute$steps[i])) {
         interval_value <- data_impute$interval[i]
         steps_value <- steps_interval[
             steps_interval$interval == interval_value,]
         data_impute$steps[i] <- steps_value$steps
     }
 }
 
 
 imputed_steps <- aggregate(steps ~ date, data_impute, sum)
```

## Histogram for total number of steps taken each day

```r
hist(imputed_steps$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps for each day")
```

![](RR_Project1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

##Mean, median of steps taken per day
###For imputed data

```r
mean(imputed_steps$steps)
```

```
## [1] 10766.19
```

```r
median(imputed_steps$steps)
```

```
## [1] 10766.19
```
###Data without NA values

```r
mean(steps_per_day$total)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$total)
```

```
## [1] 10765
```
### So the mean is same but different median

## 5. Difference in activity patterns between weekdays and weekends


```r
data_impute['type_of_day'] <- weekdays(as.Date(data_impute$date))
 data_impute$type_of_day[data_impute$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
 data_impute$type_of_day[data_impute$type_of_day != "weekend"] <- "weekday"
 


 # convert type_of_day from character to factor
 data_impute$type_of_day <- as.factor(data_impute$type_of_day)
 
 # calculate average steps by interval across all days
 imputed_steps <- aggregate(steps ~ interval + type_of_day, data_impute, mean)
```

##Panel plot containing time series

```r
# creating panel plot
 qplot(interval, 
       steps, 
       data = imputed_steps, 
       type = 'l', 
       geom=c("line"),
       xlab = "Interval", 
       ylab = "Number of steps", 
       main = "") +
    facet_wrap(~ type_of_day, ncol = 1)
```

```
## Warning: Ignoring unknown parameters: type
```

![](RR_Project1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

