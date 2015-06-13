# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
#Unzip and load data file
unzip("activity.zip")
data <- read.csv("activity.csv")

#Convert column date type into Date format
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
1. Total number of steps taken per day

```r
library(dplyr)
total_steps <- summarize(group_by(data, date), total_steps=sum(steps, na.rm=TRUE))
```



2. Histogram of the total number of steps taken each day

```r
hist(total_steps$total_steps, main = "Histogram of the total number of steps taken each day", xlab = "Total number of steps", col="blue", nclass=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Mean and median of the total number of steps taken per day  
3.1 Compute mean and median

```r
mean(total_steps$total_steps)
```

```
## [1] 9354.23
```

```r
median(total_steps$total_steps)
```

```
## [1] 10395
```


3.2 Report mean and median

```r
hist(total_steps$total_steps, main = "Histogram of the total number of steps taken each day", xlab = "Total number of steps", col="blue", nclass=20)
abline(v=mean(total_steps$total_steps), col="red", lwd=2)
abline(v=median(total_steps$total_steps), col="green", lwd=2)
legend("topright", lwd=c(2,2),col = c("red", "green"), legend = c("mean","median"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
