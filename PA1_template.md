Project 1 - Assignment
========================================================

## Load required libraries


```r
library(reshape2)
library(lattice)
```

## Loading and preprocessing the data


```r
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

## Question 1 - What is mean total number of steps taken per day? 


```r
melted <- melt(data, id.vars="date", measure.var = "steps" )
totalSteps <- dcast(melted, date ~ variable, sum)

hist(totalSteps$steps, xlab= "Steps", main = paste("Histogram of" , "Steps per Day"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

## Calculate and report the mean and median total number of steps taken per day

### Mean

```r
meanSteps <- dcast(melted, date ~ variable, mean)
```

### Median

```r
medianSteps <- dcast(melted, date ~ variable, median, fill=0)
```

## Question 2 - What is the average daily activity pattern?

```r
dataNoNA <- na.omit(data)
meltInterval <- melt(dataNoNA, id.vars="interval", measure.var = "steps" )
totalStepsInterval <- dcast(meltInterval, interval ~ variable, sum)

plot(totalStepsInterval$interval,totalStepsInterval$steps, type="l", xlab = "Interval", ylab = "Steps")
title(main = "Nmber of Steps per 5 min Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


## Question 3 - Imputing missing values

### Poplulate the NA with the mean for that 5-minute interval


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
for(i in 1:nrow(data)){
    temp <- 0
    if(is.na(data[i,1])){
        temp <- mean(subset(data, data$interval == data[i,3])[,1], na.rm=T)
        data[i,1] <- temp
    }
}
```

### Histogram of the total number of steps taken each day

```r
meltedClean <- melt(data, id.vars="date", measure.var = "steps" )
totalStepsclean <- dcast(meltedClean, date ~ variable, sum)

hist(totalStepsclean$steps, xlab= "Steps", main = paste("Histogram of" , "Steps per Day"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### Mean

```r
meanStepsClean <- dcast(meltedClean, date ~ variable, mean)
```


### Median

```r
medianStepsClean <- dcast(meltedClean, date ~ variable, median)
```

### Only Mean and Median of NA values are differ than the first part of assignment


## Question 4 - Are there differences in activity patterns between weekdays and weekends?


```r
weekdayCol <- c()
for(i in 1:nrow(data)){
    temp <- as.character(weekdays(data[i,2]))
    weekdayCol <- append(weekdayCol, temp)
}

newData <- cbind(data,weekdayCol)
weekdayData <- subset(newData, newData$weekdayCol %in% c("Monday","Tuseday","Wednesday","Thursday","Friday"))

weekendData <- subset(newData, newData$weekdayCol %in% c("Saturday", "Sunday"))

meltedWeek <- melt(weekdayData, id.vars="interval", measure.var = "steps" )
totalStepsWeek <- dcast(meltedWeek, interval ~ variable, mean)
totalStepsWeek <- data.frame(totalStepsWeek[,1:2], Week = "Weekday")

meltedWeekend <- melt(weekendData, id.vars="interval", measure.var = "steps" )
totalStepsWeekend <- dcast(meltedWeekend, interval ~ variable, mean)
totalStepsWeekend <- data.frame(totalStepsWeekend[,1:2], Week ="Weekdayend")

mergeWeek <- rbind(totalStepsWeek,totalStepsWeekend)

options(scipen=999)
xyplot( steps ~ interval | Week , data = mergeWeek , type= "l", layout =c (1,2))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
