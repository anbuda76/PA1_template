---
title: 'Peer Graded Assignment: Course Project 1 (Reproducible Research)'
author: "Andrea Buda"
date: "11 ottobre 2018"
output:
  html_document: default
  pdf_document: default
---

# Loading Data
### First all we need to load csv file in data variable
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

``` {r}
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
DataSet <- read.csv(unzip(temp, "activity.csv"), header = TRUE) 
head(DataSet)
summary(DataSet)
```

# Commit

## 1 Histogram of the total number of steps taken each day
### Aggregating (summation) of steps over date and make the histogram
``` {r}
agg_steps<- aggregate(steps ~ date, DataSet , FUN=sum, na.rm=TRUE)
hist(agg_steps$steps, xlab="Total number of steps", col = "lightblue",
     main="Histogram of the total number of steps taken each day")
```


## 2. Calculate and report the mean and median total number of steps taken per day

``` {r}
# Mean total number of steps taken per day
mean(agg_steps$steps)

# Median total number of steps taken per day
median(agg_steps$steps)
```

## 3. Make a time series plot of the average number of steps taken
``` {r}
agg_interval <- aggregate(steps ~ interval, DataSet , FUN=sum, na.rm=TRUE)
plot(agg_interval, type = "l", main = "Total Steps in time series")
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps
``` {r}
subset(agg_interval, agg_interval$steps == max(agg_interval$steps))
```

## 6. Code to describe and show a strategy for imputing missing data
``` {r}
#In the Output of the below query TRUE represents the total number of NA values
table(is.na(DataSet))

#In the original data set aggregating (mean) steps over 5-minute interval
mean_interval <- aggregate(steps ~ interval, DataSet, FUN=mean)

#Merging the mean of total steps for a date with the original data set
DataSet_new <- merge(x= DataSet, y=mean_interval, by="interval")

#Replacing the NA values with the mean for that 5-minute interval
DataSet_new$steps <- ifelse(is.na(DataSet_new$steps.x), DataSet_new$steps.y, DataSet_new$steps.x)
```
# 7. Histogram of the total number of steps taken each day after missing values are imputed
``` {r}
agg_steps2 <- aggregate(steps ~ date, DataSet_new , FUN=sum)
hist(agg_steps2$steps, xlab="Total number of steps", col = "lightblue",
     main="Histogram of the total number of steps taken each day\n(After imputing NA values with \n mean of 5-min interval)")

mean(agg_steps2$steps)
median(agg_steps2$steps)
```
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
``` {r}
# Convert date into weekdays

# install.packages("chron")
library(chron)
DataSet$dayofweek <- ifelse(is.weekend(DataSet$date), "weekend", "weekday")

#Aggregating(mean) steps over interval and day of week
mean_interval_new<- aggregate(steps ~ interval + dayofweek, DataSet, FUN=mean)

par(mfrow=c(1,2)) 
plot(subset(mean_interval_new, mean_interval_new$dayofweek == "weekday")$steps, type ="l", 
     col = "red", main="weekday", xlab="interval", ylab="number of steps" )
grid(col = "lightgray", lty = "dotted")

plot(subset(mean_interval_new, mean_interval_new$dayofweek == "weekend")$steps, type ="l", 
     main="weekend", xlab="interval", ylab="number of steps" )
grid(col = "lightgray", lty = "dotted")
```
