---
title: 'Reproducible Research: Peer Assessment 1'
author: "Jared A Smith"
date: "January 4, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Loading and preprocessing the data

```{r}
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?

```{r}
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)
```

##What is the average daily activity pattern?

```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

On average across all the days in the dataset, which 5-minute interval contains the maximum number of steps?

```{r}
averages[which.max(averages$steps),]
```

##Imputing missing values

There are many days/intervals where there are missing values. In this dataset these values are coded NA. The presence of missing days may introduce bias into calculations or summaries of the data.

```{r}
missing <- is.na(data$steps)
# How many missing
table(missing)
```

All of the missing values are filled in with mean value for that 5-minute interval.

```{r}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

Below a histogram of the total number of steps taken each day and the calculation of the mean and median total number of steps is given with the filled dataset.

```{r}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```

Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with steps values NA for all 5-minute intervals. After replacing missing steps values with the mean steps of associated interval value, these 0 values are removed from the histogram.

##Are there differences in activity patterns between weekdays and weekends?

First, the day of the week for each measurement in the dataset. In this part, we use the dataset with the filled-in values.

```{r}
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Below there is a panel plot containing plots of the average number of steps taken on weekdays and on weekends.

```{r}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```