---
title: "RepRes - Peer 1"
author: "Michael Pawlus"
date: "Saturday, January 17, 2015"
output: html_document
---

Reproducible Research - Peer assessment 1

Loading and preprocessing the data

```{r echo = TRUE}
activity <- read.csv("activity.csv")
```

What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```{r echo = TRUE}
stepsByDate <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(stepsByDate$steps, names.arg = stepsByDate$date, xlab = "date", ylab = "steps")
```

2. Calculate and report the mean and median total number of steps taken per day

```{r echo = TRUE}
mean(stepsByDate$steps)
median(stepsByDate$steps)
```

What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r echo = TRUE}
stepsByInt <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(stepsByInt, type="l")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r echo = TRUE}
stepsByInt$interval[which.max(stepsByInt$steps)]
```

Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r echo=TRUE}
sum(is.na(activity))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy choice is to use the mean value to replace missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r echo=TRUE}
imputedActivity <- activity
imputedActivity$steps[is.na(imputedActivity$steps)] = mean(imputedActivity$steps, na.rm=TRUE)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r echo=TRUE}
stepsByDateI <- aggregate(steps ~ date, data = imputedActivity, FUN = sum)
barplot(stepsByDateI$steps, names.arg = stepsByDateI$date, xlab = "date", ylab = "steps")
```

```{r echo=TRUE}
mean(stepsByDateI$steps)
median(stepsByDateI$steps)
```

By adding imputed values the mean remains unchanged and the new median is equal to mean.  The shape of the histogram is largely unchanged but there are more values present

Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r echo=TRUE}
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
imputedActivity$daytype <- as.factor(sapply(imputedActivity$date, daytype))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r echo=TRUE}
daytypeIntSt <- aggregate(data=imputedActivity, steps ~ daytype + interval, FUN=mean)
library("lattice")
xyplot(type="l", data=daytypeIntSt, steps ~ interval | daytype, xlab="interval", ylab="steps", layout=c(1,2))
```
