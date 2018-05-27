---
title: "Project 1"
output: html_document
---

Unzip data to Obtain csv file

```{r,echo = FALSE}
unzip("activity.zip",exdir = "data")
```

Reading the data into activuity data frame and show some summary statistics
```{r,echo = FALSE}
activity <- read.csv("data/activity.csv", stringsAsFactors=FALSE)
str(activity)
summary(activity)
```

Conver date to POSIX class using lubridate pakage and convert interval to hour:minute 
```{r,echo = FALSE}
format
library(lubridate)
activity$date <- ymd(activity$date)
str(activity)
```

## Whats the average daily activity pattern?
1. Calculate the number of steps taken per day (Ignore the missin values)
```{r,echo = FALSE}
require(dplyr)
total_day <- activity %>% group_by(date) %>%summarise(total_steps=sum(steps,na.rm=TRUE),na=mean(is.na(steps))) %>% print
```

Visualise the total number of steps taken per day as a barplot
```{r, echo = FALSE}
barplot(height = total_day$total_steps,names.arg=total_day$date,cex.names=0.68,las=3,col="orange")
abline(h=median(total_day$total_steps), lty=2,lwd=3, col="black")
abline(h=mean(total_day$total_steps), lty=2,lwd=3, col="red")
text(x = 0,y=median(total_day$total_steps),pos=3,labels = "median")
text(x = 0,y=mean(total_day$total_steps),pos=1,labels = "mean",col="red")
```

2. Make a histogram of the total number of steps taken each day Histogram does not contain days where all observations are missing (i.e. there have to be a number of steps for at least one interval for that day, to be included). Otherwise, there would be about ten days with 0 steps.
```{r, echo = FALSE}
total_day <- filter(total_day, na < 1)
hist(total_day$total_steps,col="orange",breaks=20,main="Total steps per day",xlab="Steps per day")
abline(v=median(total_day$total_steps),lty=3, lwd=2, col="black")
legend(legend="median","topright",lty=3,lwd=2,bty = "n")
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r, echo = FALSE}
mean_steps <- mean(total_day$total_steps,na.rm=TRUE)
median_steps <- median(total_day$total_steps,na.rm=TRUE)
```

Mean and median of the total number of steps taken per day are r `round(mean_steps,2) steps and r median_steps steps`, respectively.

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, echo = FALSE}
library(dplyr,quietly = TRUE)
daily_patterns <- activity %>% group_by(interval) %>% summarise(average=mean(steps,na.rm=TRUE))
plot(x = 1:nrow(daily_patterns),y = daily_patterns$average,type = "l",
     col = "red", xaxt = "n",xlab="Intervals", 
     ylab = "Average for given interval across all days")
axis(1,labels=daily_patterns$interval[seq(1,288,12)],
     at = seq_along(daily_patterns$interval)[seq(1,288,12)])
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo = FALSE}
max_numb_steps_interval <- filter(daily_patterns,average==max(average))
```

Interval `r max_numb_steps_interval$interval` contains on average the maximum number of steps `(r round(max_numb_steps_interval$average,2) steps)`.


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r, echo = FALSE}
na_number <- sum(is.na(activity$steps))
na_number
percentage_na <- mean(is.na(activity$steps))
percentage_na
```


Total number of missing values in the dataset amounts to `r na_number` ** (what is * r `round(percentage_na100,1)` % of total observations).

2. Devise a strategy for filling in all of the missing values in the dataset
As the number of missing values in this dataset is fairly large, we cannot be sure if there is no bias introduced by missing values. Therefore we impute missing values based on average number of steps in particular 5-minutes interval.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r, echo = FALSE}
without_NAs <- numeric(nrow(activity))
for (i in 1:nrow(activity))
{
    if (is.na(activity[i,"steps"])==TRUE)
    {
        without_NAs[i]<-filter(daily_patterns,interval==activity[i,"interval"]) %>% select(average)
    } 
    else
    {
        without_NAs[i]<-activity[i,"steps"]
    }
    
}
activity_without_NAs<-mutate(activity,steps_no_NAs=without_NAs)
head(activity_without_NAs)
```

Below code is just to verify if process of imputing missing values correctly preserved original values (lines with no NAs)

```{r, echo = FALSE}
check <- filter(activity_without_NAs,!is.na(steps)) %>% mutate(ok = (steps==steps_no_NAs))
mean(check$ok)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```{r, echo = FALSE}
total_day_noNAs <- activity_without_NAs %>% mutate(steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(date) %>% summarise(total_steps=sum(steps_no_NAs))
hist(total_day_noNAs$total_steps,col="blue",breaks=20,main="Total steps per day",xlab="Steps per day")
abline(v=median(total_day$total_steps),lty=3, lwd=2, col="black")
legend(legend="median","topright",lty=3,lwd=2,bty = "n")
```

```{r, echo = FALSE}
summary(total_day_noNAs$total_steps)
```

Imputing missing values, mean of the total number of steps taken per day increased while median decreased,compared to estimates from the first part (ingoring missing values). Imputing missing data resulted in increase of total daily number of steps (instead of each NAs we have average that is always >=0)

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```{r, echo = FALSE}
library(lubridate)
is_weekday <-function(date){
    if(wday(date)%in%c(1,7)) result<-"weekend"
    else
        result<-"weekday"
    result
}

activity_without_NAs <- mutate(activity_without_NAs,date=ymd(date)) %>% mutate(day=sapply(date,is_weekday))
table(activity_without_NAs$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```{r, echo = FALSE}
library(ggplot2)
daily_patterns <- activity_without_NAs %>% mutate(day=factor(day,levels=c("weekend","weekday")),steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(interval,day) %>% summarise(average=mean(steps_no_NAs))
qplot(interval,average,data=daily_patterns,geom="line",facets=day~.)
```