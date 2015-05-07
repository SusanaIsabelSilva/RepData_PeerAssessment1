## Q1: Loading and preprocessing the data
## Show any code that is needed to
## 1. Load the data (i.e. read.csv())
## 2. Process/transform the data (if necessary) into a format suitable for your analysis.

activity <- read.csv(unz("activity.zip", "activity.csv"),header=TRUE)
str(activity)
activity$date <- as.Date(activity$date, '%Y-%m-%d')
summary(activity)

## Q2: What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values in the dataset.
## 1. Calculate the total number of steps taken per day
## 2. If you do not understand the difference between a histogram and a barplot, 
## research the difference between them. Make a histogram of the total number of steps taken each day
## 3. Calculate and report the mean and median of the total number of steps taken per day
activity_woNA <- activity[!is.na(activity$steps),]
summary(activity_woNA)
totalsteps <- with(activity_woNA,tapply(steps,date,sum))
hist(totalsteps)
mean(totalsteps)
median(totalsteps)

## Q3: What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval
## (x-axis) and the average number of steps taken, averaged across all days (y-axis)
## 2. Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
avgsteps <- with(activity_woNA,tapply(steps,interval,mean))
activity_internalmean <- data.frame(interval=as.integer(names(avgsteps)),avgsteps)
str(activity_internalmean)
head(activity_internalmean)
tail(activity_internalmean)
plot(x=activity_internalmean$interval,y=activity_internalmean$avgsteps,type="l")
activity_internalmean[which.max(activity_internalmean$avgsteps), ]

## Q4: Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). 
## The presence of missing days may introduce bias into some calculations or summaries of the data.
## 1. Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
## 2. Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the 
## mean/median for that day, or the mean for that 5-minute interval, etc.
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
## 4. Make a histogram of the total number of steps taken each day and Calculate and report the
## mean and median total number of steps taken per day. Do these values differ from the estimates 
## from the first part of the assignment? What is the impact of imputing missing data on the estimates 
## of the total daily number of steps?
activity_NA <- activity[is.na(activity$steps),]
nrow(activity_NA)
activity_auxiliar <- merge(activity_NA,activity_internalmean)
activity_auxiliar <- activity_auxiliar[order(activity_auxiliar$date,activity_auxiliar$interval),]
new_activity <- activity
new_activity[is.na(new_activity$steps),1] <- floor(activity_auxiliar$avgsteps)
summary(new_activity)
newtotalsteps <- with(new_activity,tapply(steps,date,sum))
hist(newtotalsteps)
mean(newtotalsteps)
median(newtotalsteps)

## Q5: Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.
## 1. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? 
## indicating whether a given date is a weekday or weekend day.
## 2. Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis). 
## See the README file in the GitHub repository to see an example of what this 
## plot should look like using simulated data.
new_activity <- data.table(new_activity)
new_activity[,weekday:=weekdays(new_activity$date)]
new_activity[new_activity$weekday=="Saturday" | new_activity$weekday=="Sunday",4] <- "weekend"
new_activity[new_activity$weekday!="weekend",4] <- "weekday"
str(new_activity)
new_activity$weekday <- as.factor(new_activity$weekday)
new_activity[,avgsteps:=mean(steps),by=.(interval,weekday)]
library(lattice)
p <- xyplot(avgsteps ~ interval | weekday, data=new_activity, 
            type = 'l', layout=c(1,2),
            main="Average number of steps taken across weekday or weekend days",
            xlab="5-Minute Interval",
            ylab="Number of steps")
print (p) 