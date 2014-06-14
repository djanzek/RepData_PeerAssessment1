activity <- read.csv("activity.csv")
tail(activity)
head(activity)

sum(is.na(activity$steps))
sum(is.na(activity$date))
sum(is.na(activity$interval))

totalSteps <- aggregate(steps~date, data=activity, sum, na.rm=TRUE)
totalSteps
hist(totalSteps$steps, xlab="Total Steps", main="Total Number of Steps")
mean(totalSteps$steps)
median(totalSteps$steps)

stepsInterval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
stepsInterval
plot(steps~interval, data=stepsInterval, type="l", 
     main="Average Daily Activity Pattern", xlab="Interval", ylab="Steps")

max(stepsInterval$steps)
stepsInterval[which.max(stepsInterval$steps),]$interval


intervalToSteps <- function(interval){
      stepsInterval[stepsInterval$interval==interval,]$steps
}

activityFilled <- activity

count = 0
for(i in 1:nrow(activityFilled)){
      if(is.na(activityFilled[i,]$steps)){
            activityFilled[i,]$steps<-intervalToSteps(activityFilled[i,]$interval)
            count = count + 1
      }
}
cat("Total ", count, "NA values were filled.\n\r")

totalSteps2 <- aggregate(steps~date, data=activityFilled, sum)
totalSteps2
hist(totalSteps2$steps, xlab="Total Steps", main="Total Number of Steps")
mean(totalSteps2$steps)
median(totalSteps2$steps)

activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend", "weekday")
activityFilled$day = factor(activityFilled$day, levels=c("weekday", "weekend"))

stepsInterval2 = aggregate(steps~interval+day, activityFilled, mean)

library(lattice)
xyplot(steps~interval|factor(day), data=stepsInterval2, aspect=1/2, type="l")