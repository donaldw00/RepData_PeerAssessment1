library(ggplot2)
library(dplyr)
library(lubridate)

#1. loading and processing data 
unzip("activity.zip")
activity <- read.csv("activity.csv")

activity$date <- as.Date.POSIXct(activity$date, "%Y%m%d") #format 'date' field from character class to date class

summary(activity)
#2. What is mean total number of steps taken per day?
tnsteps <- activity %>% group_by(date) %>% summarize(daily.steps = sum(steps))

tnsteps.plot <- ggplot(tnsteps,aes(daily.steps),) +
  geom_histogram() + ylim(c(0,10)) +
  ggtitle("Mean Total Steps per Day ") + xlab("Daily Steps") + ylab("Count") +
  theme_light() + geom_bar()

tnsteps.plot

daily.mean <- mean(tnsteps$daily.steps, na.rm = T)
daily.median <- median(tnsteps$daily.steps, na.rm = T)

daily.mean
daily.median

#3. What is the average daily activity pattern?
tnsteps.interval <- activity %>% group_by(interval) %>% summarize(mean.steps = mean(steps, na.rm = T))

daily.activity.plot <- ggplot(tnsteps.interval, aes(x = interval, y = mean.steps)) +
  geom_line() + ggtitle("Daily Activity Pattern") + xlab("Interval") + ylab("Number of Steps") +
  theme_light()

daily.activity.plot

#4. Imputing missing values
sum(is.na(activity$steps))

imputed.steps <- activity %>% mutate(
  steps = case_when(is.na(steps) ~ tnsteps.interval$mean.steps[match(activity$interval,tnsteps.interval$interval)],
                    T ~ as.numeric(steps)
                    ))
sum(is.na(imputed.steps$steps))

total.imputed.steps <- imputed.steps %>% group_by(date) %>% summarize(newdaily.steps = sum(steps))

total.imputed.steps.plot <- ggplot(total.imputed.steps,aes(newdaily.steps),) +
  geom_histogram() +
  ggtitle("Mean Total Steps per Day (N/As replaced)") + xlab("Daily Steps") + ylab("Count") +
  theme_light() + geom_bar()

total.imputed.steps.plot

daily.mean <- mean(total.imputed.steps$newdaily.steps, na.rm = T)
daily.median <- median(total.imputed.steps$newdaily.steps, na.rm = T)

daily.mean
daily.median

#5. Are there differences in activity patterns between weekdays and weekends?
imputed.steps$day <- weekdays(imputed.steps$date)
imputed.steps$period <- case_when(imputed.steps$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday") ~ "Weekday",
                                  imputed.steps$day %in% c("Saturday","Sunday") ~ "Weekend")

week.period <- imputed.steps %>% group_by(interval,period) %>% summarize(wd.steps = mean(steps, na.rm = T))

week.period.plot <- ggplot(week.period, aes(x = interval, y = wd.steps)) +
  geom_line() + ggtitle("Mean Steps per Interval") + xlab("Interval") + ylab("Number of Steps") +
  theme_light() + facet_wrap(~period, ncol = 1, nrow = 2)

week.period.plot
