wd <- file.path ("D:","Users","wbrandl","Coursera","ReproducableResearchCourseProject1")
setwd (wd)
if (!file.exists("data")){
  dir.create("data")
}
wd <- file.path ("D:","Users","wbrandl","Coursera","ReproducableResearchCourseProject1","data")
setwd (wd)
data <- read.csv(unz("repdataFdataFactivity.zip", "activity.csv"), header=T, quote="\"", sep=",")
library(ggplot2)
library(data.table)
datatable <- data.table(data)
total.steps <- datatable[steps>0,.(stepsperday = sum(steps)),by=.(date)]
qplot(total.steps$stepsperday, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps$stepsperday, na.rm=TRUE)
median(total.steps$stepsperday, na.rm=TRUE)

daily.averages <- datatable[!is.na(steps),.(steps=as.integer(mean(steps))),by=.(interval)]
ggplot(data=daily.averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

daily.averages[which.max(daily.averages$steps),]
missing <- is.na(data$steps)
# How many missing
table(missing)
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (daily.averages[daily.averages$interval==interval]$steps)
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)





setkey(daily.averages, interval)

datatable[is.na(steps),steps := as.integer(daily.averages[interval]$avragesday)]
