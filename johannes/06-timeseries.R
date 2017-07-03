library(tidyverse)
library(lubridate)
library(nycflights13)


ymd("84-6-26")

mdy("Jun 26th, 1984")

dmy("26-Jun-1984")

ymd_hms("2017-06-26 14:00:00")


flights %>% select(year, month, day, hour, minute) %>% mutate(make_datetime(year, month, day, hour, minute))


# Convert all time information to dates 
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100),
    arr_time = make_datetime(year, month, day, arr_time %/% 100, arr_time %% 100),
    sched_dep_time = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100),
    sched_arr_time = make_datetime(year, month, day, sched_arr_time %/% 100, sched_arr_time %% 100)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% ggplot(aes(dep_time)) + geom_freqpoly(binwidth=86400) #seconds per day

flights_dt %>% filter(dep_time < ymd("2013-01-02")) %>% ggplot(aes(dep_time)) + geom_freqpoly(binwidth=600)


flights_dt %>% filter(dep_time >= ymd_hms("2013-01-01 00:00:00"), dep_time < ymd_hms("2013-01-02 00:00:00")) %>% ggplot(aes(dep_time)) + geom_freqpoly(binwidth=600)

flights_dt %>% filter(dep_time >= ymd("2013-01-01"), dep_time < ymd("2013-01-02")) %>% ggplot(aes(dep_time)) + geom_freqpoly(binwidth=600)


d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

d1 <- mdy("January 1, 2010")
d2 <- ymd("2015-Mar-07")
d3 <- dmy("06-Jun-2017")
d4 <- mdy(c("August 19 (2015)", "July 1 (2015)"))
d5 <- mdy("12/30/14") # Dec 30, 2014

# Getting components from dates 
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)  # year 
month(datetime) # month 
mday(datetime)  # day of the month 
yday(datetime)  # day of the year 
wday(datetime)  # day of the week  

month(datetime, label=TRUE)
month(datetime, label=TRUE, abbr=FALSE)


flights_dt %>% mutate(wday=wday(dep_time, label=TRUE)) %>% ggplot(aes(x=wday)) + geom_bar()

flights_dt %>% mutate(
  wday = wday(dep_time, label = TRUE),
  hday = hour(dep_time),
  period = ifelse(hday > 12, "morning", "afternoon"),
  period = ifelse(hday > 18, "evening", period)
) %>% ggplot(aes(x = wday, fill = period)) + geom_bar()


flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

