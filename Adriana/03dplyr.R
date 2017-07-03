filter() #subset observations 
select()#select columns
rename()#column names 
library(nycflights13)
flights
#select all flights on January 1st
filter(flights, month==1,day==1)
#flights that departed in November or December 
nov_dec<-filter(flights, month==11|month==12)
nov_dec
#flights that departed in November or December 
nov_dec<-filter(flights, month %in% c(11,12))
nov_dec
#missing values
is.na()
#flights that were delayed on arrival or departure by more than two hours 
not_delayed <- filter(flights, arr_delay > 120, dep_delay>120)

ggplot(data = not_delayed)+
  geom_point(mapping = aes(x = dep_delay, y = arr_delay, color = carrier))
#select columns by name
select(flights, year, month, day)
#selec all columns between year and day
select(flights, year:day)
#select all columns except those from year to day(inclusive)
select(flights, -(year:day))
#select all columns starting with arr
select(flights, starts_with("arr"))
#other options
ends_with()
contains()
matches()
num_range()

#rename columns using select
select(flights, arrival_time = arr_time)
#rename columns using rename
rename(flights, arrival_time = arr_time)
#add gain and hours
mutate(flights, gain = arr_delay - dep_delay, air_hours= air_time/60)
#using variables that you just created
q<-mutate(flights, gain = arr_delay - dep_delay, air_hours= air_time/60, gain_per_hour= gain/air_hours)
View(q)

s<-mutate(flights, speed = distance/air_time*60)
ss <- filter(s,air_time>60, air_time<120)
ss


ggplot(data = ss)+
  geom_point(mapping = aes(x = dep_delay, y = speed, color = carrier))