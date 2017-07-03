install.packages("nycflights13")
library("nycflights13")
#load tidyverse
library(tidyverse)
filter(flights, month = 1, day = 1)
# it is necessary to use ==
filter(flights, month == 1, day == 1)
# If you want to use more conditions example Flights departure in Novermber or december
nov_dec <- filter(flights, month == 11 | month == 12)
# another option
nov_dec <- filter(flights, month %in% c(11, 12) )
# NA always give a NA answer to avoid this  Usein.na()
# Flights that were delayed on arrival or departure by more than two hours
not_delayed <- filter(flights, arr_delay > 120, dep_delay > 120 )
# to graph
ggplot(data = not_delayed) + geom_point(mapping = aes(x = arr_delay, y = dep_delay))
#command plot
ggplot(data = not_delayed) + geom_point(mapping = aes(x = arr_delay, y = dep_delay, color = carrier))
# the next command is the select columns by name
select(flights, year, month, day)
#select all columns between year and day
select(flights, year:day)
#select all columns except those from year to day (inlcusive)
select(flights, -(year:day) )
#select all columns starting with "arr" it can be ends, or contains, matches, num_range
select(flights, starts_with("arr"))
# rename columns
select(flights, arrival_time = arr_time)
# to keep the other columns we need to use rename
rename(flights, arrival_time = arr_time)
#to create new variables
mutate(flights, gain = arr_delay - dep_delay, air_hours = air_time / 60)
#using the variables that were created so it is no necessary to creat first and then use them. it is possible to do it at the same time.
flights <- mutate(flights, gain = arr_delay - dep_delay, air_hours = air_time / 60, gain_per_hour = gain / air_hours)
# to see part of the data you cannot see view but first give it a name 

# to keep only the new variables
transmutate(flights, gain = arr_delay - dep_delay, air_hours = air_time / 60, gain_per_hour = gain / air_hours)
#excercise. Create a new tibble with the speed
flights <- mutate(flights, speed = distance / air_hours)

#filter the flight time smaller than two houts and larger than hour 
flight_speed <- filter(flights, air_time > 60, dep_delay < 120 )
# Make a scatterplot of depl_delay vs speed
ggplot(data = flight_speed) + geom_point(mapping = aes(x = dep_delay, y = speed))

