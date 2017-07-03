#----------------------------------
#Third script - Exercise
#Aline Soterroni
#29 may, 2017
#----------------------------------

library(tidyverse)
library(nycflights13)

#Given the lines below (11 to 22) rewrite the code using pipes
# flight_speed <- mutate(flights,
#                        air_hours = air_time/60,
#                        speed = distance / air_hours,
#                        month_name = month.name[month]
# )
# 
# flight_speed <-
#   left_join(x=flight_speed,y=airlines,by=c("carrier","carrier"))
# 
# flight_speed <- select(flight_speed,name,air_hours,speed,month_name)
# 
# flight_speed <- filter(flight_speed,air_hours <2, air_hours >1)


#This is the code above (lines 11 to 22) using pipes
flight_speed <-
  mutate(flights,
         air_hours = air_time/60,
         speed = distance / air_hours,
         month_name = month.name[month]) %>%
  left_join(y=airlines,by=c("carrier","carrier")) %>%
  select(name,air_hours,speed,month_name) %>%
  filter(air_hours <2, air_hours >1)


#Visualizing the data

#Histogram of speed using 100 bins
ggplot(data=flight_speed) +
  geom_histogram(mapping = aes(x=speed), bins=100)

#Box plot of speed and the airline names
ggplot(data=flight_speed) +
  geom_boxplot(mapping = aes(x=name,y=speed)) +
  theme(axis.text.x=element_text(angle=90))




