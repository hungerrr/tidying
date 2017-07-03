library(tidyverse)
library(nycflights13)
# mezclar dos dataframes
#vuelos retrasados mas de dos horas
flights_delayed <- filter(flights, arr_delay > 120, dep_delay > 120 )
#join flights and airlines by carrier
flights_delayed <- left_join(x = flights_delayed, y = airlines, by = c("carrier", "carrier"))
#ahora tiene otra columna
flights_delayed
ggplot(data = flights_delayed) + geom_point(mapping = aes( x = dep_delay, y = arr_delay, color = name))
#improve readability reduce el comando se sustituye la x 
flights_delayed <- filter(flights, arr_delay > 120, dep_delay > 120 ) %>% left_join (y = airlines, by = c("carrier", "carrier"))
# el comando o funcion %>% es del paquete "magrittr" pero lo incluyen el tideverse
#Excercise


flight_speed <- 
  mutate(flights, 
         air_hours = air_time / 60, 
         speed = distance / air_hours, 
         month_name = month.name[month]) %>% 
  left_join(y = airlines, by = c("carrier", "carrier")) %>% 
  select(name, air_hours, speed, month_name)

# to create a histo
ggplot(data = flight_speed) + geom_histogram(mapping = aes(x = speed), bins = 100)
# to create the 
ggplot(data = flight_speed) + 
  geom_boxplot(mapping = aes(x = name, y = speed)) +
  theme(axis.text.x = element_text(angle = 90))
# Homework rewrite using pipes
#flight_speed <- 


