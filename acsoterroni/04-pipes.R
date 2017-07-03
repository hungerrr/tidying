#----------------------------------
#Fourth script
#Aline Soterroni
#29 may, 2017
#----------------------------------

library(tidyverse)
library(nycflights13)
flights

flights_delayed <- filter(flights, arr_delay > 120, dep_delay > 120)

#Command to join two tibbles com uma informação em comum
#O comando abaixo vai criar a coluna name <chr> no 'flights_delayed'
flights_delayed <- left_join(x=flights_delayed, y=airlines, by = c("carrier","carrier"))

#Ao plotar o scatterplot passando como argumento 'name' para 'color', os nomes das companias aparecerão
#Antes desse 'join' apareciam apenas siglas
ggplot(data = flights_delayed) + 
  geom_point(mapping = aes (x = dep_delay, y = arr_delay, color = name))

#Improve the readability of R code with the pipe %>%
#Commands of the lines 11 and 15 above can be combined as following:
flights_delayed <- filter(flights, arr_delay > 120, dep_delay > 120) %>%
  left_join(y=airlines, by = c("carrier","carrier"))





