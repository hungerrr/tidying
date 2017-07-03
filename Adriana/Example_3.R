flights_delayed<-filter(flights, arr_delay>120, dep_delay>120)
flights_delayed
flights_delayed<-left_join(x = flights_delated, y = airlines, by=c("carrier", "carrier"))
flights_delayed
ggplot(data = flights_delayed)+
  geom_point(mapping = aes(x = dep_delay, y = arr_delay, color = name))

#improve the readability of R code into one pipe
flights_delayed<- filter(flights, arr_delay>120, dep_delay>120) %>% 
  left_join( y = airlines, by=c("carrier", "carrier"))
flights_delayed
