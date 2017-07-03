#----------------------------------
#Fifth script - Exercise
#Aline Soterroni
#19 June, 2017
#----------------------------------

library(tidyverse)

#create tibble from existing data.frame
as.tibble(iris)

#create tibble from individuals vectors
tibble(
  x=1:5,
  y=1,  #R repeat values
  z=x^2+y
)

#create tibble by rows
tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)

#variable names
tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)

#select a variable
tb %>%
  select(`:)`)

#extract variable by name
tb %>%
  .$`:)`

#------------------
#reading csv
#------------------

#example heights
heights_1 <- read_csv("data/heights.csv")   #read as tibble
heights_2 <- read.csv("data/heights.csv")   #read as data.frame

class(heights_1)
class(heights_2)

#data.frame converts characters to factor, tibble does not
class(heights_1$race)
class(heights_2$race)


#example challenge 
challenge <- read_csv(readr_example("challenge.csv"))

#show problems
problems(challenge)

#trying to solve the 'problems'
#supplying colum types
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x=col_double(),
    y=col_character()
  )
)

#last observations in challenge (because it is big dataset - ver só o final)
tail(challenge)

#data as data type
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x=col_double(),
    y=col_date()
  )
)

tail(challenge)

#-------------------
#write csv
#-------------------
write_csv(challenge, "data/challenge.csv")





