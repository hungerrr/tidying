

library(tidyverse)

# mpg - Fuel economy data from 1999 and 2008 for 38 popular models of car
# default 
mpg

# Print all rows and columns 
mpg %>% 
  print(n = Inf, width = Inf)

# or View all rows and columns 
mpg %>% 
  View()

# extract variable by name 
mpg$model

# or using pipe 
mpg %>% 
  .$model

# extract variable by position  
mpg[[2]]

# or using pipe 
mpg %>% 
  .[[2]]


# create tibble from existing data.frame 
# Edgar Anderson's Iris Data, for details see ?iris
as_tibble(iris)

# create tibble from individuals vectors
tibble(
  x = 1:5,
  y = 1,
  z = x ^2 + y
)

# create tibble by rows 
tribble(
  ~x, ~y,  ~z,
  "a", 2,  3.6,
  "b", 1,  8.5
)

# variable names 
tb <- tibble(
  `:)`   = "smile",
  ` `    = "space",
  `2000` = "number"
)

# select a variable 
tb %>% 
  select(`:)`)

# extract variable by name 
tb %>%
  .$`:)`

# Reading CSV
heights <- read_csv("data/heights.csv")

# What is the difference between heights_1 and heights_2? 

heights_1 <- read_csv("data/heights.csv")

heights_2 <- read.csv("data/heights.csv")

class(heights_1)

class(heights_2)

class(heights_1$race)

class(heights_2$race)


# Example challenge 
challenge <- read_csv(readr_example("challenge.csv"))

# Show problems 
problems(challenge)

# Supplying column types  
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

# Show problems 
problems(challenge)

# Print data 
challenge

# Last observations in challenge
tail(challenge)

# Date as data type 
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

tail(challenge)

# Write csv 
write_csv(challenge, "data/challenge.csv")



