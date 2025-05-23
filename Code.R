# Import required packages
library(dplyr)
library(readr)

# Load the data
flights <- read_csv("data/flights2022-h2.csv")
airlines <- read_csv("data/airlines.csv")
airports <- read_csv("data/airports.csv")

# Start your code here!

head(flights)
head(airlines)
head(airports)

# Which airline and airport pair receives the most flights from NYC and what is the average duration of that flight?
AP <- airports %>% filter(tzone == "America/New_York") %>% select(faa) %>% pull()

# creating a frecuency table with the airlines, airports and the counts 
tabla <- flights %>% filter(origin %in% AP) %>% group_by(carrier) %>% summarise(index = which.max(table(dest)), coun = max(table(dest))) %>% ungroup() %>% 
  mutate(airport_name = names(index))

# finding the pair of airline and airport with the highest frecuency
maximo <- tabla[which.max(tabla$coun),]

# finding the names of the airline and airport
airline_name <- airlines %>% filter(carrier == maximo$carrier) %>% select(name) %>% pull()
airport_name <- airports %>% filter(faa == maximo$airport_name) %>% select(name) %>% pull()

# calculing the average of the time in the air of the pair of airline and airport with the highest frecuency
avg <- flights %>% filter(origin %in% AP, carrier == maximo$carrier, dest == maximo$airport_name) %>% summarise(ave = round(mean(air_time, na.rm = TRUE)))

# creating the object frequent
frequent <- data.frame(airline_name = airline_name, airport_name = airport_name, avg = avg)



# Find the airport that has the longest average flight duration (in hours) from NYC. What is the name of this airport?

# creating a table with the average of time in the air for each pair of airline and airport
tabla_2 <- flights %>% filter(origin %in% AP) %>% mutate(h_in_air = air_time/60) %>% group_by(carrier, dest) %>% 
  summarise(y = mean(h_in_air, na.rm = TRUE)) %>% ungroup()

# finding the maximun average
maximo_2 <- tabla_2[which.max(tabla_2$y),]

# finding the names of airport and airline which havving the maximun average
airline_name_2 <- airlines %>% filter(carrier == maximo_2$carrier) %>% select(name) %>% pull()
airport_name_2 <- airports %>% filter(faa == maximo_2$dest) %>% select(name) %>% pull()

# creating the object longest
longest <- data.frame(airline_name = airline_name_2, airport_name = airport_name_2, avg = maximo_2$y)


# Which airport is the least frequented destination for flights departing from JFK?

# creating a frecuency table 
air_2 <- flights %>% filter(origin == "JFK") %>% mutate(h_in_air = air_time/60) %>% 
  group_by(dest) %>% summarise(y = table(dest))

# Finding the airport with the least frecuency
tabla_3 <- air_2[which.min(air_2$y),]
least <- airports %>% filter(faa == tabla_3$dest) %>% select(name) %>% pull()
