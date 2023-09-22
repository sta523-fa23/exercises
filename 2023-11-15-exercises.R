## Exercise 1

library(tidyverse)
library(nycflights13)

# How many flights to Los Angeles (LAX) did each of the legacy carriers 
# (AA, UA, DL or US) have in May from JFK, and what was their average duration?
  
flights |>
  mutate(carrier = as.factor(carrier)) |>
  filter(dest == "LAX") |>
  filter(month == 5) |>
  filter(origin == "JFK") |>
  filter(carrier %in% c("AA", "UA", "DL", "US")) |>
  summarize(
    n = n(),
    avg_dur = mean(air_time, na.rm=TRUE),
    .by = carrier
  )

# What was the shortest flight out of each airport in terms of distance?
# In terms of duration?

flights |>
  select(origin, dest, distance) |>
  group_by(origin) |>
  filter(!is.na(distance)) |>
  arrange(distance) |>
  distinct() |>
  slice_min(n=3, order_by=distance)

# Which plane (check the tail number) flew out of each New York airport the most?
  
# Which date should you fly on if you want to have the lowest possible average 
# departure delay? What about arrival delay?