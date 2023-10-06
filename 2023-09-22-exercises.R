library(tidyverse)

## Example 1

grades = tibble::tribble(
  ~name,   ~hw_1, ~hw_2, ~hw_3, ~hw_4, ~proj_1, ~proj_2,
  "Alice",    19,    19,    18,    20,      89,      95,
  "Bob",      18,    20,    18,    16,      77,      88,
  "Carol",    18,    20,    18,    17,      96,      99,
  "Dave",     19,    19,    18,    19,      86,      82
)

grades |>
  pivot_longer(
    -name, names_to = "assignment", values_to = "score"
  ) |>
  separate_wider_delim(
    assignment, delim="_", names = c("type","index")
  ) |>
  summarize(
    avg_score = mean(score),
    .by = c(name, type)
  ) |>
  pivot_wider(
    id_cols = name, names_from = type, values_from = avg_score
  ) |>
  mutate(
    overall = 0.5 * hw / 20 + 0.5 * proj / 100
  ) |>
  arrange(desc(overall))


## Exercise 1

palmerpenguins::penguins |>
  count(island, species) |>
  pivot_wider(
    id_cols = island, names_from = species, values_from = n,
    values_fill = 0
  )

palmerpenguins::penguins |>
  count(island, species) |>
  pivot_wider(
    id_cols = species, names_from = island, values_from = n,
    values_fill = 0
  )



## Exercise 2

library(repurrrsive)

## 1. Which planet appeared in the most starwars film (according to the data in sw_planets)?

tibble::tibble(
  planet = sw_planets 
) |>
  unnest_wider(planet) |>
  select(name, url, films) |>
  unnest_longer(films) |>
  count(name) |>
  arrange(desc(n))

  
## 2. Which planet was the homeworld of the most characters in the starwars films?

inner_join(
  tibble::tibble(
    char = sw_people
  ) |>
    unnest_wider(char) |>
    select(name, homeworld),
  
  tibble::tibble(
    planet = sw_planets 
  ) |>
    unnest_wider(planet) |>
    select(planet = name, url),
  by = c("homeworld" = "url")
) |>
  select(-homeworld) |>
  count(planet) |>
  arrange(desc(n))

