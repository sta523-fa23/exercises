## Example 1

draw_points = function(n) {
  list(
    x = runif(n),
    y = runif(n)
  )
}

in_unit_circ = function(d) {
  sqrt(d$x^2 + d$y^2) <= 1
}

draw_points(1e5) |>
  in_unit_circ() |>
  sum() |>
  (function(x) {(4 * x) / 1e5})()

tibble(
  n = 10^(1:6)
) |>
  mutate(
    draws = map(n,  draw_points)
  ) |>
  View()

library(tidyverse)

tibble(
  disc = repurrrsive::discog
) |>
  mutate(
    id = map_int(disc, "id"),
    year = map_int(disc, c("basic_information", "year")),
    title = map_chr(disc, c("basic_information", "title")),
    artist1 = map_chr(disc, list("basic_information", "artists", 1, "name")),
    artist2 = map_chr(disc, list("basic_information", "artists", 2, "name"), .default = NA),
    label = map_chr(disc, list("basic_information", "labels", 1, "name"), .default = NA)
  )

tibble(
  disc = repurrrsive::discog
) |>
  hoist(
    disc,
    year    = c("basic_information", "year"),
    title   = c("basic_information", "title"),
    artist1 = list("basic_information", "artists", 1, "name"),
    artist2 = list("basic_information", "artists", 2, "name"),
    label   = list("basic_information", "labels", 1, "name")
  )




