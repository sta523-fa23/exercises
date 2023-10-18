library(rvest)
library(tidyverse)

## Denny's nomnom API 

paste0(
  "https://nomnom-prod-api.dennys.com/restaurants/near",
  "?lat=35.780398&long=-78.639099&radius=100&limit=1000",
  "&nomnom=calendars&nomnom_calendars_from=20231017",
  "&nomnom_calendars_to=20231025&nomnom_exclude_extref=999"
) |>
  jsonlite::read_json() |>
  View()

## Denny's restaurant page scraping 

read_html("https://locations.dennys.com/NY/CORTLAND/248853") |>
  html_nodes("span.coordinates > meta") |>
  html_attr("content") |>
  unique()


dir.create("data/dennys", showWarnings = FALSE, recursive = TRUE)


url = "https://locations.dennys.com/NY/CORTLAND/248853"

message("Processing ", basename(url))
download.file(
  url = url, 
  destfile = file.path(
    "data/dennys",
    paste0(basename(url), ".html")
  ),
  quiet = TRUE
)


## Robust read html


readr::read_lines("https://locations.dennys.com/NY/CORTLAND/248853") |>
  paste(collapse="\n") |>
  read_html()

