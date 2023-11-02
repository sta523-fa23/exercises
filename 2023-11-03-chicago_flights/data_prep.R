# Data prep

# devtools::install_github("cpsievert/chiflights22")

library(tidyverse)

to_hm = function(x) {
  paste(
    floor(x / 100),
    x - floor(x / 100) * 100,
    sep=":"
  ) |> lubridate::hm()
}

flights <- chiflights22::flights |>
  left_join(
    chiflights22::airports |>
      transmute(dest_name = paste0(name, " (", faa, ")"), dest = faa, end_lat = lat, end_lon = lon)
  ) |>
  filter(!is.na(dest_name)) |>
  left_join(
    chiflights22::airports |> select(origin = faa, start_lat = lat, start_lon = lon)
  ) |>
  left_join(
    chiflights22::airlines, 
    by = "carrier"
  ) |>
  rename(carrier_name = name) |>
  select(-time_hour) |>
  left_join(chiflights22::weather) |>
  mutate(
    precip = scales::rescale(precip^-1.55/-.55),
    date = lubridate::ymd(paste(year, month, day, sep = "-"))#,
    #sched_dep_time = to_hm(sched_dep_time),
    #sched_arr_time = to_hm(sched_arr_time)
  )

saveRDS(flights,"flights.rds")
