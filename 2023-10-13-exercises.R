library(tidyverse)
library(httr2)

## Basics

jsonlite::read_json("https://www.anapioficeandfire.com/api/books/") |>
  tibble(books = _) |>
  unnest_wider(books) |>
  View()

c1 = jsonlite::read_json("https://www.anapioficeandfire.com/api/characters?pageSize=50") |>
  tibble(chars = _) |>
  unnest_wider(chars) |>
  View()

c2 = jsonlite::read_json("https://www.anapioficeandfire.com/api/characters?pageSize=50&page=2") |>
  tibble(chars = _) |>
  unnest_wider(chars) |>
  View()


## Iteration

full = list()
page = 1

repeat {
  message("Grabbing page ", page)
  
  chars = jsonlite::read_json(
    glue::glue("https://www.anapioficeandfire.com/api/books?pageSize=50&page={page}")
  )
  
  if (length(chars) == 0)
    break
  
  full = c(full, chars)
  page = page+1
}

full |>
  tibble(chars = _) |>
  unnest_wider(chars) |>
  View()




## httr2

req = request("https://www.anapioficeandfire.com/api/characters") |>
  req_url_query(page=2, pageSize=50)

req_dry_run(req)

resp = req_perform(req)

get_next = function(resp) {
  resp_header(resp, "link") |>
    str_match_all("<(.*?)>; rel=\"next\"") |>
    (\(x) x[[1]][,2])() 
}

get_next(resp)


full = list()
page = 1

req = request("https://www.anapioficeandfire.com/api/characters") |>
  req_url_query(page=1, pageSize=50)

resp = req_perform(req)

repeat {
  message("Grabbing page ", page)
  
  full = c(full, resp_body_json(resp))
  link = get_next(resp) 
  
  if (length(link) == 0)
    break
  
  print(link)
  
  resp = req_url(req, link) |>
    req_perform()
  
  page = page+1
}

full |>
  tibble(chars = _) |>
  unnest_wider(chars) |>
  View()



## GitHub

request("http://api.github.com/user") |>
  req_auth_bearer_token("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") |>
  req_perform() |>
  resp_body_json()



request("http://api.github.com/orgs/sta523-fa23/repos") |>
  req_auth_bearer_token("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") |>
  req_perform() |>
  resp_body_json()


