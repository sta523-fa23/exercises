library(tidyverse)
library(rvest)

url = "https://www.rottentomatoes.com/"

session = polite::bow(url)


#page = read_html(url)

page = polite::scrape(session)




d = tibble(
  name = page |> 
    html_elements(".dynamic-text-list__streaming-links+ ul .dynamic-text-list__item-title") |>
    html_text(),
  
  rating = page |>
    html_elements(".dynamic-text-list__streaming-links+ ul .b--medium") |>
    html_text2() |>
    str_remove("%") |>
    as.integer(),
  
  freshness = page |>
    html_elements(".dynamic-text-list__streaming-links+ ul .icon--tiny") |>
    html_attr("class") |>
    str_remove("icon icon--tiny icon__") |>
    str_to_title(),
  
  url = page |>
    html_elements(".dynamic-text-list__streaming-links+ ul li a.dynamic-text-list__tomatometer-group") |>
    html_attr("href") |>
    (\(x) paste0(url, x))()
)


get_subpage = function(url) {
  message("Scraping: ", url, "\n")
  
  subpage = polite::nod(session, url) |>
    polite::scrape()
  
  tibble(
    mpaa_rating = subpage |> 
      html_element(".info-item:nth-child(1) span") |>
      html_text() |>
      str_remove(" \\(.*\\)") |>
      str_trim(),
    runtime = subpage |>
      html_elements(".info-item:nth-child(9) time") |>
      html_attr("datetime") |>
      str_remove_all("P|M")
  )
}

z = d |>
  mutate(
    sub = map(url, get_subpage)
  )

z |> unnest_wider(sub)








