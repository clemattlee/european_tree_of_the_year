library(dplyr)
library(readr)
library(leaflet)
library(htmltools)


trees <- read_csv("data/european_tree_of_the_year_results.csv")


popup_html <- function(tree_name, year, place, species, age, region, score_raw, cover_photo_url, description) {
  sprintf(
    paste0(
      "<div style='width:260px;'>",
      "<h3>%s</h3>",
      "<img src='%s' style='width:100%%; height:auto; margin-bottom:8px;'>",
      "<p><b>Year:</b> %s<br>",
      "<b>Place:</b> %s<br>",
      "<b>Species:</b> %s<br>",
      "<b>Age:</b> %s<br>",
      "<b>Region:</b> %s<br>",
      "<b>Score:</b> %s</p>",
      "<p>%s</p>",
      "</div>"
    ),
    tree_name, cover_photo_url, year, place, species, age, region, score_raw, description
  )
}

trees <- trees |>
  mutate(
    popup = pmap_chr(
      list(tree_name, year, place, species, age, region, score_raw, cover_photo_url, description),
      popup_html
    )
  )

leaflet(trees) |>
  addTiles() |>
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~popup
  )
