library(dplyr)
library(readr)
library(leaflet)
library(htmltools)
library(purrr)
library(htmlwidgets)

setwd("C:/R Work/european_tree_of_the_year")

trees <- read_csv("data/european_tree_of_the_year_results.csv")

trees <- trees |>
  mutate(
    place_num = readr::parse_number(place),
    place_badge = case_when(
      place_num == 1 ~ "🥇",
      place_num == 2 ~ "🥈",
      place_num == 3 ~ "🥉",
      TRUE ~ "🌳"
    ),
    marker_html = case_when(
      place_num %in% 1:3 ~ paste0(
        "<div style='
          font-size: 26px;
          line-height: 26px;
          text-align: center;
          width: 30px;
          height: 30px;
        '>", place_badge, "</div>"
      ),
      TRUE ~ "<div style='
          font-size: 22px;
          line-height: 22px;
          text-align: center;
          width: 26px;
          height: 26px;
        '>🌳</div>"
    )
  )

popup_html <- function(tree_name, year, place, place_badge, species, age, region, score_raw, cover_photo_url, description) {
  sprintf(
    paste0(
      "<div style='width:260px;'>",
      "<h3>%s %s</h3>",
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
    place_badge, tree_name, cover_photo_url, year, place, species, age, region, score_raw, description
  )
}

trees <- trees |>
  mutate(
    popup = pmap_chr(
      list(tree_name, year, place, place_badge, species, age, region, score_raw, cover_photo_url, description),
      popup_html
    )
  )

tree_map <- leaflet(trees) |>
  addTiles() |>
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 6,
    stroke = FALSE,
    fillOpacity = 0,
    popup = ~popup
  ) |>
  addLabelOnlyMarkers(
    lng = ~lon,
    lat = ~lat,
    label = ~lapply(place_badge, htmltools::HTML),
    labelOptions = labelOptions(
      noHide = TRUE,
      textOnly = TRUE,
      direction = "center",
      style = list(
        "font-size" = "20px",
        "background" = "transparent",
        "border" = "none",
        "box-shadow" = "none"
      )
    )
  )

htmlwidgets::saveWidget(
  tree_map,
  "docs/index.html",
  selfcontained = FALSE
)
