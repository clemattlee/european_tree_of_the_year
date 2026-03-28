library(rvest)
library(xml2)
library(httr2)
library(stringr)
library(dplyr)
library(purrr)
library(tibble)

base_url <- "https://www.treeoftheyear.org"

get_html <- function(url) {
  request(url) |>
    req_user_agent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0 Safari/537.36"
    ) |>
    req_timeout(60) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_html()
}

abs_url <- function(x, base = base_url) {
  ifelse(is.na(x) | x == "", NA_character_, url_absolute(x, base))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

extract_field <- function(page_text, field) {
  m <- str_match(page_text, paste0("(?i)", field, ":\\s*([^\\n\\r]+)"))
  ifelse(is.na(m[, 2]), NA_character_, str_squish(m[, 2]))
}

year_page_cache <- new.env(parent = emptyenv())

get_year_page <- function(year) {
  key <- as.character(year)
  if (!exists(key, envir = year_page_cache, inherits = FALSE)) {
    year_url <- paste0(base_url, "/previous-years/", year)
    assign(key, get_html(year_url), envir = year_page_cache)
  }
  get(key, envir = year_page_cache, inherits = FALSE)
}

download_cover_photo <- function(img_url,
                                 dest_folder = "docs/photos",
                                 tree_name = NULL,
                                 year = NULL,
                                 tree_url = NULL) {
  if (is.na(img_url) || img_url == "") return(NA_character_)
  
  dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)
  
  img_url_clean <- str_remove(img_url, "\\?.*$")
  ext <- tools::file_ext(img_url_clean)
  if (ext == "") ext <- "jpg"
  
  safe_name <- tree_name |>
    tolower() |>
    gsub("[^a-z0-9]+", "_", x = _) |>
    gsub("^_|_$", "", x = _)
  
  slug <- basename(tree_url %||% "")
  slug <- gsub("[^a-zA-Z0-9_-]", "_", slug)
  
  file_name <- paste0(year, "_", safe_name, "_", slug, ".", ext)
  dest_path <- file.path(dest_folder, file_name)
  
  tryCatch({
    download.file(img_url, destfile = dest_path, mode = "wb", quiet = TRUE)
    dest_path
  }, error = function(e) {
    NA_character_
  })
}

extract_candidate_image <- function(node) {
  nodes <- html_elements(node, "img, [style*='background-image'], [data-src], [data-background], [data-bg]")
  
  if (length(nodes) == 0) return(NA_character_)
  
  img_tbl <- tibble(
    src = html_attr(nodes, "src"),
    data_src = coalesce(
      html_attr(nodes, "data-src"),
      html_attr(nodes, "data-background"),
      html_attr(nodes, "data-bg")
    ),
    srcset = html_attr(nodes, "srcset"),
    data_srcset = html_attr(nodes, "data-srcset"),
    style = html_attr(nodes, "style"),
    alt = html_attr(nodes, "alt"),
    width = suppressWarnings(as.numeric(html_attr(nodes, "width"))),
    height = suppressWarnings(as.numeric(html_attr(nodes, "height")))
  ) |>
    mutate(
      raw_url = coalesce(src, data_src),
      raw_url = if_else(is.na(raw_url) | raw_url == "", srcset, raw_url),
      raw_url = if_else(is.na(raw_url) | raw_url == "", data_srcset, raw_url),
      raw_url = if_else(
        (is.na(raw_url) | raw_url == "") & !is.na(style),
        str_match(style, "background-image\\s*:\\s*url\\((['\"]?)(.*?)\\1\\)")[,3],
        raw_url
      ),
      raw_url = str_trim(raw_url),
      raw_url = str_split(raw_url, ",", simplify = TRUE)[,1],
      raw_url = str_remove(raw_url, "\\s+\\d+w$"),
      raw_url = str_remove(raw_url, "\\s+\\d+x$"),
      raw_url = str_remove(raw_url, "\\?.*$"),
      url_lower = str_to_lower(coalesce(raw_url, "")),
      alt_lower = str_to_lower(coalesce(alt, "")),
      area = coalesce(width, 0) * coalesce(height, 0),
      bad = str_detect(url_lower, "logo|partner|footer|icon|facebook|instagram|share") |
        str_detect(alt_lower, "logo|partner|tree of the year"),
      good = str_detect(url_lower, "\\.(jpg|jpeg|png|webp)$|/upload|/uploads|/media|/storage")
    ) |>
    filter(!is.na(raw_url), raw_url != "", !bad) |>
    distinct(raw_url, .keep_all = TRUE) |>
    arrange(desc(good), desc(area))
  
  if (nrow(img_tbl) == 0) return(NA_character_)
  
  abs_url(img_tbl$raw_url[1])
}

extract_cover_image_from_year_page <- function(year_page, tree_url) {
  rel_url <- sub("^https?://[^/]+", "", tree_url)
  
  links <- html_elements(year_page, "a")
  hrefs <- html_attr(links, "href")
  
  hit <- which(hrefs %in% c(tree_url, rel_url))
  
  if (length(hit) == 0) return(NA_character_)
  
  target_link <- links[[hit[1]]]
  ancestors <- xml2::xml_find_all(target_link, "ancestor::*")
  
  if (length(ancestors) == 0) return(NA_character_)
  
  for (node in rev(ancestors)) {
    img_url <- extract_candidate_image(node)
    if (!is.na(img_url)) return(img_url)
  }
  
  NA_character_
}

extract_tree_page <- function(url, year = NA_integer_) {
  Sys.sleep(1)
  
  page <- get_html(url)
  page_text <- html_text2(page)
  txt <- str_squish(page_text)
  
  tree_name <- html_element(page, "h1") |> html_text2() |> str_squish()
  
  paragraphs <- html_elements(page, "p") |> html_text2() |> str_squish()
  paragraphs <- paragraphs[nchar(paragraphs) > 80]
  description <- if (length(paragraphs) > 0) paragraphs[1] else NA_character_
  
  species <- extract_field(page_text, "Species")
  age     <- extract_field(page_text, "Age")
  region  <- extract_field(page_text, "Region")
  gps     <- extract_field(page_text, "GPS")
  
  score_match <- str_match(txt, "(\\d+[\\d,.]*\\s*(?:votes|tree points))")
  score_raw <- score_match[,2]
  
  place_match <- str_match(txt, "(\\d+\\s*(?:st|nd|rd|th)\\s*place)")
  place <- place_match[,2]
  
  year_page <- get_year_page(year)
  cover_photo_url <- extract_cover_image_from_year_page(year_page, url)
  
  tibble(
    year = year,
    tree_name = tree_name,
    description = description,
    species = species,
    age = age,
    region = region,
    gps = gps,
    score_raw = score_raw,
    place = place,
    cover_photo_url = cover_photo_url,
    tree_url = url
  )
}

extract_year_links <- function(year) {
  year_page <- get_year_page(year)
  
  hrefs <- html_elements(year_page, "a") |>
    html_attr("href") |>
    unique()
  
  hrefs <- hrefs[!is.na(hrefs)]
  hrefs <- hrefs[str_detect(hrefs, paste0("^/previous-years/", year, "/[^/]+$"))]
  
  tibble(
    year = as.integer(year),
    tree_url = abs_url(hrefs)
  ) |>
    distinct()
}

safe_extract_tree_page <- possibly(
  extract_tree_page,
  otherwise = tibble(
    year = NA_integer_,
    tree_name = NA_character_,
    description = NA_character_,
    species = NA_character_,
    age = NA_character_,
    region = NA_character_,
    gps = NA_character_,
    score_raw = NA_character_,
    place = NA_character_,
    cover_photo_url = NA_character_,
    tree_url = NA_character_
  )
)



all_years <- 2011:2026
all_results_list <- vector("list", length(all_years))

for (i in seq_along(all_years)) {
  yr <- all_years[i]
  message("Scraping year: ", yr)
  
  year_links <- extract_year_links(yr)
  
  year_results <- pmap_dfr(
    year_links,
    \(year, tree_url) safe_extract_tree_page(tree_url, year)
  )
  
  all_results_list[[i]] <- year_results
  
  partial_results <- bind_rows(all_results_list)
  
  readr::write_excel_csv(
    partial_results,
    "data/european_tree_of_the_year_results_partial.csv"
  )
}

all_results <- bind_rows(all_results_list) |>
  mutate(
    score_value = str_extract(score_raw, "\\d+[\\d,.]*"),
    score_value = str_remove_all(score_value, ","),
    score_value = as.numeric(score_value),
    score_type = case_when(
      str_detect(score_raw, "tree points") ~ "tree_points",
      str_detect(score_raw, "votes") ~ "votes",
      TRUE ~ NA_character_
    ),
    place_num = as.integer(str_extract(place, "^\\d+"))
  ) |>
  rowwise() |>
  mutate(
    cover_photo_file = download_cover_photo(
      img_url = cover_photo_url,
      tree_name = tree_name,
      year = year,
      tree_url = tree_url
    )
  ) |>
  ungroup()

convert_gps <- function(gps) {
  if (is.na(gps) || gps == "") return(c(NA, NA))
  
  parts <- str_split(gps, ",\\s*")[[1]]
  
  convert_part <- function(part) {
    nums <- str_extract_all(part, "[0-9.]+")[[1]]
    
    if (length(nums) < 3) return(NA)
    
    deg <- as.numeric(nums[1])
    min <- as.numeric(nums[2])
    sec <- as.numeric(nums[3])
    
    dec <- deg + min/60 + sec/3600
    
    if (str_detect(part, "S|W")) dec <- -dec
    dec
  }
  
  lat <- convert_part(parts[1])
  lon <- convert_part(parts[2])
  
  c(lat, lon)
}

coords <- t(sapply(all_results$gps, convert_gps))

all_results$lat <- coords[, 1]
all_results$lon <- coords[, 2]

readr::write_excel_csv(
  all_results,
  "data/european_tree_of_the_year_results.csv"
)