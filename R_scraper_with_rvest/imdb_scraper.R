library(rvest)
library(stringr)

# request a single listing of an apartment
movie <-
  read_html('https://www.imdb.com/title/tt0068646/')

movie_title <- movie %>%
  html_nodes(".originalTitle") %>%
  html_text()

movie_rating <-  movie %>%
  html_nodes(".ratingValue") %>%
  html_text() %>%
  str_replace("[\n]", "") %>%
  str_replace("/10", "") %>%
  str_trim()

