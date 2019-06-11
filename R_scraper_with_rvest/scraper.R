library(rvest)
library(stringr)


# request a single listing of an apartment
immo <-
  read_html('https://www.immobilienscout24.de/expose/109523308')

# extract the rent with specific html-classes
rent <- immo %>%
  html_nodes(".is24qa-kaltmiete.is24-value.font-semibold") %>%
  html_text()

# perform string operations to transform the string of the rent into usable numbers
rent <- immo %>%
  html_nodes(".is24qa-kaltmiete.is24-value.font-semibold") %>%
  html_text() %>%
  str_remove("€") %>%
  str_trim() %>%
  str_replace("[.]", "") %>%
  str_replace(",", ".") %>%
  as.numeric


# extend the string operations to also work on the square meters
sqm <- immo %>%
  html_nodes('.is24qa-flaeche.is24-value.font-semibold') %>%
  html_text() %>%
  str_remove("€") %>%
  str_replace("[.]", "") %>%
  str_replace("m²", "") %>%
  str_replace(",", ".")  %>%
  str_trim() %>%
  as.numeric

# create a simple function to request urls
scrape_complete_page <- function(url) {
  print(paste("scraping page:", url))
  html_download <- read_html(url)
  return(html_download)
  
}

# create a simple function to extract elements from the web pages
extract_single_element <- function(html_download, html_class) {
  value <- html_download %>%
    html_nodes(html_class) %>%
    html_text() %>%
    str_remove("€") %>%
    str_replace("[.]", "") %>%
    str_replace("m²", "") %>%
    str_replace(",", ".")  %>%
    str_trim() %>%
    as.numeric
  return(value)
}

# simple example to show the usefulness of functions in R
first_listing <-
  scrape_complete_page('https://www.immobilienscout24.de/expose/109523308')


rent <-
  extract_single_element(first_listing, ".is24qa-kaltmiete.is24-value.font-semibold")
rooms <-
  extract_single_element(first_listing, '.is24qa-zi.is24-value.font-semibold')
sqm <-
  extract_single_element(first_listing, '.is24qa-flaeche.is24-value.font-semibold')


# loop through three html-classes and three urls
html_classes <- c(
  ".is24qa-kaltmiete.is24-value.font-semibold",
  '.is24qa-zi.is24-value.font-semibold',
  '.is24qa-flaeche.is24-value.font-semibold'
)

urls <- c(
  'https://www.immobilienscout24.de/expose/109523308',
  'https://www.immobilienscout24.de/expose/108982092',
  'https://www.immobilienscout24.de/expose/110182204'
)

# show what the for loop does
for (url in urls) {
  print(url)
}

# loop through urls and classes, print results
for (url in urls) {
  print(paste("scraping url:", url))
  listing <- scrape_complete_page(url)
  for (html_class in html_classes) {
    value <- extract_single_element(listing, html_class)
    print(value)
  }
}

# create a small function to not use loops (for increased performance)
immo_scraper <- function(urls, html_classes) {
  print(paste("scraping page:", urls))
  html_download <- read_html(urls)
  immo_data <-
    sapply(html_classes, extract_single_element, html_download = html_download)
  return(immo_data)
}

# apply the function, transpose the dataframe
df <- sapply(urls, immo_scraper, html_classes = html_classes)
df <- t(df)

# extract listing urls from an overview webpage
url = 'https://www.immobilienscout24.de/Suche/S-T/P-1/Wohnung-Miete/Umkreissuche/Berlin/-/229459/2511140/-/-/50?enteredFrom=result_list'
site <- read_html(url)
urls <- site %>%
  html_nodes("article") %>%
  html_attr("data-obid") %>%
  paste0("https://www.immobilienscout24.de/expose/", .)

# crawl 20 listings with this simple line of code
df <- sapply(urls, immo_scraper, html_classes = html_classes)
df <- t(df)


# create the urls of the overview webpages with this function
generate_overview_urls <- function(page) {
  url = paste0(
    'https://www.immobilienscout24.de/Suche/S-T/P-',
    page,
    '/Wohnung-Miete/Umkreissuche/Berlin/-/229459/2511140/-/-/50?enteredFrom=result_list'
  )
  return(url)
}

overview_urls <- lapply(1:2, generate_overview_urls)

# function to crawl all data at once
get_data <- function(url, immo_scraper, html_classes){
  random_time <- runif(1, 0.5, 3)
  print(paste("sleeping for", random_time, "seconds"))
  Sys.sleep(random_time)
  site <- read_html(url)
  urls <- site %>%
    html_nodes("article") %>%
    html_attr("data-obid") %>%
    paste0("https://www.immobilienscout24.de/expose/", .)
  
  
  df <- sapply(urls, immo_scraper, html_classes = html_classes)
  df <- t(df)
  
  return(df)
}


full_data <- lapply(overview_urls, get_data, immo_scraper=immo_scraper, html_classes=html_classes)
full_data <- do.call("rbind", full_data)





