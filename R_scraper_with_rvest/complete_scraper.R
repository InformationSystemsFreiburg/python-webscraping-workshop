library(rvest)
library(stringr)

# create a simple function to extract elements from the web pages
extract_single_element <- function(html_download, html_class) {
  value <- html_download %>% # access the saved web page
    html_nodes(html_class) %>% # select the node with the correct HTML class
    html_text() %>% # select the text value
    str_remove("€") %>% # remove the € sign
    str_replace("[.]", "") %>% # repalce . with , for english number format
    str_replace("m²", "") %>% # remove m^2 
    str_replace(",", ".")  %>% # replace , with . for english number format
    str_trim() %>% # remove whitespace before and after
    as.numeric # convert to number
  return(value)
}

# loop through three html-classes and three urls
html_classes <- c(
  ".is24qa-kaltmiete.is24-value.font-semibold", # this is the class for the rent
  '.is24qa-zi.is24-value.font-semibold', # this is the class for the rooms
  '.is24qa-flaeche.is24-value.font-semibold' # this is the class for the area
)


# create a small function to scrape elements from single listing 
immo_scraper <- function(urls, html_classes) {
  print(paste("scraping page:", urls))  # print the url
  html_download <- read_html(urls)  # open the url and save the web page
  immo_data <-
    sapply(html_classes, extract_single_element, html_download = html_download) # use function to download all three HTML elements
  return(immo_data) # return the three saved elements (rent, area, rooms)
}

# create the urls of the overview webpages with this function
generate_overview_urls <- function(page) {
  url = paste0(
    'https://www.immobilienscout24.de/Suche/S-T/P-',
    page,
    '/Wohnung-Miete/Umkreissuche/Berlin/-/229459/2511140/-/-/50?enteredFrom=result_list'
  )
  return(url)
}

# function to crawl all data at once
get_data <- function(url, immo_scraper, html_classes){
  random_time <- runif(1, 0.5, 3) # generate a random amount of seconds between 0.5 and 3
  print(paste("sleeping for", random_time, "seconds")) # print sleeping time
  Sys.sleep(random_time) # sleep for the random time
  site <- read_html(url) # read overview URL
  urls <- site %>% 
    html_nodes("article") %>% # get the expose ID for all listings of the overview
    html_attr("data-obid") %>% # extract the expos ID
    paste0("https://www.immobilienscout24.de/expose/", .)
  
  
  df <- sapply(urls, immo_scraper, html_classes = html_classes) # scrape the data from all listings
  df <- t(df) # transpose the dataframe
  
  return(df)
}

overview_urls <- lapply(1:2, generate_overview_urls)
full_data <- lapply(overview_urls, get_data, immo_scraper=immo_scraper, html_classes=html_classes)
full_data <- do.call("rbind", full_data) # merge the final dataframes in a single dataframe

