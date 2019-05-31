library(tidyverse)
library(rvest)

base_url <- "https://en.wikisource.org/wiki/Moral_letters_to_Lucilius/Letter_"

letter_numbers <- seq(1, 124)

letter_urls <- paste0(base_url, letter_numbers)

get_raw_text <- function(base_url, letter_number){
  paste0(base_url, letter_number) %>%
    read_html() %>%
    html_nodes(xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "mw-parser-output", " " ))]') %>%  
    html_text()
}


extract_text <- function(raw_text, letter_number){
  raw_text <- raw_text %>%
    str_split("\n") %>%  
    flatten_chr() %>%  
    discard(~`==`(., ""))

  start <- str_which(raw_text, paste0(as.roman(letter_number), ". On"))

  end <- str_which(raw_text, "Footnotes*")


  raw_text[start:(end-1)] %>%
    str_remove_all("\\[\\d{1,}\\]")
}

get_letter <- function(base_url, letter_number){

  raw_text <- get_raw_text(base_url, letter_number)

  extract_text(raw_text, letter_number)
}
